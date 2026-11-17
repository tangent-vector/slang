#include "slang-ir-preserve-precise.h"

#include "slang-ir-insts.h"
#include "slang-ir-util.h"
#include "slang-ir.h"

namespace Slang
{

// A declared `precise` policy applies to writes, rather than later uses of a stored value.
// Consider this example:
//
//     float compute(float a, float b, float c)
//     {
//         precise float product = a * b;
//         return product + c;
//     }
//
// AST-to-IR lowering attaches `IRPreciseDecoration` to the variable for `product`. SSA construction
// removes that variable, so we first rewrite its store to use `makePrecise(a * b)`. The marker
// survives storage elimination without making the subsequent addition precise. The later precision
// propagation pass resolves the marker into operation attributes after ordinary specialization.
//
// The same ordering must preserve whole-value writes to types containing precise fields. Their
// fields may not be known until specialization, so those writes receive a conditional demand
// marker. We first identify writes and preserve their values, then define the address and type
// checks used to choose the marker. No inferred policy is attached to a storage declaration by this
// pass.

// Preserve the value operand of a write before its destination can disappear. Function-result
// demands are already explicit markers from AST lowering; only a global-variable initializer's
// `IRReturn` represents a stored value that this pass must preserve.
static void preservePreciseWrite(
    IRBuilder* builder,
    IRInst* inst,
    PreciseFieldContainmentQuery* containment)
{
    IRUse* valueUse = nullptr;
    bool hasDeclaredPrecisePolicy = false;
    switch (inst->getOp())
    {
    case kIROp_Store:
    case kIROp_SwizzledStore:
    case kIROp_MatrixSwizzleStore:
        valueUse = inst->getOperandUse(1);
        hasDeclaredPrecisePolicy = hasDeclaredPreciseStoragePolicy(inst->getOperand(0));
        break;

    case kIROp_Return:
        {
            auto owner = inst->getParent()->getParent();
            // We exclude both generic return values and the function returns introduced by
            // error-handling lowering, which can carry an independently computed error payload.
            if (!as<IRGlobalVar>(owner))
                return;
            SLANG_RELEASE_ASSERT(inst->getOperandCount() == 1);
            valueUse = inst->getOperandUse(0);
            hasDeclaredPrecisePolicy = owner->findDecoration<IRPreciseDecoration>() != nullptr;
        }
        break;

    case kIROp_GlobalConstant:
        if (inst->getOperandCount() == 0)
            return;
        valueUse = inst->getOperandUse(0);
        hasDeclaredPrecisePolicy = inst->findDecoration<IRPreciseDecoration>() != nullptr;
        break;

    default:
        return;
    }

    // We change only this write's input. Other uses still refer to the original value, which also
    // remains the marker's operand; the marker records a demand rather than a new storage policy.
    builder->setInsertBefore(inst);
    IRBuilderSourceLocRAII sourceLoc(builder, inst->sourceLoc);
    valueUse->set(emitValueForPreciseStorage(
        builder,
        valueUse->get(),
        hasDeclaredPrecisePolicy,
        containment));
}

// Visit nested functions and generics as well as module-level initializer blocks. We save the next
// instruction before inserting markers, so each original write is visited once in this traversal.
static void preservePreciseWritesRecursively(
    IRBuilder* builder,
    IRInst* parent,
    PreciseFieldContainmentQuery* containment)
{
    for (auto inst = parent->getFirstChild(); inst;)
    {
        auto next = inst->getNextInst();
        preservePreciseWrite(builder, inst, containment);
        preservePreciseWritesRecursively(builder, inst, containment);
        inst = next;
    }
}

void preservePreciseStorageDemands(IRModule* module)
{
    IRBuilder builder(module);
    PreciseFieldContainmentQuery containment;
    preservePreciseWritesRecursively(&builder, module->getModuleInst(), &containment);
}

bool hasDeclaredPreciseStoragePolicy(IRInst* address)
{
    // An explicit policy on an enclosing storage declaration also applies to its projected fields
    // and elements. We inspect each field key before following its base, because a precise field
    // need not have a precise containing aggregate.
    for (;;)
    {
        // An ordinary pointer parameter is already the pointer value, unlike an `IRVar`, which
        // is the address of the variable holding that value. Only a parameter-direction wrapper
        // identifies referenced parameter storage. A precise local proxy for a mutable input
        // remains an `IRVar`, so assigning a new pointer still constrains its computation.
        if (as<IRGlobalParam>(address))
            return false;
        if (auto parameter = as<IRParam>(address))
        {
            auto type = as<IRType>(parameter->getDataType());
            if (!type)
                return false;
            auto direction = std::get<0>(splitParameterDirectionAndType(type));
            if (direction.kind == ParameterDirectionInfo::In)
                return false;
        }
        if (address->findDecoration<IRPreciseDecoration>())
            return true;
        switch (address->getOp())
        {
        case kIROp_FieldAddress:
            if (address->getOperand(1)->findDecoration<IRPreciseDecoration>())
                return true;
            address = address->getOperand(0);
            break;

        case kIROp_GetElementPtr:
        case kIROp_NodeOutputRecordGetElementPtr:
        case kIROp_GetOffsetPtr:
        case kIROp_BitCast:
        case kIROp_Reinterpret:
        case kIROp_PtrCast:
        case kIROp_MakePrecise:
        case kIROp_MakePreciseIfTypeContainsPrecise:
        case kIROp_ValueWithPreciseDependency:
            address = address->getOperand(0);
            break;

        default:
            // A load of a pointer starts a new storage path: a policy on the variable holding the
            // pointer restricts writes of that pointer value, not writes through the pointer.
            return false;
        }
    }
}

IRInst* emitValueForPreciseStorage(
    IRBuilder* builder,
    IRInst* value,
    bool hasDeclaredPrecisePolicy,
    PreciseFieldContainmentQuery* containment)
{
    // Type, generic, and witness values are canonical semantic representations, not runtime
    // computations. No declaration policy can add a floating-point demand to those values, and
    // wrapping them in an ordinary value instruction would invalidate specialization's operands.
    auto valueType = unwrapAttributedType(value->getDataType());
    if (as<IRTypeKind>(valueType))
        return value;
    if (as<IRGenericKind>(valueType))
        return value;
    if (as<IRWitnessTableTypeBase>(valueType))
        return value;

    // Multiple preparation steps can revisit a write as pointer promotion or early inlining
    // exposes its destination. An existing unconditional demand already covers either policy.
    if (as<IRMakePrecise>(value))
        return value;

    // AST lowering can call this helper while a surrounding structure's field list is still
    // being built. Whole-module callers provide a query after those definitions are complete.
    PreciseFieldContainmentQuery incompleteTypes(PreciseTypeDefinitionState::MayBeIncomplete);
    if (!containment)
        containment = &incompleteTypes;
    if (!hasDeclaredPrecisePolicy)
    {
        if (containment->get(valueType) == PreciseFieldContainment::No)
        {
            if (auto conditionalDemand = as<IRMakePreciseIfTypeContainsPrecise>(value))
                return conditionalDemand->getValue();
            return value;
        }
    }

    if (auto conditionalDemand = as<IRMakePreciseIfTypeContainsPrecise>(value))
    {
        if (!hasDeclaredPrecisePolicy)
            return value;

        // An explicitly precise destination strengthens the existing conditional demand. We
        // replace that demand on this write instead of nesting another marker around it.
        value = conditionalDemand->getValue();
    }

    if (hasDeclaredPrecisePolicy)
        return builder->emitIntrinsicInst(value->getFullType(), kIROp_MakePrecise, 1, &value);

    // Other aggregates and values with unresolved generic parameter types can contain precise
    // fields. We retain that question until the post-specialization type query can inspect them.
    return builder->emitIntrinsicInst(
        value->getFullType(),
        kIROp_MakePreciseIfTypeContainsPrecise,
        1,
        &value);
}

// A whole value contains precise storage if any of its contained values does. A known precise
// field settles that question even when another field's type has not been specialized yet.
static PreciseFieldContainment combineContainment(
    PreciseFieldContainment left,
    PreciseFieldContainment right)
{
    if (left == PreciseFieldContainment::Yes)
        return left;
    if (right == PreciseFieldContainment::Yes)
        return right;
    if (left == PreciseFieldContainment::Unknown)
        return left;
    return right;
}

PreciseFieldContainment PreciseFieldContainmentQuery::get(IRInst* type)
{
    // Type attributes do not introduce field storage. We normalize that wrapper before using
    // the ordinary IR type identity as the memoization key for this phase.
    type = unwrapAttributedType(type);
    if (auto result = m_results.tryGetValue(type))
        return *result;

    // Concrete storage contained by value is acyclic. An unresolved recursive type does not
    // establish either answer, so we keep `Unknown` if a partially built type revisits itself.
    m_results.add(type, PreciseFieldContainment::Unknown);
    auto result = calculate(type);
    m_results[type] = result;
    return result;
}

PreciseFieldContainment PreciseFieldContainmentQuery::calculate(IRInst* type)
{
    // We first follow storage explicitly contained by value: declared fields, array elements,
    // and native aggregate payloads. We then recognize types whose values expose no field
    // storage. Any remaining type-producing form stays unresolved instead of proving absence.
    if (auto structType = as<IRStructType>(type))
    {
        auto result = m_definitionState == PreciseTypeDefinitionState::Complete
                          ? PreciseFieldContainment::No
                          : PreciseFieldContainment::Unknown;
        for (auto field : structType->getFields())
        {
            if (field->getKey()->findDecoration<IRPreciseDecoration>())
                return PreciseFieldContainment::Yes;
            result = combineContainment(result, get(field->getFieldType()));
        }
        return result;
    }
    if (auto arrayType = as<IRArrayTypeBase>(type))
        return get(arrayType->getElementType());
    if (as<IRDifferentialPairTypeBase>(type))
        return getDifferentialPairContainment(type);
    if (auto boundInterface = as<IRBoundInterfaceType>(type))
        return get(boundInterface->getConcreteType());

    switch (type->getOp())
    {
    case kIROp_TupleType:
    case kIROp_TypePack:
    case kIROp_TargetTupleType:
        {
            auto result = PreciseFieldContainment::No;
            for (UInt i = 0; i < type->getOperandCount(); ++i)
                result = combineContainment(result, get(type->getOperand(i)));
            return result;
        }
    case kIROp_OptionalType:
    case kIROp_AtomicType:
    case kIROp_EnumType:
    case kIROp_HLSLInputPatchType:
    case kIROp_HLSLOutputPatchType:
        return get(type->getOperand(0));
    case kIROp_ConditionalType:
        // A settled false condition removes the payload storage. An unresolved condition may
        // retain the payload, so its type still determines whether precise fields can occur.
        if (auto condition = as<IRBoolLit>(type->getOperand(1)))
        {
            if (!condition->getValue())
                return PreciseFieldContainment::No;
        }
        return get(type->getOperand(0));
    case kIROp_ResultType:
        return combineContainment(get(type->getOperand(0)), get(type->getOperand(1)));
    default:
        break;
    }

    // These values have no instance field storage. In particular, a texture or pointer does
    // not contain its element type by value, and an interface value does not expose fields of
    // its hidden concrete type. We do not inspect generic argument positions to infer fields.
    if (as<IRBasicType>(type))
        return PreciseFieldContainment::No;
    if (as<IRStringTypeBase>(type))
        return PreciseFieldContainment::No;
    if (as<IRPackedFloatType>(type))
        return PreciseFieldContainment::No;
    if (isPtrLikeOrHandleType(type))
        return PreciseFieldContainment::No;
    if (as<IRResourceTypeBase>(type))
        return PreciseFieldContainment::No;
    if (as<IRUntypedBufferResourceType>(type))
        return PreciseFieldContainment::No;
    if (as<IRSamplerStateTypeBase>(type))
        return PreciseFieldContainment::No;
    if (as<IRWorkGraphRecordTypeBase>(type))
        return PreciseFieldContainment::No;

    switch (type->getOp())
    {
    case kIROp_TupleNameType:
        // A named tuple's final type operand supplies the generated structure's name. Tuple
        // lowering excludes that sentinel from its fields, and its layout has zero size.
        return PreciseFieldContainment::No;

    // Builtin arithmetic composites store scalar elements rather than declared fields. The
    // remaining cases represent functions, references, or opaque runtime values; their type
    // operands describe referents or metadata rather than contained instance storage.
    case kIROp_VectorType:
    case kIROp_MatrixType:
    case kIROp_MetalPackedVectorType:
    case kIROp_CoopVectorType:
    case kIROp_CoopMatrixType:
    case kIROp_FuncType:
    case kIROp_ClassType:
    case kIROp_InterfaceType:
    case kIROp_AnyValueType:
    case kIROp_DynamicType:
    case kIROp_NativePtrType:
    case kIROp_DescriptorHandleType:
    case kIROp_UntypedResourceHandleType:
    case kIROp_UntypedSamplerHandleType:
    case kIROp_TensorViewType:
    case kIROp_TorchTensorType:
    case kIROp_ArrayListType:
    case kIROp_DynamicResourceType:
        return PreciseFieldContainment::No;
    default:
        // Generic parameters, associated types, and unresolved type-producing instructions can
        // become structures after specialization. We cannot prove absence of precise fields.
        return PreciseFieldContainment::Unknown;
    }
}

PreciseFieldContainment PreciseFieldContainmentQuery::getDifferentialPairContainment(IRInst* type)
{
    // A differential pair contains both its primal and differential values. A precise primal
    // field proves containment immediately. Otherwise we need the differential type from the
    // conformance witness; its declaration can differ from the primal type's declaration.
    // We inspect the canonical builtin requirement used by autodiff without constructing lookup
    // instructions. An unresolved witness cannot prove absence of precise differential fields.
    auto pairType = cast<IRDifferentialPairTypeBase>(type);
    auto primal = get(pairType->getValueType());
    if (primal == PreciseFieldContainment::Yes)
        return primal;

    auto witness = as<IRWitnessTable>(pairType->getWitness());
    if (!witness)
        return PreciseFieldContainment::Unknown;
    auto differentialKind = as<IRDifferentialPairType>(type)
                                ? BuiltinRequirementKind::DifferentialType
                                : BuiltinRequirementKind::DifferentialPtrType;
    for (auto entry : witness->getEntries())
    {
        auto key = as<IRBuiltinRequirementKey>(entry->getRequirementKey());
        if (!key)
            continue;
        if (key->getKind() != IRIntegerValue(differentialKind))
            continue;
        return combineContainment(primal, get(entry->getSatisfyingVal()));
    }
    return PreciseFieldContainment::Unknown;
}
} // namespace Slang
