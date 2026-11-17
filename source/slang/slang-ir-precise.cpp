#include "slang-ir-precise.h"

#include "slang-ir-insts.h"
#include "slang-ir-preserve-precise.h"
#include "slang-ir-util.h"

namespace Slang
{
namespace
{
// A precision demand applies to the computation of a value, rather than every later use of
// that value. Consider this example:
//
//     float product = a * b;
//     precise float sum = product + c;
//     return sum * d;
//
// `preservePreciseStorageDemands` inserts `makePrecise` on the value written to `sum` before
// SSA construction removes its storage. We follow that marker backward to the addition and
// the multiplication computing `product`. The return expression consumes the demanded value,
// so its multiplication receives no demand.
//
// We identify demands from declarations and preserved writes, solve value and memory
// dependencies separately within each code body, and attach `IRPreciseAttr` to the affected
// arithmetic. Module constant expressions use the same solver in their own scope. Separating
// those dependencies matters for pointers: computing an address can depend on index arithmetic
// without reading the addressed contents.
//
// Calls contribute dependencies from their input/output modes without exposing their bodies.
// Mandatory early inlining is an intentional exception: its expanded bodies are part of the
// caller when this analysis runs. Later inlining copies already-resolved operation attributes.

/// The caller-side value accesses permitted by one parameter's passing mode.
struct PreciseCallArgumentAccess
{
    /// Whether a demanded call result or output constrains the incoming argument computation.
    bool readsValue = true;
    /// Whether the parameter mode permits writing the caller's argument storage.
    bool writesValue = false;
    /// Whether the incoming value is read from referenced storage, rather than being the address.
    bool readsThroughReference = false;
};

// Passing modes remain inspectable when the value type is generic. We use the existing mode
// decomposition for type instructions and its ordinary-input case for a bare type
// parameter, without requiring that parameter's concrete instantiation.
PreciseCallArgumentAccess getCallArgumentAccess(IRCall* call, UInt argumentIndex)
{
    PreciseCallArgumentAccess access;
    auto parameterType =
        findCallArgumentParameterType(call, call->getOperandUse(argumentIndex + 1));
    SLANG_RELEASE_ASSERT(parameterType);
    auto directionType = as<IRType>(parameterType);
    if (!directionType)
        return access;

    // A direction wrapper is an `IRType` even when its contained value type is a generic
    // parameter. This cast recognizes the wrapper representation, not a fully concrete type.
    auto direction = std::get<0>(splitParameterDirectionAndType(directionType));
    switch (direction.kind)
    {
    case ParameterDirectionInfo::In:
        break;
    case ParameterDirectionInfo::BorrowIn:
        access.readsThroughReference = true;
        break;
    case ParameterDirectionInfo::Out:
        access.readsValue = false;
        access.writesValue = true;
        break;
    case ParameterDirectionInfo::BorrowInOut:
        access.readsThroughReference = true;
        access.writesValue = true;
        break;
    case ParameterDirectionInfo::Ref:
        access.readsValue = direction.accessQualifier != AccessQualifier::WriteOnly;
        access.writesValue = direction.accessQualifier == AccessQualifier::ReadWrite;
        if (direction.accessQualifier == AccessQualifier::WriteOnly)
            access.writesValue = true;
        access.readsThroughReference = access.readsValue;
        break;
    }
    return access;
}

// Integer operations and casts transmit demands, but only floating-point arithmetic acquires
// the restriction on contraction and reassociation.
bool isFloatingPointArithmetic(IRInst* inst)
{
    switch (inst->getOp())
    {
    case kIROp_Add:
    case kIROp_Sub:
    case kIROp_Mul:
    case kIROp_Div:
    case kIROp_FRem:
    case kIROp_Neg:
        break;
    default:
        return false;
    }
    auto type = as<IRType>(unwrapAttributedType(inst->getDataType()));
    if (auto matrixType = as<IRMatrixType>(type))
        type = matrixType->getElementType();
    type = getVectorOrCoopMatrixElementType(type);
    if (isFloatingType(type))
        return true;
    return isPackedFloatType(type);
}

/// A location in resource contents, separate from the storage holding the resource descriptor.
///
/// The resource and index are existing IR values. Element indices and texel coordinates can
/// prove accesses disjoint; byte offsets alone cannot, because accesses can span several bytes.
/// A null index means no particular element is known, including when a pointer projection can
/// leave the element selected by the original resource access.
struct PreciseResourceLocation
{
    enum class IndexKind
    {
        Element,
        Texel,
        ByteOffset,
    };

    IRInst* resource = nullptr;
    IRInst* index = nullptr;
    IndexKind indexKind = IndexKind::Element;
};

// Remove only the precision identities, which preserve the exact value and type of operand zero.
// Casts of pointers or resource descriptors remain because their view or element stride can differ.
IRInst* unwrapPreciseValueIdentities(IRInst* value)
{
    for (;;)
    {
        switch (value->getOp())
        {
        case kIROp_MakePrecise:
        case kIROp_MakePreciseIfTypeContainsPrecise:
        case kIROp_ValueWithPreciseDependency:
            value = value->getOperand(0);
            break;
        default:
            return value;
        }
    }
}

/// One original shader binding represented by a field of a compiler-collected parameter group.
struct PreciseCollectedResourceBinding
{
    IRGlobalParam* group = nullptr;
    IRStructKey* key = nullptr;
};

// Identify a resource declaration after shader-parameter collection has replaced it by a field.
// `collectGlobalUniformParameters` creates a struct with `IRSynthesizedParameterGroupDecoration`
// and one key per original parameter, then replaces its uses by
// `load(fieldAddress(globalParam, key))` or a direct field extraction. The direct global parameter
// and marked type prove that the key still identifies an independent binding. A load from
// arbitrary mutable descriptor storage provides no such proof.
bool tryGetCollectedResourceBinding(IRInst* resource, PreciseCollectedResourceBinding& binding)
{
    IRInst* groupValue = nullptr;
    IRInst* field = nullptr;
    if (auto load = as<IRLoad>(resource))
    {
        auto fieldAddress = as<IRFieldAddress>(load->getPtr());
        if (!fieldAddress)
            return false;
        groupValue = fieldAddress->getBase();
        field = fieldAddress->getField();
    }
    else if (auto extract = as<IRFieldExtract>(resource))
    {
        groupValue = extract->getBase();
        field = extract->getField();
    }
    else
        return false;

    auto group = as<IRGlobalParam>(groupValue);
    if (!group)
        return false;
    auto type = unwrapAttributedType(group->getDataType());
    if (auto parameterGroupType = as<IRParameterGroupType>(type))
        type = unwrapAttributedType(parameterGroupType->getElementType());
    auto structType = as<IRStructType>(type);
    if (!structType)
        return false;
    if (!structType->findDecoration<IRSynthesizedParameterGroupDecoration>())
        return false;
    auto key = as<IRStructKey>(field);
    if (!key)
        return false;
    binding = {group, key};
    return true;
}

/// A native resource access and its written value or separate status output, when present.
struct PreciseResourceAccess
{
    /// The instruction whose result and input expressions participate in demand propagation.
    IRInst* instruction = nullptr;
    PreciseResourceLocation location;
    /// The value written into resource contents; null for a read.
    IRInst* writtenValue = nullptr;
    /// The output-only address written by a status-returning load; null for other accesses.
    IRInst* statusAddress = nullptr;
};

// Describe the memory accessed by a native resource instruction. These operations have already
// replaced builtin calls; inspecting their operands does not expose any callee's implementation.
bool tryGetNativeResourceAccess(IRInst* inst, PreciseResourceAccess& access)
{
    access = {};
    access.instruction = inst;
    using IndexKind = PreciseResourceLocation::IndexKind;
    switch (inst->getOp())
    {
    case kIROp_StructuredBufferLoad:
    case kIROp_RWStructuredBufferLoad:
        access.location.indexKind = IndexKind::Element;
        break;
    case kIROp_StructuredBufferLoadStatus:
    case kIROp_RWStructuredBufferLoadStatus:
        access.location.indexKind = IndexKind::Element;
        access.statusAddress = inst->getOperand(2);
        break;
    case kIROp_ByteAddressBufferLoad:
        access.location.indexKind = IndexKind::ByteOffset;
        break;
    case kIROp_ImageLoad:
        access.location.indexKind = IndexKind::Texel;
        break;
    case kIROp_RWStructuredBufferStore:
        access.location.indexKind = IndexKind::Element;
        access.writtenValue = inst->getOperand(2);
        break;
    case kIROp_ImageStore:
        access.location.indexKind = IndexKind::Texel;
        access.writtenValue = inst->getOperand(2);
        break;
    case kIROp_ByteAddressBufferStore:
        access.location.indexKind = IndexKind::ByteOffset;
        // `emitByteAddressBufferStore` and the builtin intrinsic place alignment before the
        // written value: `byteAddressBufferStore(buffer, offset, alignment, value)`.
        access.writtenValue = inst->getOperand(3);
        break;
    default:
        return false;
    }
    access.location.resource = inst->getOperand(0);
    access.location.index = inst->getOperand(1);
    return true;
}

// Identify resource contents reached through an ordinary pointer access. A struct field remains
// within its containing resource element. Pointer offsets, indexing, and casts conservatively end
// that element-index proof.
// Consider `float* p = &buffer[0]; precise float value = p[1];`: the final address can read element
// one, so the original zero index cannot prove that a store to element one is independent. We
// retain the resource but discard that index proof unless the projections preserve it.
bool tryGetResourceLocationFromAddress(IRInst* address, PreciseResourceLocation& location)
{
    location = {};
    bool preservesElementIndex = true;
    using IndexKind = PreciseResourceLocation::IndexKind;
    for (;;)
    {
        address = unwrapPreciseValueIdentities(address);
        switch (address->getOp())
        {
        case kIROp_FieldAddress:
            address = address->getOperand(0);
            continue;
        case kIROp_GetElementPtr:
        case kIROp_GetOffsetPtr:
        case kIROp_BitCast:
        case kIROp_Reinterpret:
        case kIROp_PtrCast:
            preservesElementIndex = false;
            address = address->getOperand(0);
            continue;
        case kIROp_RWStructuredBufferGetElementPtr:
            location.indexKind = IndexKind::Element;
            if (preservesElementIndex)
                location.index = address->getOperand(1);
            break;
        case kIROp_ImageSubscript:
        case kIROp_ImageTexelPointer:
            location.indexKind = IndexKind::Texel;
            if (preservesElementIndex)
                location.index = address->getOperand(1);
            break;
        case kIROp_GetStructuredBufferPtr:
            location.indexKind = IndexKind::Element;
            break;
        case kIROp_GetUntypedBufferPtr:
            location.indexKind = IndexKind::ByteOffset;
            break;
        default:
            return false;
        }
        location.resource = address->getOperand(0);
        return true;
    }
}

// Prove that two element indices or texel coordinates differ using their integer literals.
// Unrecognized expressions provide no proof. For a vector coordinate, one unequal component
// suffices; we do not need a new equivalence relation over arbitrary IR expressions.
bool areResourceIndicesKnownDifferent(IRInst* first, IRInst* second)
{
    if (auto firstLiteral = as<IRIntLit>(first))
    {
        if (auto secondLiteral = as<IRIntLit>(second))
            return firstLiteral->getValue() != secondLiteral->getValue();
        return false;
    }
    auto firstVector = as<IRMakeVector>(first);
    if (!firstVector)
        return false;
    auto secondVector = as<IRMakeVector>(second);
    if (!secondVector)
        return false;
    if (firstVector->getOperandCount() != secondVector->getOperandCount())
        return false;
    for (UInt i = 0; i < firstVector->getOperandCount(); ++i)
    {
        if (areResourceIndicesKnownDifferent(
                firstVector->getOperand(i),
                secondVector->getOperand(i)))
            return true;
    }
    return false;
}

// Find the nearest code owner, or the module for a computation outside a body. This exact-owner
// query keeps module-constant propagation from descending into any function or generic body.
IRInst* getPreciseDemandOwner(IRInst* value)
{
    for (auto parent = value->getParent(); parent; parent = parent->getParent())
    {
        if (as<IRGlobalValueWithCode>(parent))
            return parent;
        if (as<IRModuleInst>(parent))
            return parent;
    }
    SLANG_UNEXPECTED("A precision dependency must belong to a module or a code body.");
}

/// The pending demands within one code body or the module's constant expressions.
///
/// We select the permitted flow-insensitive policy: a demanded location constrains every write
/// to that location in the function, including later writes and writes that cannot reach the
/// demanded read. Distinct storage declarations are independent roots for this policy even when
/// their addresses could alias at runtime; computed pointers still require conservative aliasing.
/// Consider this example:
///
///     float example(__ref float a, __ref float b, float x, float y)
///     {
///         precise float result = a;
///         a = x * y;
///         b = x + y;
///         return result;
///     }
///
/// The write to `a` receives a demand even though it follows the read, while the separately
/// declared `b` does not. This is an inferred policy for the location, not reaching-write analysis.
///
/// Each instruction enters each work list at most once. Memory demands can demand calls,
/// whose incoming reference reads can introduce more memory demands. We solve both finite
/// work lists together. Inferred storage policies never modify declaration metadata.
struct PreciseDemands
{
    IRModule* module;
    IRInst* scopeOwner;
    PreciseFieldContainmentQuery* containment;
    List<IRInst*> instructions;
    List<IRInst*> writes;
    List<PreciseResourceAccess> nativeResourceWrites;
    List<PreciseResourceAccess> nativeStatusWrites;
    List<IRCall*> calls;
    List<IRInst*> declaredPreciseStorage;
    HashSet<IRInst*> demandedValues;
    List<IRInst*> valueWorkList;
    HashSet<IRInst*> demandedStorage;
    List<IRInst*> storageWorkList;

    PreciseDemands(
        IRModule* inModule,
        IRInst* inScopeOwner,
        PreciseFieldContainmentQuery* inContainment,
        const List<IRInst*>& declaredGlobals)
        : module(inModule)
        , scopeOwner(inScopeOwner)
        , containment(inContainment)
        , declaredPreciseStorage(declaredGlobals)
    {
    }

    // We collect writes, seed declared and preserved demands, and follow dependencies until
    // neither kind of work remains. We rewrite arithmetic only after propagation, keeping
    // instructions stable throughout the work-list traversal.
    void resolve()
    {
        collectScopeInstructions();
        collectDeclaredPreciseStorage();
        collectWritesAndSeedDemands();
        Index nextValue = 0;
        Index nextStorage = 0;
        for (;;)
        {
            while (nextValue < valueWorkList.getCount())
                propagateValueDemand(valueWorkList[nextValue++]);
            if (nextStorage == storageWorkList.getCount())
                break;
            propagateStorageDemand(storageWorkList[nextStorage++]);
        }
        attributeArithmetic();
    }

    // A code body contributes its block instructions. The module contributes only its immediate
    // value instructions; nested bodies are collected and analyzed as separate owners.
    void collectScopeInstructions()
    {
        if (auto code = as<IRGlobalValueWithCode>(scopeOwner))
        {
            for (auto block : code->getBlocks())
                for (auto inst : block->getChildren())
                    instructions.add(inst);
            return;
        }
        SLANG_RELEASE_ASSERT(as<IRModuleInst>(scopeOwner));
        for (auto inst : scopeOwner->getChildren())
        {
            if (!as<IRGlobalValueWithCode>(inst))
                instructions.add(inst);
        }
    }

    // A value computed by another owner is not exposed to local propagation. In particular,
    // a function's read of a global cannot constrain a separate initializer or another function.
    void demandValue(IRInst* value)
    {
        if (as<IRGlobalValueWithCode>(value))
            return;
        if (getPreciseDemandOwner(value) != scopeOwner)
            return;
        if (demandedValues.add(value))
            valueWorkList.add(value);
    }

    // Keep the original address until resource matching has inspected its projections. Ordinary
    // storage matching still groups aggregate roots, but peeling an offset here would lose the
    // information that a pointer can leave its original resource element.
    void demandStorage(IRInst* address)
    {
        if (demandedStorage.add(address))
            storageWorkList.add(address);
    }

    // A precise declaration restricts every surviving write. Legacy precise fields also
    // restrict whole-value writes, including writes through a call's output arguments.
    bool hasDeclaredWriteDemand(IRInst* address)
    {
        if (hasDeclaredPreciseStoragePolicy(address))
            return true;
        auto pointerType = as<IRPtrTypeBase>(unwrapAttributedType(address->getDataType()));
        if (pointerType)
        {
            if (containment->get(pointerType->getOperand(0)) == PreciseFieldContainment::Yes)
                return true;
        }

        // Direct accesses to distinct declarations remain independent. A computed pointer can
        // select precise storage, so its writes must account for the declared policy of every
        // location that pointer might refer to.
        if (isDeclarationRoot(peelAddressForwardingOps(address)))
            return false;
        for (auto declaredAddress : declaredPreciseStorage)
        {
            if (canShareStorageDemand(address, declaredAddress))
                return true;
        }
        return false;
    }

    // We retain declared policies separately from inferred reads. This inventory is needed when
    // a pointer-valued phi or selection obscures a write's otherwise directly named destination.
    void collectDeclaredPreciseStorage()
    {
        for (auto inst : instructions)
        {
            if (!as<IRPtrTypeBase>(unwrapAttributedType(inst->getDataType())))
                continue;
            if (hasDeclaredPreciseStoragePolicy(inst))
                declaredPreciseStorage.add(inst);
        }
    }

    // Store forms name their destination first and their written value second. An atomic update
    // also writes its first operand and reads its previous contents, so its inputs include storage
    // as well as explicit values. Swizzled writes share the aggregate's all-writes inventory.
    void collectWritesAndSeedDemands()
    {
        for (auto inst : instructions)
        {
            if (tryCollectNativeResourceAccess(inst))
                continue;
            if (auto atomic = as<IRAtomicOperation>(inst))
            {
                if (as<IRAtomicLoad>(atomic))
                    continue;
                writes.add(atomic);
                if (hasDeclaredWriteDemand(atomic->getPtr()))
                    demandWriteInputs(atomic);
                continue;
            }
            switch (inst->getOp())
            {
            case kIROp_MakePrecise:
                demandValue(inst->getOperand(0));
                break;
            case kIROp_MakePreciseIfTypeContainsPrecise:
                if (containment->get(inst->getDataType()) == PreciseFieldContainment::Yes)
                    demandValue(inst->getOperand(0));
                break;
            case kIROp_Store:
            case kIROp_SwizzledStore:
            case kIROp_MatrixSwizzleStore:
                writes.add(inst);
                if (hasDeclaredWriteDemand(inst->getOperand(0)))
                    demandWriteInputs(inst);
                break;
            case kIROp_Call:
                {
                    auto call = cast<IRCall>(inst);
                    calls.add(call);
                    seedCallOutputDemands(call);
                    break;
                }
            default:
                break;
            }
        }
    }

    // An ordinary or atomic store supplies its explicit value without reading its destination.
    // Every other writing atomic operation also reads its destination. Demanding that instruction
    // follows both its incoming contents and its evaluated value operands, including
    // the comparison and desired values of `AtomicCompareExchange`.
    void demandWriteInputs(IRInst* write)
    {
        if (as<IRAtomicOperation>(write))
        {
            if (!as<IRAtomicStore>(write))
            {
                demandValue(write);
                return;
            }
        }
        demandValue(write->getOperand(1));
    }

    // Native stores name resource contents directly instead of using an address instruction.
    // A precise field in the stored aggregate applies to that write, but a qualifier on the
    // descriptor does not qualify its resource contents. Status-returning loads also write an
    // ordinary output address, whose declared demand can constrain the load's input expressions.
    bool tryCollectNativeResourceAccess(IRInst* inst)
    {
        PreciseResourceAccess access;
        if (!tryGetNativeResourceAccess(inst, access))
            return false;
        if (access.writtenValue)
        {
            nativeResourceWrites.add(access);
            if (containment->get(access.writtenValue->getDataType()) ==
                PreciseFieldContainment::Yes)
                demandValue(access.writtenValue);
        }
        if (access.statusAddress)
        {
            nativeStatusWrites.add(access);
            if (hasDeclaredWriteDemand(access.statusAddress))
                demandValue(inst);
        }
        return true;
    }

    // A void call can have a demand through an output argument. One demanded output is enough
    // to constrain every input, independent of the computations in the callee.
    void seedCallOutputDemands(IRCall* call)
    {
        for (UInt i = 0; i < call->getArgCount(); ++i)
        {
            if (!getCallArgumentAccess(call, i).writesValue)
                continue;
            if (hasDeclaredWriteDemand(call->getArg(i)))
            {
                demandValue(call);
                return;
            }
        }
    }

    // Parameters and declared variables are distinct roots for policy inference even when
    // they might alias at runtime. A computed pointer can obscure that identity and requires
    // a conservative query for the writes that may contribute to its contents.
    bool isDeclarationRoot(IRInst* address)
    {
        if (as<IRVar>(address))
            return true;
        if (as<IRGlobalVar>(address))
            return true;
        if (as<IRGlobalParam>(address))
            return true;
        if (auto parameter = as<IRParam>(address))
        {
            if (auto code = as<IRGlobalValueWithCode>(scopeOwner))
                return parameter->getParent() == code->getFirstBlock();
        }
        return false;
    }

    // We intentionally group access chains under the same root. The existing alias query
    // handles ambiguous pointers without changing the independence of known declarations.
    bool canShareStorageDemand(IRInst* first, IRInst* second)
    {
        first = peelAddressForwardingOps(first);
        second = peelAddressForwardingOps(second);
        if (first == second)
            return true;
        if (isDeclarationRoot(first))
        {
            if (isDeclarationRoot(second))
                return false;
        }
        if (auto code = as<IRGlobalValueWithCode>(scopeOwner))
            return canAddressesPotentiallyAlias(code, first, second);

        // Module constant expressions have no function-entry parameters. Any remaining
        // computed addresses are conservatively related after separating known declarations.
        return true;
    }

    // Descriptor value identity or a proven collected binding identifies the same resource view.
    // Distinct known bindings remain independent under the declaration policy. Other descriptor
    // computations can produce aliasing views, so their indices alone cannot prove disjointness.
    // For a known shared view, literal element/texel indices can prove separate locations. Byte
    // accesses remain conservative because their widths can depend on layout.
    bool canShareResourceDemand(
        const PreciseResourceLocation& first,
        const PreciseResourceLocation& second)
    {
        auto firstResource = unwrapPreciseValueIdentities(first.resource);
        auto secondResource = unwrapPreciseValueIdentities(second.resource);
        if (firstResource != secondResource)
        {
            PreciseCollectedResourceBinding firstBinding;
            PreciseCollectedResourceBinding secondBinding;
            bool hasFirstBinding = tryGetCollectedResourceBinding(firstResource, firstBinding);
            bool hasSecondBinding = tryGetCollectedResourceBinding(secondResource, secondBinding);
            if (hasFirstBinding && hasSecondBinding)
            {
                // Collection creates separate loads/field-address instructions at different
                // uses. The group and declaration key identify a binding across those uses;
                // address-instruction identity and generic storage aliasing do not.
                if (firstBinding.group != secondBinding.group ||
                    firstBinding.key != secondBinding.key)
                    return false;
            }
            else
            {
                if (isDeclarationRoot(firstResource) && isDeclarationRoot(secondResource))
                    return false;

                // Mutable descriptor variables can hold the same resource despite having
                // separate storage. Without binding provenance, their loaded values remain
                // conservative even when an address alias query could separate the variables.
                return true;
            }
        }
        if (first.indexKind != second.indexKind)
            return true;
        if (first.indexKind == PreciseResourceLocation::IndexKind::ByteOffset)
            return true;
        return !areResourceIndicesKnownDifferent(first.index, second.index);
    }

    // Compare resource contents with an ordinary pointer access. Recognized resource projections
    // retain their index precision. Opaque computed pointers conservatively retain a possible
    // contents dependency, while known ordinary declaration storage remains separate.
    bool canShareResourceDemandWithAddress(const PreciseResourceLocation& location, IRInst* address)
    {
        PreciseResourceLocation addressLocation;
        if (tryGetResourceLocationFromAddress(address, addressLocation))
            return canShareResourceDemand(location, addressLocation);

        // Ordinary declaration storage is independent of resource contents. In particular,
        // a descriptor value loaded from a CPU parameter aggregate is not an address that can
        // alias an unrelated local variable. A remaining computed pointer can obscure a resource
        // projection, so it conservatively retains a possible contents dependency.
        return !isDeclarationRoot(peelAddressForwardingOps(address));
    }

    // We apply the all-writes policy to resource contents through both native and pointer-based
    // stores, as well as output arguments. All matching writes in this body receive the
    // demand, whether or not a write can reach the particular read that introduced it.
    void propagateResourceDemand(const PreciseResourceLocation& location)
    {
        for (auto access : nativeResourceWrites)
        {
            if (canShareResourceDemand(location, access.location))
                demandValue(access.writtenValue);
        }
        for (auto write : writes)
        {
            if (canShareResourceDemandWithAddress(location, write->getOperand(0)))
                demandWriteInputs(write);
        }
        for (auto access : nativeStatusWrites)
        {
            if (canShareResourceDemandWithAddress(location, access.statusAddress))
                demandValue(access.instruction);
        }
        for (auto call : calls)
        {
            for (UInt i = 0; i < call->getArgCount(); ++i)
            {
                if (!getCallArgumentAccess(call, i).writesValue)
                    continue;
                if (canShareResourceDemandWithAddress(location, call->getArg(i)))
                {
                    demandValue(call);
                    break;
                }
            }
        }
    }

    // Read-inferred demands may reach every write to the grouped location. Only writes in this
    // scope are considered; a code body can include writes to a global declaration.
    void propagateStorageDemand(IRInst* address)
    {
        PreciseResourceLocation resourceLocation;
        if (tryGetResourceLocationFromAddress(address, resourceLocation))
        {
            propagateResourceDemand(resourceLocation);
            return;
        }
        for (auto write : writes)
        {
            if (canShareStorageDemand(address, write->getOperand(0)))
                demandWriteInputs(write);
        }
        // An ambiguous ordinary pointer may also refer to contents written by a native store.
        // The resource/address matcher keeps ordinary declarations outside that contents domain.
        for (auto access : nativeResourceWrites)
        {
            if (canShareResourceDemandWithAddress(access.location, address))
                demandValue(access.writtenValue);
        }
        for (auto access : nativeStatusWrites)
        {
            if (canShareStorageDemand(address, access.statusAddress))
                demandValue(access.instruction);
        }
        for (auto call : calls)
        {
            for (UInt i = 0; i < call->getArgCount(); ++i)
            {
                if (!getCallArgumentAccess(call, i).writesValue)
                    continue;
                if (canShareStorageDemand(address, call->getArg(i)))
                {
                    demandValue(call);
                    break;
                }
            }
        }
    }

    // Reference inputs conceptually read the incoming stored value. An output-only argument
    // supplies neither an incoming read nor a demand on its destination-index arithmetic.
    // Callee-side precise declarations do not change these caller-side rules.
    void propagateCallDemand(IRCall* call)
    {
        for (UInt i = 0; i < call->getArgCount(); ++i)
        {
            auto access = getCallArgumentAccess(call, i);
            if (!access.readsValue)
                continue;
            auto argument = call->getArg(i);
            demandValue(argument);
            if (access.readsThroughReference)
                demandStorage(argument);
        }
    }

    // Most value instructions depend on their evaluated operands. Loads and calls also read
    // storage; block parameters receive values through predecessor branch arguments.
    // `IRSelect` excludes its condition. Lowering records eager `select` and logical-expression
    // dependencies explicitly with `IRValueWithPreciseDependency` instead of control dependence.
    void propagateValueDemand(IRInst* value)
    {
        if (doesInstOnlyDependOnOperandTypes(value))
            return;
        if (auto parameter = as<IRParam>(value))
        {
            for (auto argument : getPhiArgs(parameter))
                demandValue(argument);
            return;
        }
        if (auto call = as<IRCall>(value))
        {
            propagateCallDemand(call);
            return;
        }
        if (auto load = as<IRLoad>(value))
        {
            demandValue(load->getPtr());
            demandStorage(load->getPtr());
            return;
        }
        if (auto atomic = as<IRAtomicOperation>(value))
        {
            // Atomic loads and updates read their addressed contents. Atomic stores have no
            // incoming contents dependency; `demandWriteInputs` handles their written value.
            if (!as<IRAtomicStore>(atomic))
                demandStorage(atomic->getPtr());
        }
        PreciseResourceAccess resourceAccess;
        if (tryGetNativeResourceAccess(value, resourceAccess))
        {
            propagateResourceDemand(resourceAccess.location);
            // A status output does not read its previous value or demand computation of its
            // destination address. Its resource and index are the two incoming value operands.
            if (resourceAccess.statusAddress)
            {
                demandValue(value->getOperand(0));
                demandValue(value->getOperand(1));
                return;
            }
        }
        UInt firstOperand = value->getOp() == kIROp_Select ? 1 : 0;
        for (UInt i = firstOperand; i < value->getOrdinaryOperandCount(); ++i)
            demandValue(value->getOperand(i));
    }

    // Rebuilding arithmetic with an attribute includes its policy in the instruction's CSE
    // key. A decoration would allow an ordinary equivalent to replace the precise operation.
    void attributeArithmetic()
    {
        IRBuilder builder(module);
        for (auto value : valueWorkList)
        {
            if (!isFloatingPointArithmetic(value))
                continue;
            if (value->findAttr<IRPreciseAttr>())
                continue;
            List<IRInst*> operands;
            for (UInt i = 0; i < value->getOperandCount(); ++i)
                operands.add(value->getOperand(i));
            operands.add(builder.getAttr(kIROp_PreciseAttr));
            builder.setInsertBefore(value);
            auto replacement = builder.emitIntrinsicInst(
                value->getFullType(),
                value->getOp(),
                operands.getCount(),
                operands.getBuffer());
            replacement->sourceLoc = value->sourceLoc;
            value->transferDecorationsTo(replacement);
            value->replaceUsesWith(replacement);
            value->removeAndDeallocate();
        }
    }
};

// Collect the analysis owners, preserved value demands, and declaration policies in the module.
//
// We include the module's constant expressions as well as bodies nested in generics. Consider:
//
//     static const precise float4 product = float4(1, 2, 3, 4) * float4(5, 6, 7, 8);
//
// Scalar constant propagation need not fold this vector multiplication. Lowering records its
// precision demand at module scope, so that owner needs the same value analysis as a body.
void collectPreciseAnalysisInputs(
    IRInst* parent,
    List<IRInst*>& scopeOwners,
    List<IRInst*>& markers,
    List<IRPreciseDecoration*>& declaredPolicies)
{
    for (auto decoration : parent->getDecorations())
    {
        if (auto precise = as<IRPreciseDecoration>(decoration))
            declaredPolicies.add(precise);
    }
    if (as<IRModuleInst>(parent))
        scopeOwners.add(parent);
    else if (as<IRGlobalValueWithCode>(parent))
        scopeOwners.add(parent);
    switch (parent->getOp())
    {
    case kIROp_MakePrecise:
    case kIROp_MakePreciseIfTypeContainsPrecise:
    case kIROp_ValueWithPreciseDependency:
        markers.add(parent);
        break;
    default:
        break;
    }
    for (auto child : parent->getChildren())
        collectPreciseAnalysisInputs(child, scopeOwners, markers, declaredPolicies);
}
} // namespace

void propagatePreciseDemands(IRModule* module)
{
    List<IRInst*> scopeOwners;
    List<IRInst*> markers;
    List<IRPreciseDecoration*> declaredPolicies;
    collectPreciseAnalysisInputs(module->getModuleInst(), scopeOwners, markers, declaredPolicies);
    List<IRInst*> declaredGlobals;
    for (auto global : module->getGlobalInsts())
    {
        if (!as<IRPtrTypeBase>(unwrapAttributedType(global->getDataType())))
            continue;
        if (hasDeclaredPreciseStoragePolicy(global))
            declaredGlobals.add(global);
    }
    // At this checkpoint, only a positive field-containment result supplies a legacy policy.
    // Unknown generic or opaque fields are not exposed; every consumer above tests `Yes`.
    // The cache ends before cleanup removes the declaration metadata on which it depends.
    {
        PreciseFieldContainmentQuery containment;
        for (auto scopeOwner : scopeOwners)
            PreciseDemands(module, scopeOwner, &containment, declaredGlobals).resolve();
    }

    // Declaration and field policies are inputs to all scopes, including the cached field
    // containment query. Once every scope has resolved, operation attributes become the only
    // precision policy consumed by later optimization and emission. Retaining the declaration
    // metadata could let a later SSA pass create new, unresolved demands after ordinary inlining.
    for (auto policy : declaredPolicies)
        policy->removeAndDeallocate();

    // Removing the identity operations before ordinary optimization leaves only resolved
    // operation attributes, without barriers around subsequent uses of the computed values.
    // As with global-constant replacement, a name attached to an eliminated identity can name
    // the underlying value when that value has no name already. Only the name transfers; the
    // declaration policies have already been consumed.
    for (auto marker : markers)
    {
        auto value = marker->getOperand(0);
        if (auto nameHint = marker->findDecoration<IRNameHintDecoration>())
        {
            if (!value->findDecoration<IRNameHintDecoration>())
            {
                nameHint->removeFromParent();
                nameHint->insertAtStart(value);
            }
        }
        marker->replaceUsesWith(value);
        marker->removeAndDeallocate();
    }
}
} // namespace Slang
