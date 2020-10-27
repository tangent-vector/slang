//slang-ir-generics-lowering-context.cpp

#include "slang-ir-generics-lowering-context.h"

#include "slang-ir-layout.h"

namespace Slang
{
    bool isPolymorphicType(IRInst* typeInst)
    {
        if (as<IRParam>(typeInst) && as<IRTypeType>(typeInst->getDataType()))
            return true;
        switch (typeInst->op)
        {
        case kIROp_ThisType:
        case kIROp_AssociatedType:
        case kIROp_InterfaceType:
        case kIROp_lookup_interface_method:
            return true;
        case kIROp_Specialize:
        {
            for (UInt i = 0; i < typeInst->getOperandCount(); i++)
            {
                if (isPolymorphicType(typeInst->getOperand(i)))
                    return true;
            }
            return false;
        }
        default:
            break;
        }
        if (auto ptrType = as<IRPtrTypeBase>(typeInst))
        {
            return isPolymorphicType(ptrType->getValueType());
        }
        return false;
    }

    bool isTypeValue(IRInst* typeInst)
    {
        if (typeInst)
        {
            switch (typeInst->op)
            {
            case kIROp_TypeType:
            case kIROp_TypeKind:
                return true;
            default:
                return false;
            }
        }
        return false;
    }

    IRInst* SharedGenericsLoweringContext::maybeEmitRTTIObject(IRInst* typeInst)
    {
        IRInst* result = nullptr;
        if (mapTypeToRTTIObject.TryGetValue(typeInst, result))
            return result;
        IRBuilder builderStorage;
        auto builder = &builderStorage;
        builder->sharedBuilder = &sharedBuilderStorage;
        builder->setInsertBefore(typeInst->next);

        result = builder->emitMakeRTTIObject(typeInst);

        // For now the only type info we encapsualte is type size.
        IRSizeAndAlignment sizeAndAlignment;
        getNaturalSizeAndAlignment(targetReq, (IRType*)typeInst, &sizeAndAlignment);
        builder->addRTTITypeSizeDecoration(result, sizeAndAlignment.size);

        // Give a name to the rtti object.
        if (auto exportDecoration = typeInst->findDecoration<IRExportDecoration>())
        {
            String rttiObjName = exportDecoration->getMangledName();
            builder->addExportDecoration(result, rttiObjName.getUnownedSlice());
        }
        // Make sure the RTTI object for a public struct type has public visiblity.
        if (typeInst->findDecoration<IRPublicDecoration>())
        {
            builder->addPublicDecoration(result);
            builder->addKeepAliveDecoration(result);
        }
        mapTypeToRTTIObject[typeInst] = result;
        return result;
    }

    IRInst* SharedGenericsLoweringContext::findInterfaceRequirementVal(IRInterfaceType* interfaceType, IRInst* requirementKey)
    {
        if (auto dict = mapInterfaceRequirementKeyValue.TryGetValue(interfaceType))
            return (*dict)[requirementKey].GetValue();
        _builldInterfaceRequirementMap(interfaceType);
        return findInterfaceRequirementVal(interfaceType, requirementKey);
    }

    void SharedGenericsLoweringContext::_builldInterfaceRequirementMap(IRInterfaceType* interfaceType)
    {
        mapInterfaceRequirementKeyValue.Add(interfaceType,
            Dictionary<IRInst*, IRInst*>());
        auto dict = mapInterfaceRequirementKeyValue.TryGetValue(interfaceType);
        for (UInt i = 0; i < interfaceType->getOperandCount(); i++)
        {
            auto entry = cast<IRInterfaceRequirementEntry>(interfaceType->getOperand(i));
            (*dict)[entry->getRequirementKey()] = entry->getRequirementVal();
        }
    }

    IRType* SharedGenericsLoweringContext::lowerAssociatedType(IRBuilder* builder, IRInst* type)
    {
        if (type->op != kIROp_AssociatedType)
            return (IRType*)type;
        IRIntegerValue anyValueSize = kInvalidAnyValueSize;
        for (UInt i = 0; i < type->getOperandCount(); i++)
        {
            anyValueSize = Math::Min(
                anyValueSize,
                getInterfaceAnyValueSize(type->getOperand(i), type->sourceLoc));
        }
        if (anyValueSize == kInvalidAnyValueSize)
        {
            anyValueSize = kDefaultAnyValueSize;
//            sink->diagnose(type->sourceLoc, Diagnostics::dynamicInterfaceLacksAnyValueSizeAttribute, type);
        }
        return builder->getAnyValueType(anyValueSize);
    }

    IRType* SharedGenericsLoweringContext::lowerType(IRBuilder* builder, IRInst* paramType, const Dictionary<IRInst*, IRInst*>& typeMapping, IRType* concreteType)
    {
        if (!paramType)
            return nullptr;

        IRInst* resultType;
        if (typeMapping.TryGetValue(paramType, resultType))
            return (IRType*)resultType;

        if (isTypeValue(paramType))
        {
            return builder->getRTTIHandleType();
        }

//        IRIntegerValue anyValueSize = kInvalidAnyValueSize;
        switch (paramType->op)
        {
        case kIROp_WitnessTableType:
        case kIROp_WitnessTableIDType:
        case kIROp_ExtractExistentialType:
            // Do not translate these types.
            return (IRType*)paramType;
        case kIROp_Param:
        {
            if (auto anyValueSizeDecor = paramType->findDecoration<IRTypeConstraintDecoration>())
            {
                if (isBuiltin(anyValueSizeDecor->getConstraintType()))
                    return (IRType*)paramType;
                auto anyValueSize = getInterfaceAnyValueSize(anyValueSizeDecor->getConstraintType(), paramType->sourceLoc);
                return builder->getAnyValueType(anyValueSize);
            }
//            sink->diagnose(paramType, Diagnostics::unconstrainedGenericParameterNotAllowedInDynamicFunction, paramType);
            return builder->getAnyValueType(kDefaultAnyValueSize);
        }
        case kIROp_ThisType:
        {

            if (isBuiltin(cast<IRThisType>(paramType)->getConstraintType()))
                return (IRType*)paramType;
            auto anyValueSize = getInterfaceAnyValueSize(
                cast<IRThisType>(paramType)->getConstraintType(),
                paramType->sourceLoc);
            return builder->getAnyValueType(anyValueSize);
        }
        case kIROp_AssociatedType:
        {
            return lowerAssociatedType(builder, paramType);
        }
        case kIROp_InterfaceType:
        {
            if (isBuiltin(paramType))
                return (IRType*)paramType;

            // TODO(tfoley) if `concreteType` is set, then we need to take it into account
            // and make sure that the tuple we build either slots it into the "any value"
            // if it will fit, or as a kind of pseudo-ptr if it doesn't fit.
            //
            auto anyValueSize = getInterfaceAnyValueSize(paramType, paramType->sourceLoc);

            IRType* pendingType = nullptr;
            if( concreteType )
            {
                IRSizeAndAlignment sizeAndAlignment;
                Result result = getNaturalSizeAndAlignment(targetReq, concreteType, &sizeAndAlignment);
                if(SLANG_FAILED(result) || (sizeAndAlignment.size > anyValueSize))
                {
                    // The value does not fit, either because it is too large,
                    // or because it includes types that cannot be stored
                    // in uniform/ordinary memory for this target.
                    //
                    // We need to construct a placeholder to represent the data
                    // that will actually be stored out-of-line, with the understanding
                    // that the placeholder will be moved to the correct location
                    // by a later type legalization step.

                    // TODO: actually construct something here...
                    pendingType = builder->getPseudoPtrType(concreteType);
                }
            }

            // An existential type translates into a tuple of (RTTI, witness table, any-value)
            auto anyValueType = builder->getAnyValueType(anyValueSize);
            auto witnessTableType = builder->getWitnessTableIDType((IRType*)paramType);
            auto rttiType = builder->getRTTIHandleType();

            IRType* tupleType = nullptr;
            if( pendingType )
            {
                tupleType = builder->getTupleType(rttiType, witnessTableType, pendingType, anyValueType);
            }
            else
            {
                tupleType = builder->getTupleType(rttiType, witnessTableType, anyValueType);
            }

            return tupleType;
        }
        case kIROp_lookup_interface_method:
        {
            auto lookupInterface = static_cast<IRLookupWitnessMethod*>(paramType);
            auto interfaceType = cast<IRInterfaceType>(cast<IRWitnessTableType>(
                lookupInterface->getWitnessTable()->getDataType())->getConformanceType());
            if (isBuiltin(interfaceType))
                return (IRType*)paramType;
            // Make sure we are looking up inside the original interface type (prior to lowering).
            // Only in the original interface type will an associated type entry have an IRAssociatedType value.
            // We need to extract AnyValueSize from this IRAssociatedType.
            // In lowered interface type, that entry is lowered into an Ptr(RTTIType) and this info is lost.
            mapLoweredInterfaceToOriginal.TryGetValue(interfaceType, interfaceType);
            auto reqVal = findInterfaceRequirementVal(
                interfaceType,
                lookupInterface->getRequirementKey());
            SLANG_ASSERT(reqVal && reqVal->op == kIROp_AssociatedType);
            return lowerType(builder, reqVal, typeMapping, nullptr);
        }
        case kIROp_BoundInterfaceType:
        {
            auto boundInterfaceType = static_cast<IRBoundInterfaceType*>(paramType);
            return lowerType(builder, boundInterfaceType->getInterfaceType(), typeMapping, boundInterfaceType->getConcreteType());
        }
        default:
        {
            bool translated = false;
            List<IRInst*> loweredOperands;
            for (UInt i = 0; i < paramType->getOperandCount(); i++)
            {
                loweredOperands.add(lowerType(builder, paramType->getOperand(i), typeMapping, nullptr));
                if (loweredOperands.getLast() != paramType->getOperand(i))
                    translated = true;
            }
            if (translated)
                return builder->getType(paramType->op, loweredOperands.getCount(), loweredOperands.getBuffer());
            return (IRType*)paramType;
        }
        }
    }

    List<IRWitnessTable*> SharedGenericsLoweringContext::getWitnessTablesFromInterfaceType(IRInst* interfaceType)
    {
        List<IRWitnessTable*> witnessTables;
        for (auto globalInst : module->getGlobalInsts())
        {
            if (globalInst->op == kIROp_WitnessTable &&
                cast<IRWitnessTableType>(globalInst->getDataType())->getConformanceType() ==
                    interfaceType)
            {
                witnessTables.add(cast<IRWitnessTable>(globalInst));
            }
        }
        return witnessTables;
    }

    IRIntegerValue SharedGenericsLoweringContext::getInterfaceAnyValueSize(IRInst* type, SourceLoc usageLocation)
    {
        SLANG_UNUSED(usageLocation);

        if (auto decor = type->findDecoration<IRAnyValueSizeDecoration>())
        {
            return decor->getSize();
        }
        return kDefaultAnyValueSize;
//        sink->diagnose(type->sourceLoc, Diagnostics::dynamicInterfaceLacksAnyValueSizeAttribute, type);
//        sink->diagnose(usageLocation, Diagnostics::seeInterfaceUsage, type);
//        return kInvalidAnyValueSize;
    }


    bool SharedGenericsLoweringContext::doesTypeFitInAnyValue(IRType* concreteType, IRInterfaceType* interfaceType)
    {
        auto anyValueSize = getInterfaceAnyValueSize(interfaceType, interfaceType->sourceLoc);

        IRSizeAndAlignment sizeAndAlignment;
        Result result = getNaturalSizeAndAlignment(targetReq, concreteType, &sizeAndAlignment);
        if(SLANG_FAILED(result) || (sizeAndAlignment.size > anyValueSize))
        {
            // The value does not fit, either because it is too large,
            // or because it includes types that cannot be stored
            // in uniform/ordinary memory for this target.
            //
            return false;
        }

        return true;
    }

}
