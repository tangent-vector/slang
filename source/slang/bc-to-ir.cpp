// bc-to-ir.cpp
#include "bc-to-ir.h"

#include "bytecode.h"

#include "ir-insts.h"

namespace Slang
{
    struct SharedBCToIRContext
    {
        CompileRequest*         compileRequest;
        ISlangBlob*             blob;

        SlangBCFileHeader*      bcFileHeader;
        SlangBCIRSectionHeader* bcIR;

        SharedIRBuilder         sharedBuilder;
    };

    struct BCToIRContext
    {
        SharedBCToIRContext* shared;

        BCToIRContext* parent = nullptr;

        IRBuilder           builder;

        List<IRInst*>       upValues;
        List<IRInst*>       localValues;
        List<IRBlock*>      blocks;
    };


    DiagnosticSink* getSink(
        BCToIRContext*  context)
    {
        return &context->shared->compileRequest->mSink;
    }

    IRBuilder* getBuilder(
        BCToIRContext*  context)
    {
        return &context->builder;
    }

    SlangBCIRNode* getBCNode(
        BCToIRContext*  context,
        UInt            index)
    {
        auto bcIR = context->shared->bcIR;
        auto entry = (SlangBCIRNodeTableEntry*)((char*)bcIR
            + bcIR->nodeTableOffset
            + index * bcIR->nodeTableEntrySize);

        return (SlangBCIRNode*)((char*)bcIR
            + entry->nodeOffset);
    }

    UInt decodeUInt(BCOp** ioPtr);
    Int decodeSInt(BCOp** ioPtr);

    IRInst* getLocal(
        BCToIRContext*  context,
        Int             id)
    {
        if(id >= 0)
        {
            // This is a "local" ID, and maps
            // to one of the "registers" in the
            // current function/node.
            //
            return context->localValues[id];
        }
        else
        {
            // This is a reference to an "up-value"
            // from outside of the current function/node
            //
            return context->upValues[~id];
        }
    }

    IRInst* decodeOperand(
        BCToIRContext*  context,
        SlangBCCode*&   ioCode)
    {
        auto id = decodeSInt(&ioCode);
        return getLocal(context, id);
    }

    void translateDestination(
        BCToIRContext*  context,
        IRInst*         irInst,
        SlangBCCode*&   ioCode)
    {
        auto destReg = decodeSInt(&ioCode);
        context->localValues[destReg] = irInst;
    }

    void translateCode(
        BCToIRContext*  context,
        SlangBCCode*    code)
    {
        auto builder = getBuilder(context);
        auto ip = code;
        for(;;)
        {
            auto op = (IROp) decodeUInt(&ip);
            switch(op)
            {
            default:
                {
                    auto fullType = (IRType*) decodeOperand(context, ip);
                    auto operandCount = decodeUInt(&ip);
                    List<IRInst*> operands;
                    for(UInt ii = 0; ii < operandCount; ++ii)
                    {
                        operands.Add(decodeOperand(context, ip));
                    }

                    auto irInst = builder->emitIntrinsicInst(
                        fullType,
                        op,
                        operands.Count(),
                        operands.Buffer());

                    auto dataType = irInst->getDataType();
                    if(dataType && dataType->op != kIROp_VoidType)
                    {
                        translateDestination(context, irInst, ip);
                    }
                }
                break;

            case kIROp_TypeKind:
            case kIROp_GenericKind:
                {
                    auto irInst = builder->getKind(op);
                    translateDestination(context, irInst, ip);
                }
                break;

            case kIROp_ReturnVoid:
                builder->emitReturn();
                return;

            case kIROp_End:
                return;

#if 0
    case kIROp_IntLit:
        {
            auto ii = (IRConstant*) inst;
            encodeUInt(context, ii->op);
            encodeOperand(context, ii->getDataType());

            // TODO: probably want distinct encodings
            // for signed vs. unsigned here.
            encodeUInt(context, UInt(ii->u.intVal));

            // destination:
            encodeOperand(context, inst);
        }
        break;

    case kIROp_FloatLit:
        {
            auto cInst = (IRConstant*) inst;
            encodeUInt(context, cInst->op);
            encodeOperand(context, cInst->getDataType());

            static const UInt size = sizeof(IRFloatingPointValue);
            unsigned char buffer[size];
            memcpy(buffer, &cInst->u.floatVal, sizeof(buffer));

            for(UInt ii = 0; ii < size; ++ii)
            {
                encodeUInt8(context, buffer[ii]);
            }

            // destination:
            encodeOperand(context, inst);
        }
        break;

    case kIROp_boolConst:
        {
            auto ii = (IRConstant*) inst;
            encodeUInt(context, ii->op);
            encodeUInt(context, ii->u.intVal ? 1 : 0);

            // destination:
            encodeOperand(context, inst);
        }
        break;

    case kIROp_Store:
        {
            encodeUInt(context, inst->op);

            // We need to encode the type being stored, to make
            // our lives easier.
            encodeOperand(context, inst->getOperand(1)->getDataType());
            encodeOperand(context, inst->getOperand(0));
            encodeOperand(context, inst->getOperand(1));
        }
        break;

    case kIROp_Load:
        {
            encodeUInt(context, inst->op);
            encodeOperand(context, inst->getDataType());
            encodeOperand(context, inst->getOperand(0));
            encodeOperand(context, inst);
        }
        break;
    }
#endif



            }
        }
    }

    void populateParentInst(
        BCToIRContext*  context,
        IRParentInst*   irParent,
        SlangBCIRNode*  bcParent)
    {
        auto builder = getBuilder(context);

        UInt upValueCount = bcParent->upValueCount;
        for(UInt ii = 0; ii < upValueCount; ++ii)
        {
            auto bcUpValue = getUpValue(bcParent, ii);

            context->upValues.Add(
                getLocal(context->parent, bcUpValue->id));
        }

        UInt regCount = bcParent->registerCount;
        for(UInt ii = 0; ii < regCount; ++ii)
        {
            context->localValues.Add(nullptr);
        }

        if(auto irCode = as<IRGlobalValueWithCode>(irParent))
        {
            UInt blockCount = bcParent->blockCount;
            for(UInt bb = 0; bb < blockCount; ++bb)
            {
                auto bcBlock = getBlock(bcParent, bb);
                auto irBlock = builder->createBlock();

                auto bcCode = getCode(bcParent) + bcBlock->codeOffset;
                builder->setInsertInto(irBlock);

                translateCode(context, bcCode);
            }
        }
        else
        {
            auto bcCode = getCode(bcParent);
            builder->setInsertInto(irParent);
            translateCode(context, bcCode);
        }
    }

    RefPtr<IRModule> loadBinaryModuleIR(
        CompileRequest* compileRequest,
        ISlangBlob*     blob)
    {
        auto bcFileHeader = (SlangBCFileHeader*) blob->getBufferPointer();

        SharedBCToIRContext sharedContextStorage;
        auto shared = &sharedContextStorage;
        shared->compileRequest  = compileRequest;
        shared->blob            = blob;
        shared->bcFileHeader = bcFileHeader;

        BCToIRContext contextStorage;
        auto context = &contextStorage;
        context->shared = shared;
        context->builder.sharedBuilder = &shared->sharedBuilder;

        UInt sectionCount = bcFileHeader->sectionTableEntryCount;
        SlangBCIRSectionHeader* bcIR = nullptr;
        for(UInt ii = 0; ii < sectionCount; ++ii)
        {
            auto sectionTableEntry = getSectionTableEntry(bcFileHeader, ii);
            if(sectionTableEntry.type != SLANG_BC_SECTION_TYPE_IR)
                continue;

            // Okay, we have an IR section, and we should try to load it.

            bcIR = (SlangBCIRSectionHeader*) getSectionData(bcFileHeader, sectionTableEntry);

            // We are breaking out of our search as soon as we find an IR
            // section, even if it isn't the only one. Is this reasonable?
            break;
        }
        if(!bcIR)
        {
            getSink(context)->diagnose(SourceLoc(), Diagnostics::unexpected, "invalid Slang bytecode file");
            return nullptr;
        }
        shared->bcIR = bcIR;


        // We assume for now that node #0 always represents the module
        auto bcModule = getBCNode(context, 0);

        auto builder = getBuilder(context);
        RefPtr<IRModule> module = builder->createModule();
        builder->sharedBuilder->module = module;

        populateParentInst(context, module->getModuleInst(), bcModule);

        return module;
    }
}
