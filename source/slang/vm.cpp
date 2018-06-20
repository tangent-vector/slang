#include "vm.h"

// Implementation of the Slang bytecode VM
//

#include "bytecode.h"
#include "ir.h"

#include "../../slang.h"

// TODO: commenting this all out for now, so that we don't have to maintain
// the VM implementation when bringing up the new BC module format.
namespace Slang
{

struct VMValImpl
{
    // opcode used to construct the value
    uint32_t op;
};

// Representation of a type during VM execution
struct VMTypeImpl : VMValImpl
{
    // number of arguments to the type
    uint32_t argCount;

    // Size and alignment of instances of this
    // type.
    UInt    size;
    UInt    alignment;

    // operands follow
};

struct VMVal
{
    VMValImpl*  impl;

    VMValImpl* getImpl() { return impl; }
};

struct VMType : VMVal
{
    VMTypeImpl* getImpl() { return (VMTypeImpl*) impl; }
    UInt getSize() { return getImpl()->size; }
    UInt getAlignment() { return getImpl()->alignment;  }
};

struct VMPtrTypeImpl : VMTypeImpl
{
    VMType base;
};

struct VMRegister
{
    // Type that the register is meant to hold
    VMType  type;

    // offset of the variable inside the frame
    size_t  offset;
};

struct VMUpValue
{
    // Type of the value
    VMType  type;

    // Operand address to use
    void*   ptr;
};

struct VM;
struct VMFrame;

struct VMModuleFile
{
    VM*                     vm;
    SlangBCIRSectionHeader* bcIR;

    SlangBCIRNode* getBCNode(UInt index)
    {
        auto entry = (SlangBCIRNodeTableEntry*)((char*)bcIR
            + bcIR->nodeTableOffset
            + index * bcIR->nodeTableEntrySize);

        return (SlangBCIRNode*)((char*)bcIR
            + entry->nodeOffset);
    }
};

// Information about a function after it has been
// loaded into the VM.
struct VMFunc
{
    VMModuleFile* moduleFile;

    // The parent frame/module that this function belongs to
    VMFrame*   parent;

    SlangBCIRNode const* bcFunc;

    VMRegister* registers;
    VMUpValue*  upValues;

    size_t      frameSize;

    SlangBCBlock* getBCBlock(UInt index)
    {
        return moduleFile->getBCBlock(bcFunc->firstBlockIndex + index);
    }
};

struct VMFrame
{
    // The function from which this frame was spawned
    VMFunc*     func;

    // The parent frame on the call stack of the
    // current thread.
    VMFrame*    parent;

    // The instruction pointer within this frame
    SlangBCCode* ip;

    // Registers are stored after this point.
};

struct VM;

struct VMModule : VMFrame
{
    VM*         vm;
};

UInt decodeUInt(BCOp** ioPtr)
{
    BCOp* ptr = *ioPtr;

    UInt value = *ptr++;
    if( value < 128 )
    {
        *ioPtr = ptr;
        return value;
    }

    // Slower path for variable-length encoding

    UInt result = 0;
    for(;;)
    {
        value = value & 0x7F;
        result = (result << 7) | value;

        if(value < 127)
        {
            *ioPtr = ptr;
            return value;
        }

        value = *ptr++;
    }
}

Int decodeSInt(BCOp** ioPtr)
{
    UInt uVal = decodeUInt(ioPtr);
    if( uVal & 1 )
    {
        return Int(~(uVal >> 1));
    }
    else
    {
        return Int(uVal >> 1);
    }
}

void* getRegPtrImpl(VMFrame* frame, UInt id)
{
    VMFunc* vmFunc = frame->func;
    VMRegister* vmRegister = &vmFunc->registers[id];
    size_t offset = vmRegister->offset;

    return (void*)((char*)frame + offset);
}

void* getOperandPtrImpl(VMFrame* frame, Int id)
{
    if( id >= 0 )
    {
        // This ID represents a local variable/register
        // of the current call frame, and should
        // be used to index into a table of such values.

        return getRegPtrImpl(frame, id);
    }
    else
    {
        // This ID represents a global value that has
        // been imported into the current node from
        // its parent frame (or from further up the
        // chain).

        VMFunc* vmFunc = frame->func;
        VMUpValue* vmUpValue = &vmFunc->upValues[~id];
        return vmUpValue->ptr;
    }
}

VMType getOperandTypeImpl(VMFrame* frame, Int id)
{
    if( id >= 0 )
    {
        return frame->func->registers[id].type;
    }
    else
    {
        return frame->func->upValues[~id].type;
    }
}
struct VMPtrAndType
{
    void*   ptr;
    VMType  type;
};

VMPtrAndType decodeOperandPtrAndType(VMFrame* frame, BCOp** ioIP)
{
    Int id = decodeSInt(ioIP);

    VMPtrAndType ptrAndType;
    ptrAndType.ptr = getOperandPtrImpl(frame, id);
    ptrAndType.type = getOperandTypeImpl(frame, id);
    return ptrAndType;
}

void* decodeOperandPtrImpl(VMFrame* frame, BCOp** ioIP)
{
    Int id = decodeSInt(ioIP);
    return getOperandPtrImpl(frame, id);
}


template<typename T>
T* decodeOperandPtr(VMFrame* frame, BCOp** ioIP)
{
    return (T*)decodeOperandPtrImpl(frame, ioIP);
}


template<typename T>
T& decodeOperand(VMFrame* frame, BCOp** ioIP)
{
    return *decodeOperandPtr<T>(frame, ioIP);
}

VMType decodeType(VMFrame* frame, BCOp** ioIP)
{
    return decodeOperand<VMType>(frame, ioIP);
}

VMFunc* loadVMFunc(
    BCFunc*     bcFunc,
    VMModule*   vmModule);

struct VMSizeAlign
{
    UInt size;
    UInt align;
};

#if 0
VMSizeAlign getVMSymbolSize(BCSymbol* symbol)
{
    VMSizeAlign result;
    result.size = sizeof(void*);
    result.align = sizeof(void*);
    switch( symbol->op )
    {
    default:
        SLANG_UNEXPECTED("op");
        break;

    case kIROp_TypeKind:
        break;

    case kIROp_Func:
        {
            BCFunc* func = (BCFunc*) symbol;
            result.size = sizeof(VMFunc)
                + func->regCount   * sizeof(VMReg)
                + func->constCount * sizeof(VMConst);
        }
        break;
    }

    return result;
}
#endif

//void* getGlobalPtr(
//    VMModule*   vmModule,
//    uint32_t    globalID)
//{
//    return vmModule->symbols[globalID];
//}

//VMType getType(
//    VMModule*   vmModule,
//    uint32_t    typeID)
//{
//    VMType type;
//    type.impl = (VMTypeImpl*)getGlobalPtr(vmModule, typeID);
//}

//VMType getGlobalType(
//    VMModule*   vmModule,
//    uint32_t    globalID)
//{
//    return getType(vmModule, vmModule->bcModule->symbols[globalID]->typeID);
//}

BCReg* getRegister(SlangBCIRSectionHeader* bcIR, UInt index)
{
    return (BCReg*)((char*)bcIR
        + bcIR->registersOffset
        + index * bcIR->registerSize);
}

SlangBCUpValue* getUpValue(SlangBCIRSectionHeader* bcIR, UInt index)
{
    return (SlangBCUpValue*)((char*)bcIR
        + bcIR->upValuesOffset
        + index * bcIR->upValueSize);
}

VMFunc* loadVMFunc(
    SlangBCIRSectionHeader* bcIR,
    BCFunc*                 bcFunc,
    VMFrame*                parentFrame)
{
    UInt regCount = bcFunc->registerCount;
    UInt upValueCount = bcFunc->upValueCount;
    UInt vmFuncSize = sizeof(VMFunc)
        + regCount * sizeof(VMRegister)
        + upValueCount * sizeof(VMUpValue);

    VMFunc* vmFunc = (VMFunc*) malloc(vmFuncSize);
    VMRegister* vmRegs = (VMRegister*) (vmFunc + 1);
    VMUpValue* vmUpValues = (VMUpValue*) (vmRegs + regCount);

    vmFunc->parent = parentFrame;
    vmFunc->bcFunc = bcFunc;
    vmFunc->registers = vmRegs;
    vmFunc->upValues = vmUpValues;

    UInt offset = sizeof(VMFrame);
    for( UInt rr = 0; rr < regCount; ++rr )
    {
        BCReg* bcReg = getRegister(bcIR, bcFunc->firstRegisterIndex + rr);
        auto bcTypeID = bcReg->typeID;

        // We expect the type to come from the outer module, so
        // that we can allocate space for it as we go.
        auto vmType = *(VMType*)getOperandPtrImpl(parentFrame, bcTypeID);

        auto regSize = vmType.getSize();
        auto regAlign = vmType.getAlignment();

        offset = (offset + (regAlign-1)) & ~(regAlign-1);

        size_t regOffset = offset;
        offset += regSize;

        vmRegs[rr].type = vmType;
        vmRegs[rr].offset = regOffset;
    }
    vmFunc->frameSize = offset;

    for( UInt uu = 0; uu < upValueCount; ++uu )
    {
        SlangBCUpValue* bcUpValue = getUpValue(bcIR, bcFunc->firstUpValueIndex + uu);
        auto upValueID = bcUpValue->id;
        vmFunc->upValues[uu].ptr = getOperandPtrImpl(parentFrame, upValueID);
        vmFunc->upValues[uu].type = getOperandTypeImpl(parentFrame, upValueID);
    }

    return vmFunc;
}

VMFrame* createFrame(VMFunc* vmFunc)
{
    VMFrame* vmFrame = (VMFrame*) malloc(vmFunc->frameSize);
    vmFrame->func = vmFunc;
    vmFrame->ip = vmFunc->moduleFile->getBCCode()
        + vmFunc->getBCBlock(0)->codeOffset;
    return vmFrame;
}

void dumpVMFrame(VMFrame* vmFrame)
{
    fflush(stderr);

    // We want to walk the VM frame and dump the
    // state of all of its logical registers.
    // For now this is made easier by having
    // no overlapping register assignments...
    VMFunc* vmFunc = vmFrame->func;
    BCFunc const* bcFunc = vmFunc->bcFunc;
    UInt regCount = bcFunc->registerCount;

    for (UInt rr = 0; rr < regCount; ++rr)
    {
//        VMType regType = getOperandTypeImpl(vmFrame, rr);
//        void* regData = getRegPtrImpl(vmFrame, rr);
/*
        char const* name = bcFunc->registers[rr].name;

        // Use the type to print the data...

        fprintf(stderr, "0x%p: ", regData);

        fprintf(stderr, "%%%u ", (unsigned int) rr);
        if (name)
        {
            fprintf(stderr, "\"%s\" ", name);
        }
        if (regType.impl)
        {
            switch (regType.impl->op)
            {
            case kIROp_TypeKind:
                // TODO: we could recursively go and print types...
                fprintf(stderr, ": Type = ???");
                break;

            case kIROp_HLSLRWStructuredBufferType:
                fprintf(stderr, ": RWStructuredBuffer<???> = ???");
                break;

            case kIROp_HLSLStructuredBufferType:
                fprintf(stderr, ": StructuredBuffer<???> = ???");
                break;

            case kIROp_BoolType:
                fprintf(stderr, ": Bool = %s", *(bool*)regData ? "true" : "false");
                break;

            case kIROp_IntType:
                fprintf(stderr, ": Int32 = %d", *(int32_t*)regData);
                break;

            case kIROp_UIntType:
                fprintf(stderr, ": UInt32 = %u", *(uint32_t*)regData);
                break;

            case kIROp_PtrType:
                {
                    fprintf(stderr, ": Ptr<?> = [%p]", *(void**)regData);
                }
                break;

            default:
                fprintf(stderr, "<unknown>");
                break;
            }
        }
        else
        {
            fprintf(stderr, ": <null>");
        }
        fprintf(stderr, "; // ");
        fprintf(stderr, "%s", getIROpInfo((IROp) bcFunc->regs[rr].op).name);
        fprintf(stderr, "\n");

*/
        // Okay, now we need to use the type
        // stored in the VM register to tell
        // us how to print things.
    }

    IROp op = IROp(*vmFrame->ip);
    IROpInfo opInfo = getIROpInfo(op);
    fprintf(stderr, "NEXT op: %s\n", opInfo.name);

    fflush(stderr);
}

struct VM
{
};

VM* createVM()
{
    VM* vm = new VM();
    return vm;
}

struct VMThread
{
    // The currently executing call frame
    VMFrame*    frame;
};

void resumeThread(
    VMThread*   vmThread);

void computeTypeSizeAlign(
    VMTypeImpl* impl)
{
    UInt size = 0;
    UInt alignment = 0;
    switch(impl->op)
    {
    case kIROp_VoidType:
        size = 0;
        break;

    case kIROp_BoolType:
        size = 1;
        break;

    case kIROp_IntType:
    case kIROp_UIntType:
    case kIROp_FloatType:
        size = 4;
        break;

    case kIROp_FuncType:
    case kIROp_PtrType:
    case kIROp_HLSLRWStructuredBufferType:
    case kIROp_HLSLStructuredBufferType:
        size = sizeof(void*);
        break;

    default:
        SLANG_UNIMPLEMENTED_X("type sizing");
        UNREACHABLE(impl->size = 0);
        break;
    }

    if(!alignment)
        alignment = size;
    if(!alignment)
        alignment = 1;

    impl->size = size;
    impl->alignment = alignment;
}

VMType getType(
    VM*         /*vm*/,
    VMTypeImpl* typeImpl)
{
    // TODO: need to look up an existing type that matches...

    UInt argCount = typeImpl->argCount;
    UInt size = sizeof(VMTypeImpl) + argCount*sizeof(VMType);

    VMTypeImpl* impl = (VMTypeImpl*) malloc(size);
    memcpy(impl, typeImpl, size);

    computeTypeSizeAlign(impl);

    VMType type;
    type.impl = impl;
    return type;
}

void* allocateImpl(VM* /*vm*/, UInt size, UInt /*align*/)
{
    void* ptr = malloc(size);
    memset(ptr, 0, size);
    return ptr;
}

void* allocate(VM* vm, VMType type)
{
    return allocateImpl(vm, type.getSize(), type.getAlignment());
}

template<typename T>
T* allocate(VM* vm)
{
    return allocateImpl(vm, sizeof(T), alignof(T));
}

VMModuleFile* loadVMModuleInstance(
    VM*         vm,
    void const* bytecode,
    size_t      /*bytecodeSize*/)
{
    BCFileHeader* bcFileHeader = (BCFileHeader*)bytecode;

    // Need to find section(s) that represent compiled translation units

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
        return nullptr;

    UInt vmModuleFileSize = sizeof(VMModuleFile);
//        + symbolCount * sizeof(void*)
//        + typeCount * sizeof(VMType);

    VMModuleFile* vmModuleFile = (VMModuleFile*)malloc(vmModuleFileSize);
    memset(vmModuleFile, 0, vmModuleFileSize);

    vmModuleFile->vm = vm;
    vmModuleFile->bcIR = bcIR;
    vmModuleFile->bcCode

    /*
    UInt bcModuleCount = bcHeader->moduleCount;
    if (bcModuleCount == 0)
        return nullptr;

    BCModule* bcModule = bcHeader->modules[0];

    UInt symbolCount = bcModule->symbolCount;
    UInt typeCount = bcModule->typeCount;

    UInt vmModuleSize = sizeof(VMModule)
        + symbolCount * sizeof(void*)
        + typeCount * sizeof(VMType);

    VMModule* vmModule = (VMModule*)malloc(vmModuleSize);
    memset(vmModule, 0, vmModuleSize);

    void** vmSymbols = (void**)(vmModule + 1);
    VMType* vmTypes = (VMType*)(vmSymbols + symbolCount);

    vmModule->bcModule = bcModule;
    vmModule->vm = vm;
    vmModule->symbols = vmSymbols;
    vmModule->types = vmTypes;

    // Initialize types before symbols, since the symbols
    // will all have types...
    for(UInt tt = 0; tt < typeCount; ++tt)
    {
        BCType* bcType = bcModule->types[tt];
        vmTypes[tt] = loadVMType(vmModule, bcType);
    }

    // Now we need to initialize all the VM-level symbols
    // from their BC-level equivalents.
    for(UInt ss = 0; ss < symbolCount; ++ss)
    {
        BCSymbol* bcSymbol = bcModule->symbols[ss];
        vmSymbols[ss] = loadVMSymbol(vmModule, bcSymbol);
    }

    return vmModule;
    */
}

void* findGlobalSymbolPtr(
    VMModule*   module,
    char const* name)
{
    // Okay, we need to search through the available
    // symbols, looking for one that gives us a name
    // match.

    BCModule* bcModule = module->bcModule;
    UInt symbolCount = bcModule->symbolCount;
    for(UInt ss = 0; ss < symbolCount; ++ss)
    {
        BCSymbol* bcSymbol = bcModule->symbols[ss];

        char const* symbolName = bcSymbol->name;
        if (!symbolName)
            continue;

        if(strcmp(symbolName, name) == 0)
            return getGlobalPtr(module, (uint32_t)ss);
    }

    return nullptr;
}

VMThread* createThread(
    VM* /*vm*/)
{
    VMThread* thread = new VMThread();
    thread->frame = nullptr;
    return thread;
}

void beginCall(
    VMThread*   vmThread,
    VMFunc*     vmFunc)
{
    VMFrame* vmFrame = createFrame(vmFunc);

    vmFrame->parent = vmThread->frame;
    vmThread->frame = vmFrame;
}

void setArg(
    VMThread*   vmThread,
    UInt        argIndex,
    void const* data,
    size_t      size)
{
    // TODO: need all kinds of validation here...

    void* dest = getRegPtrImpl(vmThread->frame, argIndex);
    memcpy(dest, data, size);
}

void resumeThread(
    VMThread*   vmThread)
{
    auto frame = vmThread->frame;
    auto ip = frame->ip;

    for(;;)
    {
#if 0
        // debugging:
        frame->ip = ip;
        dumpVMFrame(frame);
#endif

        auto op = (IROp) decodeUInt(&ip);
        switch( op )
        {
        case kIROp_Var:
            {
                // This instruction represents the `alloca` for a variable of some type.

                VMType type = decodeType(frame, &ip);
                UInt argCount = decodeUInt(&ip);
                void* argPtrs[16] = { 0 };
                for( UInt aa = 0; aa < argCount; ++aa )
                {
                    void* argPtr = decodeOperandPtr<void>(frame, &ip);
                    argPtrs[aa] = argPtr;
                }

                void** destPtr = decodeOperandPtr<void*>(frame, &ip);

                // For now this is a bit silly and simple: the
                // storage for the variable we are "allocating"
                // should be right after outer destination, so we can
                // set it pretty easily.

                *destPtr = destPtr + 1;
            }
            break;

        case kIROp_Store:
            {
                // An ordinary memory store
                VMType type = decodeType(frame, &ip);
                void* dest = decodeOperand<void*>(frame, &ip);
                void* src = decodeOperandPtr<void>(frame, &ip);

#if 0
                fprintf(stderr, "STORE *[%p] = [%p] // size: %d\n",
                    dest,
                    src,
                    (int) type.getSize());
#endif

                memcpy(dest, src, type.getSize());
            }
            break;

        case kIROp_Load:
            {
                // An ordinary memory store
                VMType type = decodeType(frame, &ip);
                void* src = decodeOperand<void*>(frame, &ip);
                void* dest = decodeOperandPtr<void>(frame, &ip);

                memcpy(dest, src, type.getSize());
            }
            break;

        case kIROp_BufferLoad:
            {
                VMType type = decodeType(frame, &ip);
                UInt argCount = decodeUInt(&ip);
                void* argPtrs[16] = { 0 };
                for( UInt aa = 0; aa < argCount; ++aa )
                {
                    void* argPtr = decodeOperandPtr<void>(frame, &ip);
                    argPtrs[aa] = argPtr;
                }

                void* dest = decodeOperandPtr<void>(frame, &ip);

                char* bufferData = *(char**)argPtrs[0];
                uint32_t index = *(uint32_t*)argPtrs[1];

                auto size = type.getSize();
                char* elementData = bufferData + index*size;
                memcpy(dest, elementData, size);
            }
            break;

        case kIROp_BufferStore:
            {
                VMType resultType = decodeType(frame, &ip);
                /*UInt argCount = */decodeUInt(&ip);

                char* bufferData = decodeOperand<char*>(frame, &ip);
                uint32_t index = decodeOperand<uint32_t>(frame, &ip);

                auto srcPtrAndType = decodeOperandPtrAndType(frame, &ip);
                void* srcPtr = srcPtrAndType.ptr;
                VMType type = srcPtrAndType.type;

                auto size = type.getSize();
                char* elementData = bufferData + index*size;
                memcpy(elementData, srcPtr, size);
            }
            break;

        case kIROp_BufferElementRef:
            {
                VMType ptrType = decodeType(frame, &ip);
                VMType type = ((VMPtrTypeImpl*)ptrType.getImpl())->base;

                UInt argCount = decodeUInt(&ip);
                void* argPtrs[16] = { 0 };
                for( UInt aa = 0; aa < argCount; ++aa )
                {
                    void* argPtr = decodeOperandPtr<void>(frame, &ip);
                    argPtrs[aa] = argPtr;
                }

                void* dest = decodeOperandPtr<void>(frame, &ip);

                char* bufferData = *(char**)argPtrs[0];
                uint32_t index = *(uint32_t*)argPtrs[1];

                auto size = type.getSize();
                char* elementData = bufferData + index*size;

                *(void**)dest = elementData;
            }
            break;


        case kIROp_Call:
            {
                VMType type = decodeType(frame, &ip);
                UInt operandCount = decodeUInt(&ip);

                // First operand is the callee function
                VMFunc* func = decodeOperand<VMFunc*>(frame, &ip);

                // Okay, we need to create a frame to prepare the call
                VMFrame* newFrame = createFrame(func);
                newFrame->parent = frame;

                // Remaining arguments should populate the
                // first N registers of the callee
                UInt argCount = operandCount - 1;
                for( UInt aa = 0; aa < argCount; ++aa )
                {
                    void* argPtr = decodeOperandPtr<void>(frame, &ip);
                    void* regPtr = getRegPtrImpl(newFrame, aa);

                    VMType regType = func->regs[aa].type;
                    memcpy(regPtr, argPtr, regType.getSize());
                }

                // Note that we do *not* try to read off
                // the destination operand, and instead
                // leave that to be done during the
                // return sequence.
                //

                // Save the IP we were using in teh current function.
                //
                frame->ip = ip;

                // Now switch over to the callee:
                //
                frame = newFrame;
                ip = newFrame->ip;
            }
            break;

        case kIROp_ReturnVoid:
            {
                // Easy case: just jump to the parent frame,
                // without having to worry about operands.
                VMFrame* newFrame = frame->parent;
                vmThread->frame = newFrame;

                // HACK: we need to know when we are done.
                // TODO: We should probably have the bottom
                // of the stack for a thread always point
                // to a special bytecode sequence that
                // forces a `yield` op that can handle
                // the exit from the interpreter, rather
                // than always take a branch here.
                if (!newFrame)
                    return;

                frame = newFrame;
                ip = frame->ip;

            }
            break;

        case kIROp_ReturnVal:
            {
                VMType instType = decodeType(frame, &ip);
                /*UInt argCount =*/ decodeUInt(&ip);
                void* argPtr = decodeOperandPtr<void>(frame, &ip);

                VMFrame* newFrame = frame->parent;
                vmThread->frame = newFrame;

                // Note: see the comments above about
                // this branch.
                if (!newFrame)
                    return;

                frame = newFrame;
                ip = frame->ip;

                auto destPtrAndType = decodeOperandPtrAndType(frame, &ip);
                void* destPtr = destPtrAndType.ptr;
                VMType type = destPtrAndType.type;

                memcpy(destPtr, argPtr, type.getSize());
            }
            break;

        case kIROp_loop:
        case kIROp_unconditionalBranch:
            {
                // For now our encoding is very regular, so we can decode without
                // knowing too much about an instruction...

                VMType type = decodeType(frame, &ip);
                UInt argCount = decodeUInt(&ip);
                Int destinationBlockIndex = decodeSInt(&ip);

                auto func = frame->func;
                auto& destinationBlock = func->bcFunc->blocks[destinationBlockIndex];

                // We may be passing arguments through to the destination
                // block, so any remaining operands of the branch instruction
                // need to be used to fill in the parameter registers of
                // the destination block.

                UInt paramCount = destinationBlock.paramCount;

                // There might be additional operands, because some branches
                // also include information on merge points and break/continue labels.
                UInt remainingArgCount = argCount - 1;
                assert(remainingArgCount >= paramCount);

                UInt extraArgCount = remainingArgCount - paramCount;

                for (UInt ee = 0; ee < extraArgCount; ++ee)
                {
                    decodeOperandPtr<void>(frame, &ip);
                }

                auto baseRegIndex = destinationBlock.params - func->bcFunc->regs;
                for( UInt pp = 0; pp < paramCount; ++pp )
                {
                    auto regIndex = baseRegIndex + pp;

                    void* argPtr = decodeOperandPtr<void>(frame, &ip);
                    void* regPtr = getRegPtrImpl(frame, regIndex);

                    VMType regType = func->regs[regIndex].type;
                    memcpy(regPtr, argPtr, regType.getSize());
                }

                // Now simply jump to the destination block.
                //
                ip = destinationBlock.code;
            }
            break;

        case kIROp_ifElse:
        case kIROp_conditionalBranch:
            {
                // For now our encoding is very regular, so we can decode without
                // knowing too much about an instruction...

                VMType type = decodeType(frame, &ip);
                UInt argCount = decodeUInt(&ip);
                bool* condition = decodeOperandPtr<bool>(frame, &ip);
                Int trueBlockID = decodeSInt(&ip);
                Int falseBlockID = decodeSInt(&ip);
                for( UInt aa = 4; aa < argCount; ++aa )
                {
                    decodeOperandPtr<void>(frame, &ip);
                }

                Int destinationBlock = *condition ? trueBlockID : falseBlockID;

                // TODO: we need to deal with the case of
                // passing arguments to the block, which
                // means copying between the registers...
                //
                ip = frame->func->bcFunc->blocks[destinationBlock].code;
            }
            break;

        case kIROp_Greater:
            {
                // For now our encoding is very regular, so we can decode without
                // knowing too much about an instruction...

                VMType resultType = decodeType(frame, &ip);
                /*UInt argCount = */decodeUInt(&ip);
                //void* argPtrs[16] = { 0 };
                auto leftOpnd = decodeOperandPtrAndType(frame, &ip);
                auto type = leftOpnd.type;
                auto leftPtr = leftOpnd.ptr;
                void* rightPtr = decodeOperandPtr<void>(frame, &ip);

                bool* destPtr = decodeOperandPtr<bool>(frame, &ip);

                switch (type.impl->op)
                {
                case kIROp_IntType:
                    *destPtr = *(int32_t*)leftPtr > *(int32_t*)rightPtr;
                    break;

                default:
                    SLANG_UNEXPECTED("comparison op case");
                    break;
                }
            }
            break;

        case kIROp_Mul:
            {
                VMType type = decodeType(frame, &ip);
                /*UInt argCount = */decodeUInt(&ip);
                void* leftPtr = decodeOperandPtr<void>(frame, &ip);
                void* rightPtr = decodeOperandPtr<void>(frame, &ip);

                void* destPtr = decodeOperandPtr<void>(frame, &ip);

                switch (type.impl->op)
                {
                case kIROp_IntType:
                    *(int32_t*)destPtr = *(int32_t*)leftPtr * *(int32_t*)rightPtr;
                    break;

                default:
                    SLANG_UNEXPECTED("comparison op case");
                    break;
                }
            }
            break;

        case kIROp_Sub:
            {
                VMType type = decodeType(frame, &ip);
                /*UInt argCount = */decodeUInt(&ip);
                void* leftPtr = decodeOperandPtr<void>(frame, &ip);
                void* rightPtr = decodeOperandPtr<void>(frame, &ip);

                void* destPtr = decodeOperandPtr<void>(frame, &ip);

                switch (type.impl->op)
                {
                case kIROp_IntType:
                    *(int32_t*)destPtr = *(int32_t*)leftPtr - *(int32_t*)rightPtr;
                    break;

                default:
                    SLANG_UNEXPECTED("comparison op case");
                    break;
                }
            }
            break;

        default:
            {
                // For now our encoding is very regular, so we can decode without
                // knowing too much about an instruction...

                VMType type = decodeType(frame, &ip);
                UInt argCount = decodeUInt(&ip);
                void* argPtrs[16] = { 0 };
                for( UInt aa = 0; aa < argCount; ++aa )
                {
                    void* argPtr = decodeOperandPtr<void>(frame, &ip);
                    argPtrs[aa] = argPtr;
                }

                SLANG_UNEXPECTED("unknown bytecode op");
                return;
            }
            break;
        }
    }

}




SLANG_API void SlangVMThread_resume(
    SlangVMThread*  thread)
{
    Slang::resumeThread(
        (Slang::VMThread*)  thread);
}

} // namespace Slang

SLANG_API SlangVM* SlangVM_create()
{
    return (SlangVM*) Slang::createVM();
}

SLANG_API SlangVMModule* SlangVMModule_load(
    SlangVM*    vm,
    void const* bytecode,
    size_t      bytecodeSize)
{
    return (SlangVMModule*) Slang::loadVMModuleInstance(
        (Slang::VM*) vm,
        bytecode,
        bytecodeSize);
}

SLANG_API void* SlangVMModule_findGlobalSymbolPtr(
    SlangVMModule*  module,
    char const*     name)
{
    return (SlangVMFunc*) Slang::findGlobalSymbolPtr(
        (Slang::VMModule*) module,
        name);
}

SLANG_API SlangVMThread* SlangVMThread_create(
    SlangVM*    vm)
{
    return (SlangVMThread*)Slang::createThread(
        (Slang::VM*) vm);
}

SLANG_API void SlangVMThread_beginCall(
    SlangVMThread*  thread,
    SlangVMFunc*    func)
{
    Slang::beginCall(
        (Slang::VMThread*)  thread,
        (Slang::VMFunc*)    func);
}

SLANG_API void SlangVMThread_setArg(
    SlangVMThread*  thread,
    SlangUInt       argIndex,
    void const*     data,
    size_t          size)
{
    Slang::setArg(
        (Slang::VMThread*)  thread,
        argIndex,
        data,
        size);
}

SLANG_API void SlangVMThread_resume(
    SlangVMThread*  thread)
{
    Slang::resumeThread(
        (Slang::VMThread*)  thread);
}

