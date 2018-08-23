#include "bytecode.h"

// Implementation of the Slang bytecode (BC)
// (most notably including conversion from IR to BC)

#include "compiler.h"
#include "ir.h"
#include "ir-insts.h"
#include "lower-to-ir.h"

// Needed for AST->IR
#include "visitor.h"

namespace Slang
{

struct BCWriter;

template<typename T>
struct BCWriteOffset
{
    BCWriter* writer = 0;
    size_t offset = 0;

    BCWriteOffset(BCWriter* writer, size_t offset)
        : writer(writer)
        , offset(offset)
    {}

    T* getPtr();

    T* operator->() { return getPtr(); }

    T& operator[](int index)
    {
        return getPtr()[index];
    }
};

struct BCWriter
{
    List<uint8_t>* outData;
    size_t baseOffset;

    BCWriteOffset<char> write(void const* data, size_t size)
    {
        size_t startOffset = tell();

        char const* cursor = (char const*)data;
        for(size_t ii = 0; ii < size; ++ii)
            outData->Add(*cursor++);

        return BCWriteOffset<char>(this, startOffset);
    }

    void padToAlignment(int alignment)
    {
        while(tell() & (alignment - 1))
            outData->Add(0);
    }

    size_t tell()
    {
        return outData->Count() - baseOffset;
    }

    size_t reserveImpl(size_t size, int alignment)
    {
        padToAlignment(alignment);
        size_t startOffset = tell();
        for(size_t ii = 0; ii < size; ++ii)
            outData->Add(0);
        return startOffset;
    }

    template<typename T>
    BCWriteOffset<T> reserve(UInt count = 1)
    {
        return BCWriteOffset<T>(this, reserveImpl(sizeof(T)*count, SLANG_ALIGN_OF(T)));
    }
};

template<typename T>
T* BCWriteOffset<T>::getPtr()
{
    return (T*)(writer->outData->Buffer() + writer->baseOffset + offset);
}

struct BCSectionBuilder : RefObject
{
    /// The name of the section, as it will be serialized
    String name;

    /// The type of section, from the `SLANG_BC_SECTION_TYPE_*` values
    uint16_t type = SLANG_BC_SECTION_TYPE_NONE;

    /// Flags for this section, from the `SLANG_BC_SECTION_FLAG_*` values.
    uint16_t flags = 0;

    /// Required alignment in bytes.
    uint32_t alignment = 8;

    virtual void writeData(BCWriter& writer) = 0;
};

struct BCSimpleSectionBuilder : BCSectionBuilder
{
    /// The raw binary data of the section.
    List<uint8_t> data;
    uint8_t* getData() { return data.Buffer(); }

    void appendRaw(void const* inData, size_t size)
    {
        char const* cursor = (char const*)inData;
        for( size_t ii = 0; ii < size; ++ii )
        {
            data.Add(*cursor++);
        }
    }

    template<typename T>
    void append(T const& value)
    {
        appendRaw(&value, sizeof(value));
    }
};

struct BCStringTableBuilder : BCSectionBuilder
{
    Dictionary<String, UInt> mapStringToIndex;
    List<String> entries;

    UInt addString(String const& text)
    {
        UInt index = 0;
        if(mapStringToIndex.TryGetValue(text, index))
            return index;

        index = entries.Count();
        entries.Add(text);
        return index;
    }

    void writeData(BCWriter& writer) SLANG_OVERRIDE
    {
        UInt entryCount = entries.Count();

        auto bcHeader = writer.reserve<SlangBCStringTableSectionHeader>();
        auto bcEntries = writer.reserve<SlangBCStringTableEntry>(entryCount);

        bcHeader->entryTableOffset = bcEntries.offset;
        bcHeader->entryCount = entryCount;
        bcHeader->entrySize = sizeof(SlangBCStringTableEntry);

        for(UInt ii = 0; ii < entryCount; ++ii)
        {
            String text = entries[ii];
            size_t entrySize = text.Length();

            auto bcText = writer.write(text.Buffer(), entrySize + 1);

            bcEntries[ii].offset = bcText.offset;
            bcEntries[ii].size = entrySize;
        }
    }
};


struct BCFileBuilder
{
    List<RefPtr<BCSectionBuilder>> sections;

    void addSection(String const& name, BCSectionBuilder* section)
    {
        section->name = name;
        sections.Add(section);
    }

    template<typename T>
    RefPtr<T> addSection(String const& name)
    {
        RefPtr<T> section = new T();
        addSection(name, section);
        return section;
    }
};

// AST->BC translation

struct SharedASTToBCContext
{
};

struct ASTToBCContext
{
};

//struct ASTToBCVisitor : TypeVisitor<int32_t>
//{
//};

// IR->BC translation

typedef int32_t LocalID;

struct IRToBCContext
{
    /// A "parent" context for the encoding of an outer container value
    IRToBCContext*  parent;

    /// The IR value that is being lowered into bytecode here
    IRParentInst*   irValue;

    /// Linear stream of bytecode being emitted for `irValue`
    List<uint8_t>   currentBytecode;

    /// "Registers" allocatred for local values.
    List<BCReg>     registers;

    /// "Constants" pulled in from a the parent
    List<BCConst>   constants;

    /// Basic blocks for the current value
    List<BCBlock>   blocks;

    /// A map from an instruction to its local ID.
    ///
    /// The local ID of an instruction is used when referencing it
    /// as an operand within the current context.
    Dictionary<IRInst*, LocalID> mapInstToLocalID;

};

void encodeUInt8(
    IRToBCContext*  context,
    uint8_t         value)
{
    context->currentBytecode.Add(value);
}

void encodeUInt(
    IRToBCContext*  context,
    UInt             value)
{
    if( value < 128 )
    {
        encodeUInt8(context, (uint8_t)value);
        return;
    }

    uint8_t bytes[16];
    UInt count = 0;

    for(;;)
    {
        UInt index = count++;
        bytes[index] = value & 0x7F;
        value = value >> 7;
        if (!value)
            break;

        bytes[index] |= 0x80;
    }

    UInt index = count;
    while (index--)
    {
        encodeUInt8(context, bytes[index]);
    }
}

void encodeSInt(
    IRToBCContext*  context,
    Int             value)
{
    UInt uValue;
    if( value < 0 )
    {
        uValue = (~UInt(value) << 1) | 1;
    }
    else
    {
        uValue = UInt(value) << 1;
    }

    encodeUInt(context, uValue);
}

/// Get the local ID that will represent an IR value in the current context.
LocalID getLocalID(
    IRToBCContext*  context,
    IRInst*         value)
{
    // If we've encountered this value before, it will be stored in
    // our map of local IDs, and we will extract it from there.
    LocalID localID = 0;
    if( context->mapInstToLocalID.TryGetValue(value, localID) )
    {
        return localID;
    }

    // If we haven't seen this value before, then what we do next
    // will depend on whether we are the top-most context or not.
    if(auto parent = context->parent)
    {
        // If we have a parent context, then we will ask the parent
        // context to sort out what its local ID for `value` would
        // be, and then use that to initialize a "constant" for
        // use inside of this value.

        LocalID localIDInParent = getLocalID(parent, value);

        BCConst bcConst;
        bcConst.id = localIDInParent;

        UInt constantIndex = context->constants.Count();
        localID = ~constantIndex;

        context->mapInstToLocalID.Add(value, localID);

        return localID;
    }
    else
    {
        // If this is the global scope, then we should have already
        // asigned IDs to every instruction, and this indicates
        // that something very bad has happened.
        SLANG_UNEXPECTED("global scope did not generate ID");
        UNREACHABLE_RETURN(0);
    }
}

void encodeOperand(
    IRToBCContext*  context,
    IRInst*             operand)
{
    auto id = getLocalID(context, operand);
    encodeSInt(context, id);
}

bool opHasResult(IRInst* inst)
{
    // Certain operations can be referenced even though they
    // don't have a type at the level that our IR type
    // system traks.
    //
    switch(inst->op)
    {
    case kIROp_TypeKind:
    case kIROp_GenericKind:
        return true;

    default:
        break;
    }

    // Otherwise, any instruction without a type must
    // have no result, because we interpret a null
    // type as being equivalent to the `Void` type.
    //
    // TODO: We should really use an explicit `Void`
    // in all cases rather than rely on this hack.
    //
    auto type = inst->getDataType();
    if (!type) return false;

    // As a bit of a hack right now, we need to check whether
    // the function returns the distinguished `Void` type,
    // since that is conceptually the same as "not returning
    // a value."
    if(type->op == kIROp_VoidType)
        return false;

    return true;
}

void generateBytecodeForInst(
    IRToBCContext*  context,
    IRInst*         inst)
{
    // We are generating bytecode for a local instruction
    // inside a function or similar context.
    switch( inst->op )
    {
    default:
        {
            // As a default case, we will assume that bytecode ops
            // and the IR's internal opcodes are the same, and then
            // encode the necessary extra info:
            //

            auto operandCount = inst->getOperandCount();
            encodeUInt(context, inst->op);
            encodeOperand(context, inst->getDataType());
            encodeUInt(context, operandCount);
            for( UInt aa = 0; aa < operandCount; ++aa )
            {
                encodeOperand(context, inst->getOperand(aa));
            }

            if (!opHasResult(inst))
            {
                // This instructions has no type, so don't emit a destination
            }
            else
            {
                // The instruction can be encoded
                // as its own operand for the destination.
                encodeOperand(context, inst);
            }
        }
        break;

    case kIROp_TypeKind:
    case kIROp_GenericKind:
    case kIROp_ReturnVoid:
        // Trivial encoding here
        encodeUInt(context, inst->op);
        break;

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

#if 0
    case kIROp_Func:
        {
            encodeUInt(context, inst->op);

            // We just want to encode the ID for the function
            // symbol data, and then do the rest on the decode side
            UInt nestedID = 0;
            context->mapInstToNestedID.TryGetValue(inst, nestedID);
            encodeUInt(context, nestedID);

            // destination:
            encodeOperand(context, inst);
        }
        break;
#endif

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
}

#if 0
BytecodeGenerationPtr<BCType> emitBCType(
    BytecodeGenerationContext*              context,
    IRType*                                 type,
    IROp                                    op,
    BytecodeGenerationPtr<uint8_t> const*   args,
    UInt                                    argCount)
{
    UInt size = sizeof(BCType)
        + argCount * sizeof(BCPtr<void>);

    BytecodeGenerationPtr<uint8_t> bcAllocation(
        context->shared,
        allocateRaw(context, size, alignof(BCPtr<void>)));

    BytecodeGenerationPtr<BCType> bcType = bcAllocation.bitCast<BCType>();
    auto bcArgs = (bcType + 1).bitCast<BCPtr<uint8_t>>();

    bcType->op = op;
    bcType->argCount = (uint32_t)argCount;

    for(UInt aa = 0; aa < argCount; ++aa)
    {
        bcArgs[aa] = args[aa];
    }

    UInt id = context->shared->bcTypes.Count();
    context->shared->mapTypeToID.Add(type, id);
    context->shared->bcTypes.Add(bcType);
    bcType->id = (uint32_t)id;

    return bcType;
}

BytecodeGenerationPtr<BCType> emitBCVarArgType(
    BytecodeGenerationContext*              context,
    IRType*                                 type,
    IROp                                    op,
    List<BytecodeGenerationPtr<uint8_t>>    args)
{
    return emitBCType(context, type, op, args.Buffer(), args.Count());
}

BytecodeGenerationPtr<BCType> emitBCType(
    BytecodeGenerationContext*  context,
    IRType*                     type,
    IROp                        op)
{
    return emitBCType(context, type, op, nullptr, 0);
}

BytecodeGenerationPtr<BCType> emitBCType(
    BytecodeGenerationContext*  context,
    IRType*                     type);

// Emit a `BCType` representation for the given `Type`
BytecodeGenerationPtr<BCType> emitBCTypeImpl(
    BytecodeGenerationContext*  context,
    IRType*                     type)
{
    // A NULL type is interpreted as equivalent to `Void` for now.
    if( !type )
    {
        return emitBCType(context, type, kIROp_VoidType);
    }

    List<BytecodeGenerationPtr<uint8_t>> operands;
    UInt operandCount = type->getOperandCount();
    for (UInt ii = 0; ii < operandCount; ++ii)
    {
        operands.Add(emitBCType(context, (IRType*) type->getOperand(ii)).bitCast<uint8_t>());
    }
    return emitBCVarArgType(context, type, type->op, operands);
}

BytecodeGenerationPtr<BCType> emitBCType(
    BytecodeGenerationContext*  context,
    IRType*                     type)
{
    auto canonical = type->getCanonicalType();
    UInt id = 0;
    if(context->shared->mapTypeToID.TryGetValue(canonical, id))
    {
        return context->shared->bcTypes[id];
    }

    BytecodeGenerationPtr<BCType> bcType = emitBCTypeImpl(context, canonical);
    return bcType;
}

uint32_t getTypeID(
    BytecodeGenerationContext*  context,
    IRType*                     type)
{
    // We have a type, and we need to emit it (if we haven't
    // already) and return its index in the global type table.
    BytecodeGenerationPtr<BCType> bcType = emitBCType(context, type);
    return bcType->id;
}

uint32_t getTypeIDForGlobalSymbol(
    BytecodeGenerationContext*  context,
    IRInst*                     inst)
{
    auto type = inst->getDataType();
    if(!type)
        return 0;

    return getTypeID(context, type);
}

BytecodeGenerationPtr<char> allocateString(
    BytecodeGenerationContext*  context,
    char const*                 data,
    UInt                        size)
{
    BytecodeGenerationPtr<char> ptr = allocateArray<char>(context, size + 1);
    memcpy(ptr.getPtr(), data, size);
    return ptr;
}

BytecodeGenerationPtr<char> allocateString(
    BytecodeGenerationContext*  context,
    String const&               str)
{
    return allocateString(context,
        str.Buffer(),
        str.Length());
}

BytecodeGenerationPtr<char> allocateString(
    BytecodeGenerationContext*  context,
    Name*                       name)
{
    return allocateString(context, name->text);
}

BytecodeGenerationPtr<char> tryGenerateNameForSymbol(
    BytecodeGenerationContext*      context,
    IRGlobalValue*                  inst)
{
    // TODO: this is gross, and the IR should probably have
    // a more direct means of querying a name for a symbol.
    if (auto highLevelDeclDecoration = inst->findDecoration<IRHighLevelDeclDecoration>())
    {
        auto decl = highLevelDeclDecoration->decl;
        if (auto reflectionNameMod = decl->FindModifier<ParameterGroupReflectionName>())
        {
            return allocateString(context, reflectionNameMod->name);
        }
        else if(auto name = decl->nameAndLoc.name)
        {
            return allocateString(context, name);
        }
    }

    return BytecodeGenerationPtr<char>();
}
#endif


void generateBytecodeNodeForIRParentRec(
    IRToBCContext*  parentContext,
    IRParentInst*   inst)
{
    BCFunc bcFuncStorage;
    BCFunc* bcFunc = &bcFuncStorage;

    if(auto type = inst->getFullType())
    {
        bcFunc->typeID = getLocalID(parentContext, type);
    }

    IRToBCContext contextStorage;
    IRToBCContext* context = &contextStorage;
    context->parent = parentContext;
    context->irValue = inst;

    // There are two cases of parent instructions we need to look out for.
    // The first is global values that contain code, so that their immediate
    // children are basic blocks, while the second are global values
    // that directly contain their children, and don't need to actually
    // emit any bytecode ops.

    if(auto code = as<IRGlobalValueWithCode>(inst))
    {
        // First we need to enumerate our basic blocks, so that they
        // can reference one another (basic blocks can forward reference
        // blocks that haven't been seen yet).
        //
        // Note: we allow the "IDs" of blocks to overlap with ordinary
        // "register" numbers, because there is no case where an operand
        // could be either a block or an ordinary register.
        //
        UInt blockCounter = 0;
        for( auto bb = code->getFirstBlock(); bb; bb = bb->getNextBlock() )
        {
            Int blockID = Int(blockCounter++);
            context->mapInstToLocalID.Add(bb, blockID);
        }
        UInt blockCount = blockCounter;

        // Set up the array for the bytecode representation of the blocks.
        context->blocks.SetSize(blockCount);

        // Now loop through the blocks again, and allocate the storage
        // for any parameters, variables, or registers used inside
        // each block.
        //
        // We'll count in a first pass, and then fill things in
        // using a second pass
        UInt regCounter = 0;
        blockCounter = 0;
        for( auto bb = code->getFirstBlock(); bb; bb = bb->getNextBlock() )
        {
            UInt blockID = blockCounter++;
            UInt paramCount = 0;

            for( auto ii = bb->getFirstInst(); ii; ii = ii->getNextInst() )
            {
                switch( ii->op )
                {
                default:
                    // Default behavior: if an op has a result,
                    // then it needs a register to store it.
                    if(opHasResult(ii))
                    {
                        regCounter++;
                    }
                    break;

                case kIROp_Param:
                    // A parameter always uses a register.
                    regCounter++;
                    //
                    // We also want to keep a count of the parameters themselves.
                    paramCount++;
                    break;

                case kIROp_Var:
                    // A `var` (`alloca`) node needs two registers:
                    // one to hold the actual storage, and another
                    // to hold the pointer.
                    regCounter += 2;
                    break;
                }
            }

            context->blocks[blockID].paramCount = (uint32_t)paramCount;
        }

        UInt regCount = regCounter;

        // Okay, we've counted how many registers we need for each block,
        // and now we can allocate the contiguous array we will use.
        context->registers.SetSize(regCount);
        bcFunc->registerCount = regCount;

        // Now we will loop over things again to fill in the information
        // on all these registers.
        regCounter = 0;
        blockCounter = 0;
        for( auto bb = code->getFirstBlock(); bb; bb = bb->getNextBlock() )
        {
            UInt blockID = blockCounter++;

            // Loop over the instruction in the block, to allocate registers
            // for them. The parameters of a block will always be the first
            // N instructions in the block, so they will always get the
            // first N registers in that block. Similarly, the entry block
            // is always the first block, so that the parameters of the function
            // will always be the first N registers.
            //
            context->blocks[blockID].firstParamIndex = (uint32_t)regCounter;
            for( auto ii = bb->getFirstInst(); ii; ii = ii->getNextInst() )
            {
                switch(ii->op)
                {
                default:
                    // For a parameter, or an ordinary instruction with
                    // a result, allocate it here.
                    if( opHasResult(ii) )
                    {
                        Int localID = regCounter++;
                        context->mapInstToLocalID.Add(ii, localID);

                        context->registers[localID].op = ii->op;
                        context->registers[localID].previousVarIndexPlusOne = (uint32_t)localID;
                        context->registers[localID].typeID = getLocalID(context, ii->getFullType());
                    }
                    break;

                case kIROp_Var:
                    // As handled in the earlier loop, we are
                    // allocating *two* locations for each `var`
                    // instruction. The first of these will be
                    // the actual pointer value, while the second
                    // will be the storage for the variable value.
                    {
                        Int localID = regCounter;
                        regCounter += 2;

                        context->mapInstToLocalID.Add(ii, localID);
                        context->registers[localID].op = ii->op;
                        context->registers[localID].previousVarIndexPlusOne = (uint32_t)localID;
                        context->registers[localID].typeID = getLocalID(context, ii->getFullType());

                        context->registers[localID+1].op = ii->op;
                        context->registers[localID+1].previousVarIndexPlusOne = (uint32_t)localID+1;
                        context->registers[localID+1].typeID = getLocalID(context,
                            (as<IRPtrType>(ii->getDataType()))->getValueType());
                    }
                    break;
                }
            }
        }
        assert((UInt)regCounter == regCount);

        // Now that we've allocated our blocks and our registers
        // we can go through the actual process of emitting instructions. Hooray!
        blockCounter = 0;

        // Offset of each basic block from the start of the code
        // for the current funciton.
        List<UInt> blockOffsets;
        for( auto bb = code->getFirstBlock(); bb; bb = bb->getNextBlock() )
        {
            auto blockID = blockCounter++;

            // Get local bytecode offset for current block.
            UInt codeOffset = context->currentBytecode.Count();
            context->blocks[blockID].codeOffset = codeOffset;

            for( auto ii = bb->getFirstInst(); ii; ii = ii->getNextInst() )
            {
                // What we do with each instruction depends a bit on the
                // kind of instruction it is.
                switch( ii->op )
                {
                default:
                    // For most instructions we just emit their bytecode
                    // ops directly.
                    generateBytecodeForInst(context, ii);
                    break;

                case kIROp_Param:
                    // Don't actually emit code for these, because
                    // there isn't really anything to *execute*
                    //
                    // Note that we *do* allow the `var` nodes
                    // to be executed, just because they need
                    // to set up a register with the pointer value.
                    break;
                }
            }
        }

        // Having done all the work above, we just need to actually
        // emit the symbol to the output.
    }
    else
    {
        // The case when we don't have basic blocks is simpler, because
        // we really just want to emit the child instructions as-is.
        //
        // The only thing to be careful with is that we need to
        // assing IDs to all of the instructions before we emit them,
        // especially for cases where there might be circular references.
        //
        UInt regCounter = 0;
        for( auto ii = inst->getFirstChild(); ii; ii = ii->getNextInst() )
        {
            if(!opHasResult(ii))
                continue;

            regCounter++;
        }
        UInt regCount = regCounter;

        // Okay, we've counted how many registers we need,
        // and now we can allocate the contiguous array we will use.
        context->registers.SetSize(regCount);
        bcFunc->registerCount = regCount;

        // Now we will loop over things again to fill in the information
        // on all these registers.
        regCounter = 0;
        for( auto ii = inst->getFirstChild(); ii; ii = ii->getNextInst() )
        {
            if(!opHasResult(ii))
                continue;

            Int localID = regCounter++;
            context->mapInstToLocalID.Add(ii, localID);

            context->registers[localID].op = ii->op;
            context->registers[localID].previousVarIndexPlusOne = (uint32_t)localID;

            // Note: there is an assumption here that a type must precede any instruction of that type...
            context->registers[localID].typeID = ii->getFullType() ? getLocalID(context, ii->getFullType()) : 0;
        }

        for( auto ii = inst->getFirstChild(); ii; ii = ii->getNextInst() )
        {
            generateBytecodeForInst(context, ii);
        }
    }

    // Once we've generated the "direct" code for the given `inst`,
    // we need to also emit nodes for any of its children that are
    // themselves parent instructions.
    for( auto ii = inst->getFirstChild(); ii; ii = ii->getNextInst() )
    {
        if( auto parentInst = as<IRParentInst>(ii) )
        {
            generateBytecodeNodeForIRParentRec(context, parentInst);
        }
    }
}

void generateBytecodeContainer(
    BCFileBuilder*      fileBuilder,
    CompileRequest*     compileReq)
{
    BCWriter writer;
    writer.outData = &compileReq->generatedBytecode;
    writer.baseOffset = 0;

    auto bcHeader = writer.reserve<BCFileHeader>();

    static const uint8_t kMagic[] = { SLANG_BC_MAGIC };
    memcpy(&bcHeader->magic[0], kMagic, sizeof(kMagic));
    bcHeader->majorVersion = 0;
    bcHeader->minorVersion = 0;

    // We need a section for the string table of section names.
    UInt sectionNameTableSectionIndex = fileBuilder->sections.Count();
    RefPtr<BCStringTableBuilder> sectionNameTable = new BCStringTableBuilder();
    sectionNameTable->name = ".shstrtab";

    List<RefPtr<BCSectionBuilder>> sections = fileBuilder->sections;
    sections.Add(sectionNameTable);

    UInt sectionCount = sections.Count();

    auto bcSectionTable = writer.reserve<SlangBCSectionTableEntry>(sectionCount);

    bcHeader->sectionTableOffset = bcSectionTable.offset;
    bcHeader->sectionTableEntryCount = sectionCount;
    bcHeader->sectionTableEntrySize = sizeof(SlangBCSectionTableEntry);
    bcHeader->sectionNameStringTableIndex = sectionNameTableSectionIndex;

    for(UInt ii = 0; ii < sectionCount; ++ii)
    {
        auto section = sections[ii];
        uint32_t nameIndex = sectionNameTable->addString(section->name);

        writer.padToAlignment(section->alignment);
        size_t sectionOffset = writer.tell();

        BCWriter subWriter;
        subWriter.outData = writer.outData;
        subWriter.baseOffset = sectionOffset;
        section->writeData(subWriter);

        size_t sectionSize = subWriter.tell();

        SlangBCSectionTableEntry& bcEntry = bcSectionTable[ii];
        bcEntry.nameIndex = nameIndex;
        bcEntry.offsetInFile = sectionOffset;
        bcEntry.size = sectionSize;
        bcEntry.type = section->type;
        bcEntry.flags = section->flags;
    }
}

void generateBytecodeForCompileRequest(
    CompileRequest* compileReq)
{
    BCFileBuilder fileBuilderStorage;
    BCFileBuilder* fileBuilder = &fileBuilderStorage;

    // There might be multiple translation units in the compile request,
    // each of which will map to a distinct IR-level module.
    for (auto translationUnitReq : compileReq->translationUnits)
    {
        generateBytecodeNodeForIRParentRec(nullptr, translationUnitReq->irModule->getModuleInst());
    }

    generateBytecodeContainer(fileBuilder, compileReq);
}

// TODO: Need to support IR emit at the whole-module/compile-request
// level, and not just for individual entry points.
#if 0
List<uint8_t> emitSlangIRForEntryPoint(
    EntryPointRequest*  entryPoint)
{
    auto compileRequest = entryPoint->compileRequest;
    auto irModule = lowerEntryPointToIR(
        entryPoint,
        compileRequest->layout.Ptr(),
        // TODO: we need to pick the target more carefully here
        CodeGenTarget::HLSL);

#if 0
    String irAsm = getSlangIRAssembly(irModule);
    fprintf(stderr, "%s\n", irAsm.Buffer());
#endif

    // Now we need to encode that IR into a binary format
    // for transmission/serialization/etc.

    SharedBytecodeGenerationContext sharedContext;

    BytecodeGenerationContext context;
    context.shared = &sharedContext;

    generateBytecodeStream(&context, irModule);

    return sharedContext.bytecode;
}
#endif

} // namespace Slang
