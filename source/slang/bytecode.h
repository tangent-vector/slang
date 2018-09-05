// bytecode.h
#ifndef SLANG_BYTECODE_H_INCLUDED
#define SLANG_BYTECODE_H_INCLUDED

// The file defines services for working with the Slang "bytecode" format,
// which is defined in the public `slang-bytecode.h` header.
//

#include "../core/basic.h"

#include "../../slang-bytecode.h"

namespace Slang
{

// The `slang-bytecode.h` file is intended to be used by clients who want
// to parse/inspect a compiled module without needing to depend on the
// Slang compiler implementation. As such, all of its definitions are
// plain C types without namespaces, etc.
//
// For convenience inside of the Slang compiler codebase, we will map
// the external names over to internal `typedef`s that drop the `Slang`
// prefix.
//

typedef SlangBCFileHeader               BCFileHeader;
typedef SlangBCSectionTableEntry        BCSectionTableEntry;
typedef SlangBCStringTableSectionHeader BCStringTableSectionHeader;
typedef SlangBCStringTableEntry         BCStringTableEntry;

typedef SlangBCReflectionSectonHeader   BCReflectionSectionHeader;
typedef SlangBCReflectionEntry          BCReflectionEntry;
typedef SlangBCReflectionNode           BCReflectionNode;
typedef SlangBCCode                     BCOp;
typedef SlangBCIRNode                   BCFunc;
typedef SlangBCRegister                 BCReg;
typedef SlangBCUpValue                  BCConst;
typedef SlangBCBlock                    BCBlock;

inline SlangBCSectionTableEntry& getSectionTableEntry(
    SlangBCFileHeader*  header,
    UInt                index)
{
    return *(SlangBCSectionTableEntry*)((char*)header
        + header->sectionTableOffset
        + index*header->sectionTableEntrySize);
}

inline void* getSectionData(
    SlangBCFileHeader*              header,
    SlangBCSectionTableEntry const& entry)
{
    return (char*)header + entry.offsetInFile;
}

inline void* getSectionData(
    SlangBCFileHeader*  header,
    int32_t             index)
{
    return getSectionData(
        header,
        getSectionTableEntry(header, index));
}

//

inline SlangBCReflectionNode* getNode(SlangBCReflectionSectonHeader const* bcHeader, Int index)
{
    if( index >= 0 )
    {
        auto entry = (SlangBCReflectionEntry*)((char*)bcHeader
            + bcHeader->entryTableOffset
            + index * bcHeader->entrySize);

        return (SlangBCReflectionNode*)((char*)bcHeader
            + entry->offset);
    }
    else
    {
        index = ~index;

        auto entry = (SlangBCReflectionEntry*)((char*)bcHeader
            + bcHeader->importTableOffset
            + index * bcHeader->entrySize);

        return (SlangBCReflectionNode*)((char*)bcHeader
            + entry->offset);
    }

}

//

inline SlangBCBlock* getBlock(SlangBCIRNode const* bcFunc, UInt index)
{
    auto blocks = (SlangBCBlock*)((char*)bcFunc
        + bcFunc->blocksOffset);
    return blocks + index;
}

inline SlangBCCode* getCode(SlangBCIRNode const* bcFunc)
{
    return (SlangBCCode*)((char*)bcFunc
        + bcFunc->codeOffset);
}

inline SlangBCRegister* getRegister(SlangBCIRNode const* bcFunc, UInt index)
{
    return (SlangBCRegister*)((char*)bcFunc
        + bcFunc->registersOffset
        + index * sizeof(SlangBCRegister));
}

inline SlangBCUpValue* getUpValue(SlangBCIRNode const* bcFunc, UInt index)
{
    return (SlangBCUpValue*)((char*)bcFunc
        + bcFunc->upValuesOffset
        + index * sizeof(SlangBCUpValue));
}

inline UnownedTerminatedStringSlice getString(
    SlangBCStringTableSectionHeader const*  bcStringTable,
    Int                                     index)
{
    auto empty = UnownedTerminatedStringSlice("");
    if(!bcStringTable) return empty;
    if(index < 0 || index > (Int)bcStringTable->entryCount) return empty;

    auto entry = (SlangBCStringTableEntry*)((char*)bcStringTable
        + bcStringTable->entryTableOffset
        + index * bcStringTable->entrySize);

    char const* begin = ((char*)bcStringTable
        + entry->offset);
    char const* end = begin + entry->size;

    return UnownedTerminatedStringSlice(begin, end);
}

int32_t findSection(
    SlangBCFileHeader*  bcFile,
    uint32_t            sectionType,
    int32_t             firstSection = 0);

int32_t findChildSection(
    SlangBCFileHeader*  bcFile,
    int32_t             parentSection,
    uint32_t            sectionType);


//




//


class CompileRequest;
void generateBytecodeForCompileRequest(
    CompileRequest* compileReq);

}








#endif
