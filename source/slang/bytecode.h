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

class CompileRequest;
void generateBytecodeForCompileRequest(
    CompileRequest* compileReq);

}


#endif
