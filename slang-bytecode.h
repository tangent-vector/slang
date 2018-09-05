#ifndef SLANG_BYTECODE_H
#define SLANG_BYTECODE_H

#ifndef  SLANG_BC_NO_INTTYPES
#include <inttypes.h>
#endif

#ifndef  SLANG_BC_NO_STDDEF
#include <stddef.h>
#endif

/** \file slang-bytecode.h

The Slang "bytecode" module format is used to serialize compiled
code and related metadata.
*/

typedef uint32_t SlangBCOffset;

#define SLANG_BC_MAGIC  0xFF, 'S', 'l', 'a', 'n', 'g', ' ', 'C', 'o', 'd', 'e', 0x00, 0x0D, 0x0A, 0x1A, 0x0A

/** Header for a binary module file.
*/
struct SlangBCFileHeader
{
    /* Pre-defined "magic" that must appear at the start of a binary module.
    */
    unsigned char magic[16];

    /* Major version number for the file format of this file.
    */
    uint16_t majorVersion;

    /* Minor version number for the file format of this file.
    */
    uint16_t minorVersion;

    /* Offset in bytes from the start of the module to the section table.
    */
    uint32_t sectionTableOffset;

    /* Number of entries in the section table.
    */
    uint32_t sectionTableEntryCount;

    /* Size of each entry in the section table.
    */
    uint32_t sectionTableEntrySize;

    /* Index of the section in the section table that provides string names for section.
    */
    uint32_t sectionNameStringTableIndex;
};

/** Types of sections.
*/
enum
{
    SLANG_BC_SECTION_TYPE_NONE = 0,

    SLANG_BC_SECTION_TYPE_STRING_TABLE,
    SLANG_BC_SECTION_TYPE_REFLECTION,
    SLANG_BC_SECTION_TYPE_LAYOUT,
    SLANG_BC_SECTION_TYPE_IR,
};

/** Flags for sections.
*/
enum
{
    SLANG_BC_SECTION_FLAGS_NONE = 0,
};

/** An entry in the section table.
*/
struct SlangBCSectionTableEntry
{
    /* Index of the string that represents this sections name in the section name string table. */
    uint32_t nameIndex;

    /* Offset in bytes of this section's data from the start of the file. */
    uint32_t offsetInFile;

    /* Size in bytes of this section's data. */
    uint32_t size;

    /* Required alignment of the section, in bytes */
    uint32_t alignment;

    /* First "child" section of this section, or -1 for no children. */
    int32_t firstChild;

    /* Next sibling section (same parent) of this section, or -1 for end of list */
    int32_t nextSibling;

    /* General type of section, from the `SLANG_BC_SECTION_TYPE_*` enumerants. */
    uint16_t type;

    /* Flags for this section, from the `SLANG_BC_SECTION_FLAG_*` enumerants */
    uint16_t flags;
};

/** Header for a string-table section.
*/
struct SlangBCStringTableSectionHeader
{
    /* Offset in bytes from start of section to the table of entries. */
    uint32_t entryTableOffset;

    /* The number of string entries in the table. */
    uint32_t entryCount;

    /* The size of each string-table entry. */
    uint32_t entrySize;
};

/** An entry in a string-table section.
*/
struct SlangBCStringTableEntry
{
    /* Offset in bytes from start of section to the beginning of the given string. */
    uint32_t offset;

    /* Size in bytes of the string's data. */
    uint32_t size;
};

/** Header for a reflection section.
*/
struct SlangBCReflectionSectonHeader
{
    uint32_t entryTableOffset;
    uint32_t entryCount;
    uint32_t entrySize;

    uint32_t importTableOffset;
    uint32_t importCount;

    // TODO: eventually we want to support tools that can strip out reflection
    // nodes not required for runtime reflection, or those that are not required
    // to expose the public interface of a module to subsequent compilation.
    //
    // These might be best handled by sorting the entries so that the first K
    // entries are those required for a sepecific purpose, and entries after
    // that point can be safely stripped. Doing that would require some careful
    // thought about layout, however, so we put it off for now.
    //
//    uint32_t publicEntryCount;
//    uint32_t reflectionEntryCount;
};

struct SlangBCReflectionEntry
{
    /* Offset in bytes from start of section. */
    uint32_t offset;
};

/** Tag values for reflection nodes.
*/
enum
{
    // 0x0xxxxxxx : reserved
    SLANG_BC_REFLECTION_TAG_NONE = 0,

    // 0x1xxxxxxx : declarations (non-container, non-type)

    // 0x10xxxxxx : variable declarations
    SLANG_BC_REFLECTION_TAG_VAR                 = 0x10000000,
    SLANG_BC_REFLECTION_TAG_PARAM               = 0x10100000,
    SLANG_BC_REFLECTION_TAG_FIELD               = 0x10200000,
    SLANG_BC_REFLECTION_TAG_GENERIC_VALUE_PARAM = 0x10300000,

    // 0x2xxxxxxx : declarations (container, non-type)
    SLANG_BC_REFLECTION_TAG_DECL_BEGIN  = 0x20000000,
    SLANG_BC_REFLECTION_TAG_DECL_END    = 0x50000000,

    // 0x200xxxxx : generic declarations
    SLANG_BC_REFLECTION_TAG_GENERIC             = 0x20000000,

    // 0x201xxxxx : module declarations
    SLANG_BC_REFLECTION_TAG_MODULE              = 0x20100000,
    SLANG_BC_REFLECTION_TAG_IMPORTED_MODULE     = 0x20110000,


    // 0x21xxxxxx : callable (function-like) declarations
    SLANG_BC_REFLECTION_TAG_FUNC                = 0x21000000,
    SLANG_BC_REFLECTION_TAG_CONSTRUCTOR         = 0x21100000,


    // 0x3xxxxxxx : type declarations (container)

    // 0x30xxxxxx : aggregate type declarations (struct-like)
    SLANG_BC_REFLECTION_TAG_STRUCT              = 0x30000000,
    SLANG_BC_REFLECTION_TAG_INTERFACE           = 0x30100000,

    // 0x4xxxxxxx : type declarations (non-container)

    // 0x40xxxxxx : basic scalar types


    // 0x5xxxxxxx : types (non-declaration)

    // 0x500xxxxx : basic scalar types
    SLANG_BC_REFLECTION_TAG_VOID_TYPE               = 0x50000000,

    // 0x51xxxxxx : array types
    SLANG_BC_REFLECTION_TAG_ARRAY_TYPE              = 0x51000000,
    SLANG_BC_REFLECTION_TAG_UNBOUNDED_ARRAY_TYPE    = 0x51100000,

    // 0x7xxxxxx : specializations

    // 0x701xxxxx : vector types
    SLANG_BC_REFLECTION_TAG_VECTOR_TYPE             = 0x70100000,

    // 0x702xxxxx : matrix types
    SLANG_BC_REFLECTION_TAG_MATRIX_TYPE             = 0x70200000,


    SLANG_BC_REFLECTION_TAG_GENERIC_SPECIALIZATION  = 0x71000000,

    // 

};

struct SlangBCReflectionNode
{
    uint32_t tag;

    /* remaining data is determined by tag */
};

struct SlangBCReflectionDecl
{
    SlangBCReflectionNode asNode;
    int32_t parent;
    int32_t name;
};

struct SlangBCReflectionVarNode
{
    SlangBCReflectionDecl asDecl;
    int32_t typeID;
};

struct SlangBCReflectionContainerNode
{
    SlangBCReflectionDecl asDecl;

    uint32_t memberCount;
    uint32_t memberIndicesOffset;
};

struct SlangBCReflectionFuncNode
{
    SlangBCReflectionContainerNode asContainer;
    int32_t resultTypeID;
};

/** Opcodes for bytecode functions.
*/
enum
{
    SLANG_BC_OP_NOP,

};

typedef uint8_t SlangBCCode;

struct SlangBCIRSectionHeader
{
    uint32_t nodeCount;
    uint32_t nodeTableOffset;
    uint32_t nodeTableEntrySize;

    // TODO: need storage for constant data
};

struct SlangBCIRNodeTableEntry
{
    uint32_t nodeOffset;
};

struct SlangBCIRNode
{
    uint32_t op;
    int32_t typeID;

    uint32_t codeSize;
    uint32_t codeOffset;

    uint32_t registerCount;
    uint32_t registersOffset;

    uint32_t blockCount;
    uint32_t blocksOffset;

    uint32_t upValueCount;
    uint32_t upValuesOffset;

    uint32_t nodeCount;
    uint32_t nodesOffset;
};

struct SlangBCRegister
{
    uint32_t op;
    int32_t typeID;
    uint32_t previousVarIndexPlusOne;
};

struct SlangBCUpValue
{
    uint32_t id;
};

struct SlangBCBlock
{
    uint32_t codeOffset;
    uint32_t paramCount;
    uint32_t firstParamIndex;
};

#endif
