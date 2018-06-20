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
    char magic[16];

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

/* Header for a reflection section.
*/
struct SlangBCReflectionSectonHeader
{
    uint32_t entryTableOffset;
    uint32_t entryCount;
    uint32_t entrySize;

    uint32_t publicEntryCount;
    uint32_t reflectionEntryCount;
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
    SLANG_BC_REFLECTION_TAG_VAR,
    SLANG_BC_REFLECTION_TAG_PARAM,
    SLANG_BC_REFLECTION_TAG_STRUCT,
    SLANG_BC_REFLECTION_TAG_FUNC,
    SLANG_BC_REFLECTION_TAG_INTERFACE,
    SLANG_BC_REFLECTION_TAG_GENERIC,

    SLANG_BC_REFLECTION_TAG_VECTOR_TYPE,
    SLANG_BC_REFLECTION_TAG_MATRIX_TYPE,
    SLANG_BC_REFLECTION_TAG_ARRAY_TYPE,
    SLANG_BC_REFLECTION_TAG_UNBOUNDED_ARRAY_TYPE,
};

struct SlangBCReflectionNode
{
    uint32_t tag;

    /* remaining data is determined by tag */
};

struct SlangBCReflectionDecl
{
    uint32_t tag;
    int32_t parent;
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
    uint32_t nodeSize;
    uint32_t nodesOffset;

    uint32_t registerCount;
    uint32_t registerSize;
    uint32_t registersOffset;

    uint32_t blockCount;
    uint32_t blockSize;
    uint32_t blocksOffset;

    uint32_t upValueCount;
    uint32_t upValueSize;
    uint32_t upValuesOffset;

    // TODO: need storage for *true* constants
};

struct SlangBCIRNode
{
    uint32_t op;
    int32_t typeID;

    uint32_t registerCount;
    uint32_t firstRegisterIndex;

    uint32_t blockCount;
    uint32_t firstBlockIndex;

    uint32_t upValueCount;
    uint32_t firstUpValueIndex;
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
