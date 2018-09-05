// main.cpp

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "slang-bytecode.h"

SlangBCSectionTableEntry const* getSectionEntry(
    SlangBCFileHeader const*    fileHeader,
    uint32_t                    index)
{
    return (SlangBCSectionTableEntry*)((char*)fileHeader
        + fileHeader->sectionTableOffset
        + index * fileHeader->sectionTableEntrySize);
}

void const* getSection(
    SlangBCFileHeader const*        fileHeader,
    SlangBCSectionTableEntry const* sectionEntry)
{
    if(!sectionEntry) return nullptr;
    return ((char*)fileHeader
        + sectionEntry->offsetInFile);
}

void const* getSection(
    SlangBCFileHeader const*    fileHeader,
    uint32_t                    index)
{
    return getSection(fileHeader,
        getSectionEntry(fileHeader, index));
}

char const* getString(
    SlangBCStringTableSectionHeader const*  stringTable,
    uint32_t                                index)
{
    if(!stringTable) return "";

    if(index < 0  || index >= stringTable->entryCount)
        return "";

    auto entry = (SlangBCStringTableEntry*)((char*)stringTable
        + stringTable->entryTableOffset
        + index * stringTable->entrySize);

    return (char const*)((char*)stringTable
        + entry->offset);
}

//

struct DumpContext
{
    SlangBCFileHeader const*                  fileHeader;
    SlangBCStringTableSectionHeader const*    sectionNameStringTable;
};

struct ASTDumpContext : DumpContext
{
    SlangBCStringTableSectionHeader const*    symbolNameStringTable;
};

SlangBCSectionTableEntry const* findChildSectionEntry(
    DumpContext*                    context,
    SlangBCSectionTableEntry const* parentSectionEntry,
    uint32_t                        sectionType)
{
    int32_t ii = parentSectionEntry->firstChild;
    while(ii >= 0)
    {
        auto childSectionEntry = getSectionEntry(context->fileHeader, ii);
        if(childSectionEntry->type == sectionType)
            return childSectionEntry;

        ii = childSectionEntry->nextSibling;
    }
    return nullptr;
}

SlangBCStringTableSectionHeader const* findStringTable(
    DumpContext*                    context,
    SlangBCSectionTableEntry const* parentSectionEntry)
{
    return (SlangBCStringTableSectionHeader*) getSection(context->fileHeader,
        findChildSectionEntry(context, parentSectionEntry, SLANG_BC_SECTION_TYPE_STRING_TABLE));
}

void dumpBinary(
    void const* data,
    size_t      size)
{
    unsigned char const* cursor = (unsigned char*) data;
    unsigned char const* end = cursor + size;

    size_t offset = 0;
    while(cursor != end)
    {
        if(offset % 8 == 0)
        {
            if(offset != 0) printf("\n");
            printf("  0x%04x:", offset);
        }

        printf(" %02x", *cursor);
        cursor++;
        offset++;
    }
    printf("\n");
}

void dumpRawSection(
    SlangBCSectionTableEntry const* entry,
    void const*                     data)
{
    dumpBinary(data, entry->size);
}

void dumpStringTableSection(
    SlangBCSectionTableEntry const* entry,
    void const*                     sectionData)
{
    auto sectionHeader = (SlangBCStringTableSectionHeader*)sectionData;
    auto entryCount = sectionHeader->entryCount;
    for(uint32_t ii = 0; ii < entryCount; ++ii)
    {
        auto string = getString(sectionHeader, ii);
        printf("  %04d: \"%s\"\n", ii, string);
    }
}

void dumpReflectionNodeCommon(
    ASTDumpContext*         context,
    int32_t                index,
    char const*             tagName,
    SlangBCReflectionNode*  node)
{
    printf("[%d] %s {", index, tagName);
}

void dumpReflectionNodeEnd()
{
    printf("}\n");
}

void dumpReflectionDeclCommon(
    ASTDumpContext*         context,
    int32_t                    index,
    char const*                 tagName,
    SlangBCReflectionDecl*      node)
{
    printf("[%d] %s \"%s\" {", index, tagName, getString(context->symbolNameStringTable, node->name));
}

void dumpReflectionVarNode(
    ASTDumpContext*         context,
    int32_t                    index,
    char const*                 tagName,
    SlangBCReflectionVarNode*   node)
{
    dumpReflectionDeclCommon(context, index, tagName, &node->asDecl);
    printf(" type:%d", node->typeID);
    dumpReflectionNodeEnd();
}

void dumpReflectionContainerNodeCommon(
    ASTDumpContext*         context,
    int32_t                index,
    char const*             tagName,
    SlangBCReflectionContainerNode*  node)
{
    dumpReflectionDeclCommon(context, index, tagName, &node->asDecl);

    printf(" children:[");
    uint32_t* childIndices = (uint32_t*)((char*)node
        + node->memberIndicesOffset);
    uint32_t memberCount = node->memberCount;
    for( uint32_t mm = 0; mm < memberCount; ++mm )
    {
        if(mm) printf(",");
        printf("%d", childIndices[mm]);
    }
    printf("]");
}

void dumpReflectionContainerNode(
    ASTDumpContext*         context,
    int32_t                index,
    char const*             tagName,
    SlangBCReflectionContainerNode*  node)
{
    dumpReflectionContainerNodeCommon(context, index, tagName, node);
    dumpReflectionNodeEnd();
}

void dumpReflectionFuncNode(
    ASTDumpContext*             context,
    int32_t                    index,
    char const*                 tagName,
    SlangBCReflectionFuncNode*  node)
{
    dumpReflectionContainerNodeCommon(context, index, tagName, &node->asContainer);

    printf(" resultType:%d", node->resultTypeID);

    dumpReflectionNodeEnd();
}

void dumpReflectionNode(
    ASTDumpContext*         context,
    int32_t                index,
    SlangBCReflectionNode*  node)
{
    switch(node->tag)
    {
    case SLANG_BC_REFLECTION_TAG_MODULE:
        dumpReflectionContainerNode(context, index, "Module", (SlangBCReflectionContainerNode*)node);
        break;

    case SLANG_BC_REFLECTION_TAG_IMPORTED_MODULE:
        dumpReflectionContainerNode(context, index, "Imported Module", (SlangBCReflectionContainerNode*)node);
        break;

    case SLANG_BC_REFLECTION_TAG_FUNC:
        dumpReflectionFuncNode(context, index, "Func", (SlangBCReflectionFuncNode*)node);
        break;

    case SLANG_BC_REFLECTION_TAG_STRUCT:
        dumpReflectionContainerNode(context, index, "Struct", (SlangBCReflectionContainerNode*)node);
        break;

    case SLANG_BC_REFLECTION_TAG_CONSTRUCTOR:
        dumpReflectionContainerNode(context, index, "Constructor", (SlangBCReflectionContainerNode*)node);
        break;

    case SLANG_BC_REFLECTION_TAG_VAR:
        dumpReflectionVarNode(context, index, "Var", (SlangBCReflectionVarNode*)node);
        break;

    case SLANG_BC_REFLECTION_TAG_PARAM:
        dumpReflectionVarNode(context, index, "Param", (SlangBCReflectionVarNode*)node);
        break;

    case SLANG_BC_REFLECTION_TAG_FIELD:
        dumpReflectionVarNode(context, index, "Field", (SlangBCReflectionVarNode*)node);
        break;

    case SLANG_BC_REFLECTION_TAG_GENERIC_VALUE_PARAM:
        dumpReflectionVarNode(context, index, "GenericValueParam", (SlangBCReflectionVarNode*)node);
        break;

    default:
        printf("[%d] { tag = 0x%x }\n", index, node->tag);
        break;
    }
}

void dumpReflectionSection(
    DumpContext*                    context,
    SlangBCSectionTableEntry const* entry,
    void const*                     sectionData)
{
    // Look for symbol name string table
    ASTDumpContext astContextStorage;
    auto astContext = &astContextStorage;
    *(DumpContext*)astContext = *context;

    astContext->symbolNameStringTable = findStringTable(context, entry);


    auto sectionHeader = (SlangBCReflectionSectonHeader*)sectionData;

    auto entryCount = sectionHeader->entryCount;
    for(uint32_t ii = 0; ii < entryCount; ++ii)
    {
        auto entry = (SlangBCReflectionEntry*)((char*)sectionHeader
            + sectionHeader->entryTableOffset
            + ii * sectionHeader->entrySize);
        auto node = (SlangBCReflectionNode*)((char*)sectionHeader
            + entry->offset);

        dumpReflectionNode(astContext, ii, node);
    }

    auto importCount = sectionHeader->importCount;
    if(importCount) printf("imports:\n");
    for(uint32_t ii = 0; ii < importCount; ++ii)
    {
        auto entry = (SlangBCReflectionEntry*)((char*)sectionHeader
            + sectionHeader->importTableOffset
            + ii * sectionHeader->entrySize);
        auto node = (SlangBCReflectionNode*)((char*)sectionHeader
            + entry->offset);

        dumpReflectionNode(astContext, (int32_t)~ii, node);
    }
}

//

void dumpIRNode(
    SlangBCIRNode const*    node,
    int                     depth)
{
#define INDENT() \
    for(int ii = 0; ii < depth; ++ii) printf("  ")


    INDENT();
    printf("node(op: %d, typeID: %d) {\n", node->op, node->typeID);

    auto regCount = node->registerCount;
    for(uint32_t rr = 0; rr < regCount; ++rr)
    {
        auto reg = (SlangBCRegister*)((char*)node
            + node->registersOffset) + rr;

        INDENT();
        printf("register(op: %d, typeID: %d, prevPlusOne: %d)\n", reg->op, reg->typeID, reg->previousVarIndexPlusOne);
    }

    auto upValueCount = node->upValueCount;
    for(uint32_t uu = 0; uu < upValueCount; ++uu)
    {
        auto upValue = (SlangBCUpValue*)((char*)node
            + node->upValuesOffset) + uu;

        INDENT();
        printf("upValue(id: %d)\n", upValue->id);
    }

    auto blockCount = node->blockCount;
    for(uint32_t bb = 0; bb < blockCount; ++bb)
    {
        auto block = (SlangBCBlock*)((char*)node
            + node->blocksOffset) + bb;

        INDENT();
        printf("block(codeOffset: 0x%x, paramCount: %d, firstParam: %d)\n",
            block->codeOffset, block->paramCount, block->firstParamIndex);
    }

    auto code = ((char*)node + node->codeOffset);

    printf("code:\n");
    dumpBinary(code, node->codeSize);

    // Now child nodes

    auto childCount = node->nodeCount;
    for(uint32_t nn = 0; nn < childCount; ++nn)
    {
        auto entry = (uint32_t*)((char*)node
            + node->nodesOffset
            + nn * sizeof(uint32_t));

        auto child = (SlangBCIRNode*)((char*)node
            + *entry);

        dumpIRNode(child, depth + 1);
    }

}

void dumpIRSection(
    SlangBCSectionTableEntry const* entry,
    void const*                     sectionData)
{
    auto sectionHeader = (SlangBCIRSectionHeader*)sectionData;
    auto entryCount = sectionHeader->nodeCount;

    for(uint32_t ii = 0; ii < entryCount; ++ii)
    {
        auto entry = (uint32_t*)((char*)sectionHeader
            + sectionHeader->nodeTableOffset
            + ii * sectionHeader->nodeTableEntrySize);
        auto node = (SlangBCIRNode*)((char*)sectionHeader
            + *entry);

        dumpIRNode(node, 0);
    }
}

void dumpSection(
    DumpContext*    context,
    int32_t         sectionIndex)
{
    auto fileHeader = context->fileHeader;
    auto stringTable = context->sectionNameStringTable;

    auto sectionEntry = getSectionEntry(fileHeader, sectionIndex);
    auto sectionData = getSection(fileHeader, sectionEntry);

    printf("\n%s:\n", getString(stringTable, sectionEntry->nameIndex));

    switch(sectionEntry->type)
    {
    case SLANG_BC_SECTION_TYPE_STRING_TABLE:
        dumpStringTableSection(sectionEntry, sectionData);
        break;

    case SLANG_BC_SECTION_TYPE_REFLECTION:
        dumpReflectionSection(context, sectionEntry, sectionData);
        break;

    case SLANG_BC_SECTION_TYPE_IR:
        dumpIRSection(sectionEntry, sectionData);
        break;

    default:
        dumpRawSection(sectionEntry, sectionData);
        break;
    }
}

void dumpFile(
    DumpContext*    context)
{
    static const unsigned char kExpectedMagic[] = { SLANG_BC_MAGIC };

    auto fileHeader = context->fileHeader;

    if(memcmp(fileHeader->magic, kExpectedMagic, sizeof(kExpectedMagic)) != 0)
    {
        fprintf(stderr, "invalid Slang BC magic\n");
        exit(1);
    }

    printf("major version = %d\n", fileHeader->majorVersion);
    printf("minor version = %d\n", fileHeader->minorVersion);

    auto stringTableSectionEntry = getSectionEntry(
        fileHeader,
        fileHeader->sectionNameStringTableIndex);
    auto stringTable = (SlangBCStringTableSectionHeader*)getSection(fileHeader, stringTableSectionEntry);
    context->sectionNameStringTable = stringTable;

    printf("\nSections:\n");
    printf("Idx\tName    \tType    \tOffset  \tSize\tAlignment\tFlags\tFirstChild\tNextSibling\n");
    auto sectionCount = fileHeader->sectionTableEntryCount;
    for(uint32_t sectionIndex = 0; sectionIndex < sectionCount; ++sectionIndex)
    {
        auto sectionEntry = getSectionEntry(fileHeader, sectionIndex);

        static const char* const kSectionTypeNames[] = {
            "NONE",
            "STRING TABLE",
            "REFLECTION",
            "LAYOUT",
            "IR",
        };
        auto sectionTypeNameCount = sizeof(kSectionTypeNames) / sizeof(kSectionTypeNames[0]);

        printf("%3d\t", sectionIndex);
        printf("%8s\t", getString(stringTable, sectionEntry->nameIndex));
        if(sectionEntry->nameIndex < sectionTypeNameCount)
        {
            printf("%8s\t", kSectionTypeNames[sectionEntry->type]);
        }
        else
        {
            printf("%8d\t", sectionEntry->type);
        }
        printf("0x%x\t", sectionEntry->offsetInFile);
        printf("0x%x\t", sectionEntry->size);
        printf("%d\t", sectionEntry->alignment);
        printf("0x%04x\t", sectionEntry->flags);

        printf("%d\t%d\n", sectionEntry->firstChild, sectionEntry->nextSibling);
    }

    // Now to dump detailed information about sections
    for(uint32_t sectionIndex = 0; sectionIndex < sectionCount; ++sectionIndex)
    {
        dumpSection(context, sectionIndex);
    }
}

//

int main(
    int     argc,
    char**  argv)
{
    char const* path = argv[1];

    FILE* file = fopen(path, "rb");
    fseek(file, 0, SEEK_END);
    size_t fileSize = ftell(file);
    fseek(file, 0, SEEK_SET);

    void* buffer = malloc(fileSize);
    fread(buffer, fileSize, 1, file);
    fclose(file);

    DumpContext context;
    context.fileHeader = (SlangBCFileHeader*)buffer;
    dumpFile(&context);

    return 0;
}
