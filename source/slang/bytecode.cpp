#include "bytecode.h"

// Implementation of the Slang bytecode (BC)
// (most notably including conversion from IR to BC)

#include "bc-impl.h"
#include "compiler.h"

namespace Slang {

//

int32_t findSection(
    SlangBCFileHeader*  bcFile,
    uint32_t            sectionType,
    int32_t             firstSection)
{
    int32_t sectionIndex = firstSection;
    while(sectionIndex >= 0)
    {
        auto& sectionEntry = getSectionTableEntry(bcFile, sectionIndex);
        if(sectionEntry.type == sectionType)
            return sectionIndex;
    }
    return -1;
}

int32_t findChildSection(
    SlangBCFileHeader*  bcFile,
    int32_t             parentSection,
    uint32_t            sectionType)
{
    return findSection(bcFile, sectionType,
        getSectionTableEntry(bcFile, parentSection).firstChild);
}


//

struct FlattenedSectionInfo
{
    RefPtr<BCSectionBuilder>    builder;
    Int                         firstChild;
    Int                         nextSibling;
};

static Int collectFlattenedSections(
    RefPtr<BCSectionBuilder>    sectionBuilder,
    List<FlattenedSectionInfo>& ioFlattenedSections);

static Int collectFlattenedSections(
    List<RefPtr<BCSectionBuilder>>  sectionBuilders,
    List<FlattenedSectionInfo>&     ioFlattenedSections)
{
    Int count = sectionBuilders.Count();

    if(count == 0)
        return -1;

    Int firstIndex = collectFlattenedSections(sectionBuilders[0], ioFlattenedSections);

    Int prevIndex = firstIndex;
    for(Int ii = 1; ii < count; ++ii)
    {
        Int index = collectFlattenedSections(sectionBuilders[ii], ioFlattenedSections);
        ioFlattenedSections[prevIndex].nextSibling = index;
        prevIndex = index;
    }

    return firstIndex;
}

static Int collectFlattenedSections(
    RefPtr<BCSectionBuilder>    sectionBuilder,
    List<FlattenedSectionInfo>& ioFlattenedSections)
{
    FlattenedSectionInfo info;
    info.builder = sectionBuilder;
    info.firstChild = -1;
    info.nextSibling = -1;

    Int index = ioFlattenedSections.Count();
    ioFlattenedSections.Add(info);

    ioFlattenedSections[index].firstChild = collectFlattenedSections(
        sectionBuilder->childSections,
        ioFlattenedSections);

    return index;
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
    RefPtr<BCStringTableBuilder> sectionNameTable = new BCStringTableBuilder();
    sectionNameTable->name = ".strings";

    List<RefPtr<BCSectionBuilder>> topLevelSections = fileBuilder->sections;
    topLevelSections.Add(sectionNameTable);

    List<FlattenedSectionInfo> flattenedSections;
    collectFlattenedSections(topLevelSections, flattenedSections);

    UInt sectionCount = flattenedSections.Count();

    // We expect our string table section to always be last in the list
    UInt sectionNameTableSectionIndex = sectionCount-1;

    auto bcSectionTable = writer.reserve<SlangBCSectionTableEntry>(sectionCount);

    bcHeader->sectionTableOffset = bcSectionTable.offset;
    bcHeader->sectionTableEntryCount = sectionCount;
    bcHeader->sectionTableEntrySize = sizeof(SlangBCSectionTableEntry);
    bcHeader->sectionNameStringTableIndex = sectionNameTableSectionIndex;

    for(UInt ii = 0; ii < sectionCount; ++ii)
    {
        auto sectionInfo = flattenedSections[ii];
        auto section = sectionInfo.builder;
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
        bcEntry.firstChild = sectionInfo.firstChild;
        bcEntry.nextSibling = sectionInfo.nextSibling;
    }
}

void generateBytecodeForCompileRequest(
    CompileRequest* compileReq)
{
    BCFileBuilder fileBuilderStorage;
    BCFileBuilder* fileBuilder = &fileBuilderStorage;

    generateBytecodeSectionsForAST(compileReq, fileBuilder);

    generateBytecodeSectionsForIR(compileReq, fileBuilder);

    generateBytecodeContainer(fileBuilder, compileReq);
}

} // namespace Slang
