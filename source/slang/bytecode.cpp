#include "bytecode.h"

// Implementation of the Slang bytecode (BC)
// (most notably including conversion from IR to BC)

#include "bc-impl.h"
#include "compiler.h"

namespace Slang {

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

    generateBytecodeSectionsForAST(compileReq, fileBuilder);

    generateBytecodeSectionsForIR(compileReq, fileBuilder);

    generateBytecodeContainer(fileBuilder, compileReq);
}

} // namespace Slang
