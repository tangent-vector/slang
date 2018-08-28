// bc-impl.h
#pragma once

#include "bytecode.h"
#include "name.h"

namespace Slang {

struct BCWriter;

struct BCWriteOffsetBase
{
    List<uint8_t>*  data        = nullptr;
    size_t          baseOffset  = 0;
    size_t          offset      = 0;

    BCWriteOffsetBase(
        List<uint8_t>*  data,
        size_t          baseOffset,
        size_t          offset)
        : data(data)
        , baseOffset(baseOffset)
        , offset(offset)
    {}

    BCWriteOffsetBase(BCWriter* writer, size_t offset);
};

inline size_t operator-(BCWriteOffsetBase const& left, BCWriteOffsetBase const& right)
{
    return (left.baseOffset + left.offset) - (right.baseOffset + right.offset);
}

template<typename T>
struct BCWriteOffset : BCWriteOffsetBase
{
    BCWriteOffset(BCWriter* writer, size_t offset)
        : BCWriteOffsetBase(writer, offset)
    {}

    BCWriteOffset(
        List<uint8_t>*  data,
        size_t          baseOffset,
        size_t          offset)
        : BCWriteOffsetBase(data, baseOffset, offset)
    {}


    T* getPtr() const;

    T* operator->() const { return getPtr(); }

    T& operator[](int index) const
    {
        return getPtr()[index];
    }

    template<typename U>
    BCWriteOffset<U> as() const
    {
        return BCWriteOffset<U>(data, baseOffset, offset);
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
        while(outData->Count() & (alignment - 1))
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

    void setBaseOffset(BCWriteOffsetBase const& offset)
    {
        baseOffset = offset.baseOffset + offset.offset;
    }

    template<typename T>
    BCWriteOffset<T> reserveBase(UInt count = 1)
    {
        auto offset = reserve<T>(count);
        setBaseOffset(offset);
        return offset;
    }
};

inline BCWriteOffsetBase::BCWriteOffsetBase(BCWriter* writer, size_t offset)
    : data(writer->outData)
    , baseOffset(writer->baseOffset)
    , offset(offset)
{}

template<typename T>
T* BCWriteOffset<T>::getPtr() const
{
    return (T*)(data->Buffer() + baseOffset + offset);
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

    /// Sections logically nested under this one
    List<RefPtr<BCSectionBuilder>> childSections;

    virtual void writeData(BCWriter& writer) = 0;

    void addChildSection(String const& childName, BCSectionBuilder* section)
    {
        section->name = childName;
        childSections.Add(section);
    }

    template<typename T>
    RefPtr<T> addChildSection(String const& childName)
    {
        RefPtr<T> section = new T();
        addChildSection(childName, section);
        return section;
    }
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

    BCStringTableBuilder()
    {
        type = SLANG_BC_SECTION_TYPE_STRING_TABLE;
    }

    UInt addString(String const& text)
    {
        UInt index = 0;
        if(mapStringToIndex.TryGetValue(text, index))
            return index;

        index = entries.Count();
        entries.Add(text);
        return index;
    }

    Int addString(Name* nameObj)
    {
        if(!nameObj) return -1;
        return addString(nameObj->text);
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

//

void generateBytecodeSectionsForAST(
    CompileRequest* compileRequest,
    BCFileBuilder*  fileBuilder);

void generateBytecodeSectionsForIR(
    CompileRequest* compileRequest,
    BCFileBuilder*  fileBuilder);


}
