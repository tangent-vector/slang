// slang-serialize-mangled-name.h
#ifndef SLANG_SERIALIZE_MANGLED_NAME_H
#define SLANG_SERIALIZE_MANGLED_NAME_H

#include "slang-binary.h"
#include "slang-serialize.h"

namespace Slang
{
    struct MangledNameTableReader
    {
    public:
        MangledNameTableReader();

        void init(RiffContainer::ListChunk* chunk);

        // Search for an entry matching the given `mangledName`.
        //
        bool findEntry(UnownedStringSlice mangledName, Int& outDeclID);

    private:
        ArrayView<Binary::MangledNameTableEntry> _mangledNameEntries;
        ArrayView<char> _mangledNameData;
        ArrayView<UInt32> _hashTableBuckets;

        // Get the hash of the mangled name for the export at the given `entryIndex`.
        //
        Binary::HashCode _getEntryMangledNameHash(UInt32 entryIndex);

        // Get the size in bytes of the mangled name for the export at the given `entryIndex`.
        //
        size_t _getEntryMangledNameSize(UInt32 entryIndex);

        // Check if the entry at the given `entryIndex` matches the given `mangledName`.
        //
        // `mangledNameHash` should be the hash of `mangledName`, using
        // `Slang::Binary::hash`.
        //
        bool _doesEntryMatchMangledName(
            UInt32 entryIndex,
            UnownedStringSlice mangledName,
            Binary::HashCode mangledNameHash);
    };

    struct MangledNameTableWriter
    {
    public:
        MangledNameTableWriter(
            Encoder* encoder);

        void addEntry(UnownedStringSlice const& mangledName, Int declID);

        void finishWriting();

    private:
        Encoder* _encoder;

        struct EntryInfo
        {
            UnownedStringSlice mangledName;
            Binary::HashCode mangledNameHash = 0;

            UInt32 declID = 0;

            bool operator<(EntryInfo const& that) const;
        };

        List<EntryInfo> _entryInfos;

        void _addPlaceholderEntryInfo();
    };

} // namespace Slang

#endif
