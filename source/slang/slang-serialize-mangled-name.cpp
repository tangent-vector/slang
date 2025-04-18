// slang-serialize-mangled-name.cpp
#include "slang-serialize-mangled-name.h"

namespace Slang
{

//
// MangledNameTableReader
//


MangledNameTableReader::MangledNameTableReader() {}

void MangledNameTableReader::init(RiffContainer::ListChunk* chunk)
{
    if (!chunk)
        return;

    _mangledNameEntries =
        chunk->findDataArray<Binary::MangledNameTableEntry>(SerialBinary::kExportTableItemsFourCC);
    _mangledNameData = chunk->findDataArray<char>(SerialBinary::kDataFourCC);
    _hashTableBuckets = chunk->findDataArray<UInt32>(SerialBinary::kHashTableBucketsFourCC);
}

// Search for an entry matching the given `mangledName`.
//
bool MangledNameTableReader::findEntry(UnownedStringSlice mangledName, Int& outDeclID)
{
    // We want to try and find a declaration exported by `moduleDecl`
    // that has a mangled name matching `mangledName`.
    //
    // We start by looking `mangledName` up in the serialized hash
    // table stored with the serialized data.
    //
    // The first step in performing that lookup is to hash `mangledName`.
    //
    Binary::HashCode hashCode = Binary::hash(mangledName);

    Count bucketCount = _hashTableBuckets.getCount();

    Index bucketIndex = hashCode % bucketCount;
    for (;;)
    {
        auto entryIndex = _hashTableBuckets[bucketIndex];

        // If we run into an empty bucket while proping, then we
        // know the name we are searching for is not in the table.
        //
        if (entryIndex == 0)
            return false;

        // Otherwise, we need to check if the mangled name of
        // the entry at `entryIndex` matches the `mangledName`
        // we are searching for.
        //
        if (!_doesEntryMatchMangledName(entryIndex, mangledName, hashCode))
        {
            // If there isn't a match, then we need to continue
            // our search.
            //
            // The serialization step used simple linear probing
            // to build the hash table, so we follow suit here.
            //
            // TODO(tfoley): change this to be at least a little
            // more clever.
            //
            bucketIndex++;
            if (bucketIndex == bucketCount)
                bucketIndex = 0;
            continue;
        }

        // If we found a match for the mangled name, then we
        // have identified the declaration we want to return.
        //
        auto declID = _mangledNameEntries[entryIndex].declID;
        outDeclID = declID;
        return true;
    }
}


Binary::HashCode MangledNameTableReader::_getEntryMangledNameHash(UInt32 entryIndex)
{
    SLANG_ASSERT(entryIndex > 0 && entryIndex < _mangledNameEntries.getCount());

    auto& entry = _mangledNameEntries[entryIndex];
    return entry.hash;
}

size_t MangledNameTableReader::_getEntryMangledNameSize(UInt32 entryIndex)
{
    SLANG_ASSERT(entryIndex > 0 && entryIndex < _mangledNameEntries.getCount());

    // The name is stored in a slightly complicated representation for
    // compactness (because mangled names tend to be verbose and
    // have a lot of duplication).
    //
    // As a result, we need to reference both the entry being
    // queried and its immediate predecessor to compute the size.
    //
    // Note that the assertion at the top of this function intentionally
    // disallows a zero value for `entryIndex`, so we can be
    // sure that there is always a predecessor in the table.
    //
    auto& entry = _mangledNameEntries[entryIndex];
    auto& prevEntry = _mangledNameEntries[entryIndex - 1];

    // Each entry directly stores the size in bytes of the prefix
    // that it shares with the parent entry, so that part of
    // the size is easily computed.
    //
    size_t prefixSize = entry.sizeInBytesOfPrefixSharedWithParentEntry;

    // The entry only stores the *end* offset of the data for its
    // suffix, so to compute the size of the suffix we need to
    // exploit the fact that the data for this entry's suffix comes
    // right after the data for the suffix of the preceding entry.
    //
    size_t suffixSize = entry.endOffsetOfOfDataForSuffix - prevEntry.endOffsetOfOfDataForSuffix;

    return prefixSize + suffixSize;
}

bool MangledNameTableReader::_doesEntryMatchMangledName(
    UInt32 exportEntryIndexToMatch,
    UnownedStringSlice keyMangledName,
    Binary::HashCode keyMangledNameHashCode)
{
    // The entries in the export table store their mangled names
    // in a slightly complicated fashion, in order to remove some
    // of the duplication that is common with mangled name strings.
    //
    // As a result, checking if an entry matches a given key is
    // more complicated than just doing a simple `strcmp()`, and
    // we'd rather not go to the trouble of reifying the mangled
    // name of an entry just to do the comparison.

    // We start with a simple check to see if the hash code
    // of the key matches the hash code of the entry.
    //
    // If the hashes don't match, we know the names don't match.
    //
    if (keyMangledNameHashCode != _getEntryMangledNameHash(exportEntryIndexToMatch))
        return false;

    // The next early-out test is to check if the size (in bytes)
    // of the key string matches the size of the mangled
    // name for the entry.
    //
    // Note that the size of the mangled name string for the entry
    // is computed without ever reifying that string in memory.
    //
    size_t keySize = keyMangledName.getLength();
    if (keySize != _getEntryMangledNameSize(exportEntryIndexToMatch))
        return false;

    // If both the hash and the size match, it is now time
    // to start comparing the actual bytes of the key string
    // against the mangled name of the entry.
    //
    // We will still work hard to make sure that we don't have
    // to reify the name of the entry in memory (since that would
    // require allocation).
    //
    // We will perform the comparison by working backword through
    // the key string.
    //
    auto keyEnd = keyMangledName.end();
    auto entryIndex = exportEntryIndexToMatch;
    for (;;)
    {
        SLANG_ASSERT(entryIndex > 0);

        // Using an entry and its preceding entry, we can compute
        // the size of the unique suffix for that entry, as well
        // as the offset for the data of that suffix.
        //
        auto& entry = _mangledNameEntries[entryIndex];
        auto& prevEntry = _mangledNameEntries[entryIndex - 1];

        size_t entrySuffixOffset = prevEntry.endOffsetOfOfDataForSuffix;

        // We could compute the size of the suffix for the
        // chosen `entry` using its own `endOffsetOfDataForSuffix` field
        // and the `entrySuffixOffset` we just computed, but we don't
        // actually care about any part of the suffix of `entry` beyond
        // the size of our key.
        //
        // Thus, we can use the key string itself to determine how
        // much of the suffix we need to compare to.
        //
        // Note: here we are relying on the work done during serialization
        // to optimize the parent links. We know that at each step along
        // the parent chain there must be at least *some* bytes of the
        // suffix worth comparing against, or else that entry in the
        // chain would have been skipped as a parent/ancestor.
        //
        size_t prefixSize = entry.sizeInBytesOfPrefixSharedWithParentEntry;
        SLANG_ASSERT(keySize > prefixSize);
        size_t suffixSize = keySize - prefixSize;

        SLANG_ASSERT(suffixSize <= (entry.endOffsetOfOfDataForSuffix - entrySuffixOffset));
        auto entrySuffix = UnownedStringSlice(&_mangledNameData[entrySuffixOffset], suffixSize);

        SLANG_ASSERT(suffixSize <= keySize);
        auto keySuffix = UnownedStringSlice(keyEnd - suffixSize, suffixSize);

        // If the two names differ in this suffix, then
        // we do not have a match.
        //
        if (keySuffix != entrySuffix)
            return false;

        // At this point, if we've run out of data to
        // compare, then we know that we have a complete
        // match.
        //
        if (prefixSize == 0)
        {
            SLANG_ASSERT(keySize == suffixSize);
            return true;
        }

        // Otherwise, we need to continue the search
        // using the part of the key before the suffix,
        // and the parent of the current entry.
        //
        keySize -= suffixSize;
        keyEnd -= suffixSize;
        entryIndex = entry.parentEntryIndex;
        SLANG_ASSERT(entryIndex != 0);
    }
}


//
// MangledNameTableWriter
//

static Count calcSharedPrefixSize(UnownedStringSlice const& left, UnownedStringSlice const& right)
{
    auto leftSize = left.getLength();
    auto rightSize = right.getLength();
    auto sharedSize = std::min(leftSize, rightSize);

    Count prefixSize = 0;
    while (prefixSize < sharedSize)
    {
        if (left[prefixSize] != right[prefixSize])
            break;
        prefixSize++;
    }
    return prefixSize;
}

MangledNameTableWriter::MangledNameTableWriter(Encoder* encoder)
    : _encoder(encoder)
{
    _addPlaceholderEntryInfo();
}

void MangledNameTableWriter::_addPlaceholderEntryInfo()
{
    EntryInfo placeholderEntry;
    _entryInfos.add(placeholderEntry);
}

void MangledNameTableWriter::addEntry(UnownedStringSlice const& mangledName, Int declID)
{
    EntryInfo entry;
    entry.mangledName = mangledName;
    entry.mangledNameHash = Binary::hash(mangledName);
    entry.declID = UInt32(declID);

    _entryInfos.add(entry);
}

bool MangledNameTableWriter::EntryInfo::operator<(EntryInfo const& that) const
{
    return lexicographicCompare(this->mangledName, that.mangledName) < 0;
}

void MangledNameTableWriter::finishWriting()
{
    auto entryCount = _entryInfos.getCount();
    _entryInfos.sort();

    SLANG_ASSERT(_entryInfos[0].mangledName == UnownedStringSlice());

    // TODO(tfoley): we should try to be more careful about how
    // we set the number of buckets here, so that we don't waste
    // too much space, but also don't have too many collisions.
    //
    Count bucketCount = 2 * entryCount;

    // We are using a zero value to represent an empty bucket,
    // and to make sure that we can do so, we will also be
    // reserving the first entry in the serialized export table
    // to be an empty/placeholder entry.
    //
    auto buckets = List<UInt32>::makeRepeated(0, bucketCount);

    // Each export will start its search at a bucket that
    // is based on the hash of its mangled name.
    //
    for (Index entryIndex = 1; entryIndex < entryCount; ++entryIndex)
    {
        auto& entryInfo = _entryInfos[entryIndex];

        Index bucketIndex = entryInfo.mangledNameHash % bucketCount;

        for (;;)
        {
            if (buckets[bucketIndex] == 0)
            {
                buckets[bucketIndex] = UInt32(entryIndex);
                break;
            }

            bucketIndex++;
            if (bucketIndex == bucketCount)
                bucketIndex = 0;
        }
    }

    // Begin actually writing the data...
    //

    auto mangledNameEntriesChunk = _encoder->addDataChunk(SerialBinary::kExportTableItemsFourCC);
    auto mangledNameDataChunk = _encoder->addDataChunk(SerialBinary::kDataFourCC);
    auto hashTableBucketsChunk = _encoder->addDataChunk(SerialBinary::kHashTableBucketsFourCC);

    // The entry at index zero in the `mangledNameEntriesChunk` will
    // be a placeholder, with an empty mangled name.
    //
    // As discussed earlier in this function, reserving that entry allows
    // us to use a zero index to represent an empty bucket in the hash
    // table, but there is another subtle reason why we use that
    // representation.
    //
    // Each entry in the array of exports will store the information about
    // its mangled name in a way that depends on the previous entry.
    // Notably:
    //
    // * Each entry stores the size in bytes of the prefix that its
    //   mangled name shares with the previous entry.
    //
    // * Each entry stores the *end* offset of the data for the additional
    //   suffix of its mangled name. The starting offset of that data can
    //   simply be read using the end offset of the previous entry.
    //
    // Storing a placeholder first entry can help keep things simpler
    // when dealing with the boundary conditions, since every *valid*
    // entry is guaranteed to have a preceding entry that is stored.

    List<Binary::MangledNameTableEntry> entries;

    {
        Binary::MangledNameTableEntry placeholderFirstEntry;
        placeholderFirstEntry.parentEntryIndex = 0;
        placeholderFirstEntry.sizeInBytesOfPrefixSharedWithParentEntry = 0;
        placeholderFirstEntry.endOffsetOfOfDataForSuffix = 0;
        placeholderFirstEntry.hash = 0;
        placeholderFirstEntry.declID = 0;

        entries.add(placeholderFirstEntry);
    }

    UnownedStringSlice prevEntryMangledName;
    Count dataSize = 0;
    for (Index entryIndex = 1; entryIndex < entryCount; ++entryIndex)
    {
        auto& entryInfo = _entryInfos[entryIndex];
        auto mangledName = entryInfo.mangledName;

        // This new entry will only write out the part of its mangled
        // name after any prefix it shares with the previous entry.
        //
        auto prefixSize = calcSharedPrefixSize(prevEntryMangledName, mangledName);
        auto suffixSize = mangledName.getLength() - prefixSize;
        mangledNameDataChunk.writeData(mangledName.end() - suffixSize, suffixSize);

        dataSize += suffixSize;
        auto endOffset = dataSize;

        // We need to compute the index of the "parent" entry for this
        // one, which will be an entry it shares a prefix of size `prefixSize`.
        //
        // Because of how we computed `prefixSize` above, it is clear that
        // the previous entry could serve as a parent, but if we consider
        // these entries as a kind of tree structure, we'd ideally like to
        // keep the tree as shallow as possible, so we will start with
        // the previous entry and then try to follow parent links until
        // we identify the earliest entry that could be a valid parent.
        //
        auto parentEntryIndex = entryIndex - 1;
        for (;;)
        {
            auto& parentEntry = entries[parentEntryIndex];
            if (parentEntry.sizeInBytesOfPrefixSharedWithParentEntry < prefixSize)
            {
                // The given `parentEntry` is as far up the tree as we
                // can go while still sharing the common prefix. We
                // know this because our entry has `prefixSize` bytes in
                // common with the `parentEntry`, but the `parentEntry`
                // has *fewer* bytes in commong with its parent.
                //
                break;
            }
            if (parentEntryIndex == 0)
            {
                // We can't go any further up the tree than the root,
                // so if we make it all the way to our placeholder
                // entry, then we stop our search.
                break;
            }

            parentEntryIndex = parentEntry.parentEntryIndex;
        }

        Binary::MangledNameTableEntry entry;
        entry.parentEntryIndex = UInt32(parentEntryIndex);
        entry.sizeInBytesOfPrefixSharedWithParentEntry = UInt32(prefixSize);
        entry.endOffsetOfOfDataForSuffix = UInt32(endOffset);
        entry.hash = entryInfo.mangledNameHash;
        entry.declID = entryInfo.declID;

        entries.add(entry);

        prevEntryMangledName = mangledName;
    }

    for (auto entry : entries)
    {
        mangledNameEntriesChunk.writeData(&entry, sizeof(entry));
    }

    for (auto bucket : buckets)
    {
        hashTableBucketsChunk.writeData(&bucket, sizeof(bucket));
    }
}

} // namespace Slang
