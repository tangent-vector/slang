// slang-serialize-ast.cpp
#include "slang-serialize-ast.h"

#include "slang-ast-dispatch.h"
#include "slang-binary.h"
#include "slang-check.h"
#include "slang-compiler.h"
#include "slang-diagnostics.h"
#include "slang-mangle.h"
#include "slang-serialize-mangled-name.h"


// Enable to turn on some basic logging that shows how
// many declarations from a given module have ended up
// being loaded (useful when debugging).
//
#define SLANG_DEBUG_ON_DEMAND_LOADING_STATS 0

namespace Slang
{
// TODO(tfoley): have the parser export this, or a utility function
// for initializing a `SyntaxDecl` in the common case.
//
NodeBase* parseSimpleSyntax(Parser* parser, void* userData);

struct StringTableReader
{
public:
    StringTableReader() {}

    void init(RiffContainer::ListChunk* chunk)
    {
        _entries =
            chunk->findDataArray<Binary::StringTableEntry>(SerialBinary::kStringTableItemsFourCC);
        _data = chunk->findDataArray<char>(SerialBinary::kStringTableDataFourCC);
    }

    UnownedTerminatedStringSlice getString(Index index)
    {
        SLANG_ASSERT(index > 0);

        auto& entry = _entries[index];
        auto& prevEntry = _entries[index - 1];

        auto beginOffset = prevEntry.endOffsetOfData;
        auto endOffset = entry.endOffsetOfData - 1;

        return UnownedTerminatedStringSlice(&_data[beginOffset], &_data[endOffset]);
    }

private:
    ArrayView<Binary::StringTableEntry> _entries;
    ArrayView<char> _data;
};

struct StringTableWriter
{
public:
    StringTableWriter() {}

    void init(Encoder* encoder)
    {
        Encoder::WithObject withStringTable(encoder, SerialBinary::kStringTableFourCc);

        _entriesChunk = encoder->addDataChunk(SerialBinary::kStringTableItemsFourCC);
        _dataChunk = encoder->addDataChunk(SerialBinary::kStringTableDataFourCC);

        Binary::StringTableEntry entry;
        entry.endOffsetOfData = 0;
        _addEntry(entry);
    }

    void finishWriting()
    {
        // Nothing to do.
    }

    UInt32 getStringIndex(UnownedStringSlice const& text)
    {
        if (auto found = _mapStringToIndex.tryGetValue(text))
            return *found;

        return _addString(text);
    }

private:
    RiffDataChunkBuilder _entriesChunk;
    RiffDataChunkBuilder _dataChunk;

    UInt32 _entryCount = 0;
    UInt32 _dataSize = 0;

    Dictionary<UnownedStringSlice, UInt32> _mapStringToIndex;

    UInt32 _addString(UnownedStringSlice const& text)
    {
        auto endOffset = _addData(text);

        Binary::StringTableEntry entry;
        entry.endOffsetOfData = endOffset;

        auto index = _addEntry(entry);

        _mapStringToIndex.add(text, index);

        return index;
    }

    UInt32 _addData(UnownedStringSlice const& text)
    {
        _dataChunk.writeData(text.begin(), text.getLength());

        char nulByte = 0;
        _dataChunk.writeData(&nulByte, sizeof(nulByte));

        _dataSize += UInt32(text.getLength() + 1);

        auto endOffset = _dataSize;
        return endOffset;
    }

    UInt32 _addEntry(Binary::StringTableEntry entry)
    {
        UInt32 entryIndex = _entryCount++;
        _entriesChunk.writeData(&entry, sizeof(entry));
        return entryIndex;
    }
};


struct DirectMemberDeclBucket
{
    UInt32 nameID;
    UInt32 value;
};

struct DirectMemberDeclsReader
{
public:
    DirectMemberDeclsReader(RiffContainer::Chunk* chunk)
    {
        _membersChunk = as<RiffContainer::ListChunk>(chunk);
    }

    Count getDeclCount() { return getDeclIDs().getCount(); }

    UInt32 getDeclID(Index index) { return getDeclIDs()[index]; }

    ArrayView<UInt32> getDeclIDs()
    {
        return _membersChunk->findDataArray<UInt32>(SerialBinary::kASTDirectMemberIDsFourCC);
    }

    ArrayView<UInt32> findDeclsByName(Name* keyName, StringTableReader* stringTable)
    {
        // TODO(tfoley): It should probably be enforced that lookup never
        // tries to use a null name, but that is indeed happening in some
        // of our code. Rather than try to fix all instances of that issue
        // throughout the codebase, it is simpler for this routine to just
        // be defensive.
        //
        if (!keyName)
            return ArrayView<UInt32>();

        return findDeclsByName(keyName->text.getUnownedSlice(), stringTable);
    }

    ArrayView<UInt32> findDeclsByName(UnownedStringSlice keyName, StringTableReader* stringTable)
    {
        auto keyNameHash = Binary::hash(keyName);

        auto buckets = _membersChunk->findDataArray<DirectMemberDeclBucket>(
            SerialBinary::kHashTableBucketsFourCC);
        auto bucketCount = buckets.getCount();
        if (!bucketCount)
            return ArrayView<UInt32>();

        if (keyName == "$init" && bucketCount == 2)
        {
            if (getDeclCount() == 3)
            {
                int f = 9;
            }
        }

        auto bucketIndex = keyNameHash % bucketCount;
        for (;;)
        {
            auto& bucket = buckets[bucketIndex];
            if (bucket.nameID == 0)
            {
                return ArrayView<UInt32>();
            }

            auto bucketName = stringTable->getString(bucket.nameID);
            if (bucketName == keyName)
                break;

            bucketIndex = (bucketIndex + 1) % bucketCount;
        }

        auto bucketValue = buckets[bucketIndex].value;
        if (Int32(bucketValue) > 0)
        {
            // The case where the bucket value doesn't have
            // the high bit set (looks non-negative when
            // viewed as a signed integer) is the case where
            // the value is itself the sole declaration ID
            // with that name. Thus we return an array view
            // with one element, based on the value stored
            // direclty in the bucket.
            //
            return ArrayView<UInt32>(&buckets[bucketIndex].value, 1);
        }
        else
        {
            // The case where the bucket has the high bit set
            // (looks negative, when viewed as a signed integer)
            // is the case where the value is an index into
            // the auxilliary table of runs.
            //
            auto indexInRunTable = ~bucketValue;
            auto runs =
                _membersChunk->findDataArray<UInt32>(SerialBinary::kASTDirectMemberRunsFourCC);

            auto count = runs[indexInRunTable];
            return ArrayView<UInt32>(&runs[indexInRunTable + 1], count);
        }
    }

private:
    RiffContainer::ListChunk* _membersChunk;
};

struct DirectMemberDeclsWriter
{
public:
    DirectMemberDeclsWriter(Encoder* encoder, StringTableWriter* stringTableWriter)
        : _stringTable(stringTableWriter)
    {
        // The first chunk we will generate is just about as simple
        // as can be: a sequence of 32-bit values encoding the IDs
        // of the direct members.
        //
        _memberIDsChunk = encoder->addDataChunk(SerialBinary::kASTDirectMemberIDsFourCC);
        _runsChunk = encoder->addDataChunk(SerialBinary::kASTDirectMemberRunsFourCC);
        _bucketsChunk = encoder->addDataChunk(SerialBinary::kHashTableBucketsFourCC);
    }

    StringTableWriter* _stringTable;

    RiffDataChunkBuilder _memberIDsChunk;
    RiffDataChunkBuilder _runsChunk;
    RiffDataChunkBuilder _bucketsChunk;

    struct MemberDeclsOfSameNameInfo
    {
        UInt32 nameID = 0;
        Binary::HashCode nameHash = 0;
        List<UInt32> declIDs;
        Int32 encodedID = 0;
    };

    List<MemberDeclsOfSameNameInfo> runs;
    Dictionary<Name*, Index> mapNameToRunIndex;


    void addMemberDecl(Decl* decl, Int inDeclID)
    {
        SLANG_ASSERT(inDeclID > 0);
        auto declID = UInt32(inDeclID);

        _memberIDsChunk.writeData(&declID, sizeof(declID));

        auto declName = decl->getName();
        if (declName)
        {
            Index runIndex = 0;
            if (!mapNameToRunIndex.tryGetValue(declName, runIndex))
            {
                runIndex = runs.getCount();
                runs.add({});

                mapNameToRunIndex.add(declName, runIndex);

                auto nameText = declName->text.getUnownedSlice();
                auto nameHash = Binary::hash(nameText);
                auto nameID = _stringTable->getStringIndex(nameText);


                runs[runIndex].nameID = nameID;
                runs[runIndex].nameHash = nameHash;
            }

            auto& run = runs[runIndex];
            run.declIDs.add(declID);
        }
    }

    void finishWriting()
    {
        // Just having the raw list of member IDs is a starting point,
        // but we also need to have a way to accelerate lookup based
        // on the names of the members. In particular, we want to
        // make it possible to find the member(s) that need to be
        // deserialized in response to a lookup of a name in the
        // context of the container.
        //
        // TODO(tfoley): It remains to be seen whether an accelerator
        // based on the *types* of the members is also needed.
        //
        // We will start by building up "runs" of members that have
        // the same name.
        //

        List<UInt32> encodedRuns;

        // We will be inserting the runs into a hash table,
        // based on their name, but the encoded value that
        // gets inserted for a run will depend on whether
        // it consists of only a single declaration, or
        // comprises multiple declarations.
        //
        for (auto& run : runs)
        {
            SLANG_ASSERT(run.declIDs.getCount() > 0);

            if (run.declIDs.getCount() == 1)
            {
                // If there is only a single declaration
                // with the given name, then the value
                // stored in the hash table will simply
                // be the ID of that declaration.
                //
                auto declID = run.declIDs[0];

                // We know that the ID must be positive.
                // It cannot be negative because it is
                // a member of a local declaration (and
                // local declarations get positive IDs,
                // while imports get negative IDs).
                // It cannot be zero because that ID is
                // reserved for the top-level `ModuleDecl`,
                // which cannot be a member (it has no parent).
                //
                SLANG_ASSERT(declID > 0);

                run.encodedID = Int32(declID);
            }
            else
            {
                // If there are multiple decls with the given
                // name, then we we will allocate space in
                // a table to store the runs.
                //
                auto index = encodedRuns.getCount();

                // Each run is stored as the count of the
                // entries to follow, and then the IDs of
                // the declarations in the run.
                //
                encodedRuns.add(UInt32(run.declIDs.getCount()));
                for (auto declID : run.declIDs)
                {
                    encodedRuns.add(UInt32(declID));
                }

                // The value we will write into the hash
                // table for a non-trivial run is the
                // bitwise inverse of the index into
                // the `encodedRuns` table. This encoding
                // ensures that the value is distinct
                // from the value of an empty hash-table
                // bucket (zero), as well as from the
                // singleton runs (positive declaration
                // IDs).
                //
                run.encodedID = Int32(~index);
            }
        }

        auto runCount = runs.getCount();

        auto bucketCount = runCount * 2;

        static const auto kNullNameID = UInt32(0);
        DirectMemberDeclBucket emptyBucket = {kNullNameID};
        auto buckets = List<DirectMemberDeclBucket>::makeRepeated(emptyBucket, bucketCount);

        for (Index runIndex = 0; runIndex < runCount; ++runIndex)
        {
            auto& run = runs[runIndex];
            auto hash = run.nameHash;

            auto bucketIndex = hash % bucketCount;
            for (;;)
            {
                if (buckets[bucketIndex].nameID == kNullNameID)
                {
                    break;
                }

                bucketIndex = (bucketIndex + 1) % bucketCount;
            }

            buckets[bucketIndex].nameID = run.nameID;
            buckets[bucketIndex].value = run.encodedID;
        }

        for (auto runItemValue : encodedRuns)
            _runsChunk.writeData(&runItemValue, sizeof(runItemValue));

        for (auto bucketValue : buckets)
            _bucketsChunk.writeData(&bucketValue, sizeof(bucketValue));
    }
};


struct ASTEncodingContext
{
private:
    Encoder* encoder;
    struct UnhandledCase
    {
    };

    typedef Int DeclID;
    Dictionary<Decl*, DeclID> mapDeclToID;
    List<Decl*> decls;

    List<Decl*> _builtinDeclsToRegister;

    struct ImportedDeclInfo
    {
        // The `DeclID` of the module that `decl`
        // is being imported from, or 0 in the case
        // where `decl` is itself a module.
        //
        DeclID importedFromModuleDeclID = 0;

        Decl* decl = nullptr;
    };
    List<ImportedDeclInfo> importedDecls;

    typedef Int ValID;
    Dictionary<Val*, ValID> mapValToID;
    List<Val*> vals;

    ModuleDecl* _moduleDecl = nullptr;

    SerialSourceLocWriter* _sourceLocWriter = nullptr;

public:
    ASTEncodingContext(Encoder* encoder, ModuleDecl* module, SerialSourceLocWriter* sourceLocWriter)
        : encoder(encoder), _moduleDecl(module), _sourceLocWriter(sourceLocWriter)
    {
        _stringTable.init(encoder);
    }

    template<typename T>
    void encodeASTNodeContent(T* node)
    {
        Encoder::WithObject withObject(encoder);

        ASTNodeDispatcher<T, void>::dispatch(node, [&](auto n) { _encodeDataOf(n); });
    }

    void flush()
    {
        _stringTable.finishWriting();

        auto containerChunk = encoder->getRIFFChunk();

        RiffContainer::Chunk* declChunk = nullptr;
        RiffContainer::Chunk* importedDeclChunk = nullptr;
        RiffContainer::Chunk* valChunk = nullptr;
        {
            Encoder::WithArray withList(encoder, SerialBinary::kASTDeclListFourCC);
            declChunk = encoder->getRIFFChunk();
        }
        {
            Encoder::WithArray withList(encoder, SerialBinary::kASTImportedDeclListFourCC);
            importedDeclChunk = encoder->getRIFFChunk();
        }
        {
            Encoder::WithArray withList(encoder, SerialBinary::kASTValListFourCC);
            valChunk = encoder->getRIFFChunk();
        }
        Int declIndex = 0;
        Int importedDeclIndex = 0;
        Int valIndex = 0;

        bool done = false;
        do
        {
            done = true;
            while (declIndex < decls.getCount())
            {
                done = false;
                encoder->setRIFFChunk(declChunk);
                encodeASTNodeContent(decls[declIndex++]);
            }
            while (importedDeclIndex < importedDecls.getCount())
            {
                done = false;
                encoder->setRIFFChunk(importedDeclChunk);
                encodeImportedDecl(importedDecls[importedDeclIndex++]);
            }
            while (valIndex < vals.getCount())
            {
                done = false;
                encoder->setRIFFChunk(valChunk);
                encodeASTNodeContent(vals[valIndex++]);
            }
        } while (!done);

        // RiffContainer::calcAndSetSize(containerChunk);
        encoder->setRIFFChunk(containerChunk);

        if (_builtinDeclsToRegister.getCount() != 0)
        {
            Encoder::WithArray withArray(encoder, SerialBinary::kASTBuiltinDeclListFourCC);
            for (auto decl : _builtinDeclsToRegister)
            {
                auto declID = getDeclID(decl);
                encode(declID);
            }
        }

        writeDirectMemberLookupAccelerator();

        writeExportLookupAccelerator();
    }

#if 0
    struct ExportInfo
    {
        UnownedStringSlice mangledName;
        BinaryModuleHashCode mangledNameHash;

        DeclID declID;

        bool operator<(ExportInfo const& that) const
        {
            return lexicographicCompare(
                this->mangledName,
                that.mangledName) < 0;
        }
    };
#endif

    void writeDirectMemberLookupAccelerator()
    {
        // The basic idea here is to have:
        //
        // * A hash table to map from member name strings to entries
        //
        // * For each entry, one or more ranges of decl IDs that
        //   should be considered when looking up that name
        //
        // TODO: We are at the point where there needs to be more
        // thought getting put into how declarations are organized,
        // so that we can have either all the direct members of
        // a parrent declaration contiguous *or* all the descendents
        // of a parent declaration contiguous. It seems like the
        // former is more useful: every parent declaration can
        // have a simple [begin, end) range for its direct members,
        // and the Nth direct member of the parent will simply be
        // begin+N.
        //
        // Under that model, we can use a single hash table to
        // accelerate all the lookups, instead of distinct tables
        // for each declaration (TODO: is there a point to doing
        // it that way?)
    }

    StringTableWriter _stringTableWriter;

    void writeExportLookupAccelerator()
    {
        auto module = _moduleDecl->module;
        SLANG_ASSERT(module != nullptr);

        auto exportCount = module->getExportedDeclCount();
        if (exportCount == 0)
            return;

        Encoder::WithObject withExportTableScope(encoder, SerialBinary::kASTExportsFourCC);

        MangledNameTableWriter exportTableWriter(encoder);
        for (Index exportIndex = 0; exportIndex < exportCount; ++exportIndex)
        {
            auto exportMangledName = module->getExportedDeclMangledName(exportIndex);
            auto exportDecl = module->getExportedDecl(exportIndex);
            auto exportDeclID = getDeclID(exportDecl);

            exportTableWriter.addEntry(exportMangledName, exportDeclID);
        }
        exportTableWriter.finishWriting();

#if 0
        exports.sort();

        // TODO(tfoley): we should try to be more careful about how
        // we set the number of buckets here, so that we don't waste
        // too much space, but also don't have too many collisions.
        //
        Count bucketCount = 2*exports.getCount();

        // We are using a zero value to represent an empty bucket,
        // and to make sure that we can do so, we will also be
        // reserving the first entry in the serialized export table
        // to be an empty/placeholder entry.
        //
        auto buckets = List<UInt32>::makeRepeated(0, bucketCount);

        // Each export will start its search at a bucket that
        // is based on the hash of its mangled name.
        //
        for (Index exportIndex = 0; exportIndex < exportCount; ++exportIndex)
        {
            auto& exportInfo = exports[exportIndex];

            Index bucketIndex = exportInfo.mangledNameHash % bucketCount;

            for (;;)
            {
                if (buckets[bucketIndex] == 0)
                {
                    // Note: because of our decision to use entry zero
                    // to represent an empty bucket, and to store an
                    // empty/placeholder entry at index zero in the
                    // exports table, the serialized index for an
                    // export will be one greater than its index
                    // in the `exports` list here.
                    //
                    auto exportIndexToSerialize = UInt32(exportIndex + 1);

                    buckets[bucketIndex] = exportIndexToSerialize;
                    break;
                }

                bucketIndex++;
                if (bucketIndex == bucketCount)
                    bucketIndex = 0;
            }
        }

        // Begin actually writing the data...
        //

        auto mangledNameEntriesChunk = encoder->addDataChunk(SerialBinary::kExportTableItemsFourCC);
        auto mangledNameDataChunk = encoder->addDataChunk(SerialBinary::kDataFourCC);
        auto hashTableBucketsChunk = encoder->addDataChunk(SerialBinary::kHashTableBucketsFourCC);

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

        List<BinaryModuleMangledNameEntry> entries;

        {
            BinaryModuleMangledNameEntry placeholderFirstEntry;
            placeholderFirstEntry.parentEntryIndex = 0;
            placeholderFirstEntry.sizeInBytesOfPrefixSharedWithParentEntry = 0;
            placeholderFirstEntry.endOffsetOfOfDataForSuffix = 0;
            placeholderFirstEntry.hash = 0;
            placeholderFirstEntry.declID = 0;

            entries.add(placeholderFirstEntry);
        }

        UnownedStringSlice prevEntryMangledName;
        Count dataSize = 0;
        for (Index exportIndex = 0; exportIndex < exportCount; ++exportIndex)
        {
            auto& exportInfo = exports[exportIndex];
            auto mangledName = exportInfo.mangledName;

            // This new entry will only write out the part of its mangled
            // name after any prefix it shares with the previous entry.
            //
            auto prefixSize = calcSharedPrefixSize(prevEntryMangledName, mangledName);
            auto suffixSize = mangledName.getLength() - prefixSize;
            mangledNameDataChunk.writeData(
                mangledName.end() - suffixSize,
                suffixSize);

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
            // Note: because of how we are inserting a placeholder first
            // entry in the serialized array, the *serialized* index of
            // the previous entry is actually the same as the current
            // entry's index in the `exports` array.
            //
            auto parentEntryIndex = exportIndex;
            for(;;)
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

            BinaryModuleMangledNameEntry entry;
            entry.parentEntryIndex = UInt32(parentEntryIndex);
            entry.sizeInBytesOfPrefixSharedWithParentEntry = UInt32(prefixSize);
            entry.endOffsetOfOfDataForSuffix = UInt32(endOffset);
            entry.hash = exportInfo.mangledNameHash;
            entry.declID = Int32(exportInfo.declID);

            entries.add(entry);

            prevEntryMangledName = mangledName;
        }

        for (auto entry : entries)
        {
            mangledNameEntriesChunk.writeData(
                &entry, sizeof(entry));
        }

        for (auto bucket : buckets)
        {
            hashTableBucketsChunk.writeData(
                &bucket, sizeof(bucket));
        }
#endif
    }

    ModuleDecl* findModuleForDecl(Decl* decl)
    {
        for (auto d = decl; d; d = d->parentDecl)
        {
            if (auto m = as<ModuleDecl>(d))
                return m;
        }
        return nullptr;
    }

    ModuleDecl* findModuleDeclWasImportedFrom(Decl* decl)
    {
        auto declModule = findModuleForDecl(decl);
        if (declModule == nullptr)
            return nullptr;
        if (declModule == _moduleDecl)
            return nullptr;
        return declModule;
    }

    DeclID getDeclID(Decl* decl)
    {
        SLANG_ASSERT(decl != nullptr);

        if (auto found = mapDeclToID.tryGetValue(decl))
            return *found;

        // We need to detect whether the declaration is an
        // imported one, or one from this module itself.
        //
        // Imported declarations need to be handled very
        // differently, since they'll involve resolving
        // references to those other modules, and the
        // declarations within them.
        //
        if (auto importedFromModule = findModuleDeclWasImportedFrom(decl))
        {
            DeclID importedFromModuleDeclID = 0;
            if (decl != importedFromModule)
            {
                importedFromModuleDeclID = getDeclID(importedFromModule);
            }

            DeclID id = ~importedDecls.getCount();
            mapDeclToID.add(decl, id);

            ImportedDeclInfo info;
            info.importedFromModuleDeclID = importedFromModuleDeclID;
            info.decl = decl;
            importedDecls.add(info);

            return id;
        }
        else
        {
            DeclID id = decls.getCount();
            decls.add(decl);
            mapDeclToID.add(decl, id);

            if (isBuiltinDeclThatNeedsRegistration(decl))
            {
                _builtinDeclsToRegister.add(decl);
            }

            return id;
        }
    }

    void encodePtr(Decl* decl)
    {
        DeclID id = getDeclID(decl);
        encoder->encode(id);
    }

    ValID getValID(Val* val)
    {
        SLANG_ASSERT(val != nullptr);

        if (auto found = mapValToID.tryGetValue(val))
            return *found;

        // In order to ensure that values can be fully constructed
        // from the get-go (so that they will get cached correctly),
        // we conspire to ensure that every value is preceded by
        // all of its operands.
        //
        for (auto operand : val->m_operands)
        {
            switch (operand.kind)
            {
            default:
                break;

            case ValNodeOperandKind::ValNode:
                if (auto operandNode = operand.values.nodeOperand)
                {
                    SLANG_ASSERT(as<Val>(operandNode));
                    getValID(static_cast<Val*>(operandNode));
                }
                break;

            case ValNodeOperandKind::ASTNode:
                if (auto operandNode = operand.values.nodeOperand)
                {
                    SLANG_ASSERT(as<Decl>(operandNode));
                    getDeclID(static_cast<Decl*>(operandNode));
                }
                break;
            }
        }
        auto resolved = val->resolve();
        if (resolved != val)
        {
            getValID(resolved);
        }

        ValID id = vals.getCount();
        vals.add(val);
        mapValToID.add(val, id);
        return id;
    }

    void encodePtr(Val* val)
    {
        ValID id = getValID(val);
        encoder->encode(id);
    }

    void encodeImportedDecl(ImportedDeclInfo const& info)
    {
        Encoder::WithKeyValuePair withPair(encoder);
        encode(info.importedFromModuleDeclID);
        auto decl = info.decl;
        if (auto importedModuleDecl = as<ModuleDecl>(decl))
        {
            SLANG_ASSERT(info.importedFromModuleDeclID == 0);
            encode(importedModuleDecl->getName());
        }
        else
        {
            auto mangledName = getMangledName(getCurrentASTBuilder(), decl);
            encode(mangledName);
        }
    }

    void encodePtr(Modifier* modifier) { encodeASTNodeContent(modifier); }
    void encodePtr(Expr* expr) { encodeASTNodeContent(expr); }
    void encodePtr(Stmt* stmt) { encodeASTNodeContent(stmt); }

    void encodePtr(Name* name) { encode(name->text); }

    void encodePtr(MarkupEntry* entry)
    {
        // TODO: is this case needed?
        SLANG_UNUSED(entry);
    }

    void encodePtr(DeclAssociationList* list)
    {
        // We serialize this as if it were a simple list
        // of key-value pairs because... well... that's
        // what it amounts to in practice.
        //
        Encoder::WithArray withArray(encoder);
        for (auto association : list->associations)
        {
            Encoder::WithKeyValuePair withPair(encoder);
            encode(association->kind);
            encode(association->decl);
        }
    }

    void encodePtr(CandidateExtensionList* list) { encode(list->candidateExtensions); }

    void encodePtr(WitnessTable* witnessTable)
    {
        Encoder::WithObject withObject(encoder);
        encode(witnessTable->baseType);
        encode(witnessTable->witnessedType);
        encode(witnessTable->isExtern);

        // TODO(tfoley): In theory we should be able to streamline
        // this so that we only encode the requirements that we
        // absolutely need to (which basically amounts to `associatedtype`
        // requirements where the satisfying type is part of the public
        // API of the type).
        //
        encode(witnessTable->m_requirementDictionary);
    }

    void encodeValue(RequirementWitness const& witness)
    {
        Encoder::WithKeyValuePair withPair(encoder);
        encodeEnum(witness.m_flavor);
        switch (witness.m_flavor)
        {
        case RequirementWitness::Flavor::none:
            break;

        case RequirementWitness::Flavor::declRef:
            encode(witness.m_declRef);
            break;

        case RequirementWitness::Flavor::val:
            encode(witness.m_val);
            break;

        case RequirementWitness::Flavor::witnessTable:
            encode((WitnessTable*)witness.m_obj.Ptr());
            break;
        }
    }

    void encodePtr(DiagnosticInfo* info) { encode(Int(info->id)); }

    void encodePtr(DeclBase* declBase)
    {
        if (auto decl = as<Decl>(declBase))
        {
            encodePtr(decl);
        }
        else
        {
            encodeASTNodeContent(declBase);
        }
    }

    void encodeValue(UnhandledCase);

    void encodeValue(String const& value) { encoder->encode(value); }

    void encodeValue(Token const& value)
    {
        encode(value.type);
        encode(TokenFlags(value.flags & ~TokenFlag::Name));
        encode(value.loc);
        if (value.hasContent())
            encoder->encodeString(value.getContent());
        else
            encode(nullptr);
    }

    void encodeValue(NameLoc const& value) { encode(value.name); }

    void encodeValue(SemanticVersion value) { encoder->encode(value.toInteger()); }

    void encodeValue(CapabilitySet const& value)
    {
        // While the `CapabilityTargetSets` type is a dictionary,
        // in practice each entry already embeds its own key
        // (the target atom), so we can encode this as just
        // an array of the `CapabilityTargetSet` values.
        //
        Encoder::WithArray withArray(encoder);
        for (auto pair : value.getCapabilityTargetSets())
        {
            encode(pair.second);
        }
    }

    void encodeValue(CapabilityTargetSet const& value)
    {
        Encoder::WithKeyValuePair withPair(encoder);
        encode(value.target);

        // Similar to the case for the `CapabilityTargetSets` above,
        // each `CapabilityStageSet` already includes the stage atom,
        // so we can simply encode the values from the dictionary.
        //
        Encoder::WithArray withArray(encoder);
        for (auto pair : value.shaderStageSets)
        {
            encode(pair.second);
        }
    }

    void encodeValue(CapabilityStageSet const& value)
    {
        Encoder::WithKeyValuePair withPair(encoder);
        encode(value.stage);
        encode(value.atomSet);
    }

    void encodeValue(CapabilityAtomSet const& value)
    {
        Encoder::WithArray withArray(encoder);
        for (auto rawAtom : value)
        {
            encode(CapabilityAtom(rawAtom));
        }
    }

    template<typename T>
    void encodeValue(std::optional<T> const& value)
    {
        if (value)
            encodeValue(*value);
        else
            encoder->encode(nullptr);
    }

    void encodeValue(SyntaxClass<NodeBase> const& value) { encode(value.getTag()); }

    template<typename T>
    void encodeValue(DeclRef<T> const& value)
    {
        encode((DeclRefBase*)value);
    }

    void encodeValue(ValNodeOperand value)
    {
        Encoder::WithKeyValuePair withPair(encoder);

        encodeEnum(value.kind);
        switch (value.kind)
        {
        case ValNodeOperandKind::ConstantValue:
            encode(value.values.intOperand);
            break;

        case ValNodeOperandKind::ValNode:
            encode(static_cast<Val*>(value.values.nodeOperand));
            break;

        case ValNodeOperandKind::ASTNode:
            {
                if (auto decl = as<Decl>(value.values.nodeOperand))
                {
                    encode(decl);
                }
                else
                {
                    SLANG_UNEXPECTED("AST node operand of `Val` was expected to be a `Decl`");
                }
            }
            break;
        }
    }

    void encodeValue(TypeExp value) { encode(value.type); }

    void encodeValue(QualType value)
    {
        Encoder::WithObject withObject(encoder);
        encode(value.type);
        encode(value.isLeftValue);
        encode(value.hasReadOnlyOnTarget);
        encode(value.isWriteOnly);
    }

    void encodeValue(MatrixCoord value)
    {
        Encoder::WithObject withObject(encoder);
        encode(value.row);
        encode(value.col);
    }

    void encodeValue(SPIRVAsmOperand::Flavor const& value) { encodeEnum(value); }

    void encodeValue(SPIRVAsmOperand const& value)
    {
        Encoder::WithObject withObject(encoder);
        encode(value.flavor);
        encode(value.token);
        encode(value.expr);
        encode(value.bitwiseOrWith);
        encode(value.knownValue);
        encode(value.wrapInId);
        encode(value.type);
    }

    void encodeValue(SPIRVAsmInst const& value)
    {
        Encoder::WithObject withObject(encoder);
        encode(value.opcode);
        encode(value.operands);
    }


    template<typename T, typename = std::enable_if_t<std::is_same_v<T, bool>>>
    void encodeValue(T value)
    {
        encoder->encodeBool(value);
    }

    void encodeValue(Int32 value) { encoder->encode(value); }
    void encodeValue(UInt32 value) { encoder->encode(value); }
    void encodeValue(Int64 value) { encoder->encode(value); }
    void encodeValue(UInt64 value) { encoder->encode(value); }
    void encodeValue(float value) { encoder->encode(value); }
    void encodeValue(double value) { encoder->encode(value); }

    void encodeValue(uint8_t value) { encoder->encode(UInt32(value)); }

    void encodeValue(nullptr_t) { encoder->encode(nullptr); }

    template<typename T>
    void encodeEnum(T value)
    {
        encoder->encode(Int32(value));
    }

    void encodeValue(DeclVisibility value) { encodeEnum(value); }
    void encodeValue(BaseType value) { encodeEnum(value); }
    void encodeValue(BuiltinRequirementKind value) { encodeEnum(value); }
    void encodeValue(ASTNodeType value) { encodeEnum(value); }
    void encodeValue(ImageFormat value) { encodeEnum(value); }
    void encodeValue(TypeTag value) { encodeEnum(value); }
    void encodeValue(TryClauseType value) { encodeEnum(value); }
    void encodeValue(CapabilityAtom value) { encodeEnum(value); }
    void encodeValue(DeclAssociationKind value) { encodeEnum(value); }
    void encodeValue(TokenType value) { encodeEnum(value); }

    void encodeValue(SourceLoc value)
    {
        if (!_sourceLocWriter)
        {
            encoder->encode(nullptr);
        }
        else
        {
            auto intermediate = _sourceLocWriter->addSourceLoc(value);
            encoder->encode(intermediate);
        }
    }

    template<typename T>
    void encodeValue(T const* ptr)
    {
        if (!ptr)
        {
            encoder->encode(nullptr);
        }
        else
        {
            encodePtr(const_cast<T*>(ptr));
        }
    }

    template<typename T>
    void encodeValue(RefPtr<T> const& ptr)
    {
        if (!ptr)
        {
            encoder->encode(nullptr);
        }
        else
        {
            encodePtr(ptr.Ptr());
        }
    }

    void encodeValue(Modifiers const& modifiers)
    {
        Encoder::WithArray withArray(encoder);
        for (auto m : const_cast<Modifiers&>(modifiers))
        {
            encode(m);
        }
    }

    StringTableWriter _stringTable;

    void encodeValue(ContainerDeclMembers const& value)
    {
        Encoder::WithObject withMembersChunk(encoder, SerialBinary::kASTDirectMembersChunkFourCC);

        // Our task here is to encode the direct member list
        // of a container declaration.
        //
        // At the most basic this is simply a list of declarations.
        //
        // TODO(tfoley): This could be an ideal place to filter
        // the members down to just the ones that might
        // actually need to be serialized (e.g., because
        // they are public).
        //
        auto& directMemberDecls = value._get();

        if (directMemberDecls.getCount() > 0)
        {
            if (auto extensionDecl = as<ExtensionDecl>(directMemberDecls[0]->parentDecl))
            {
                if (auto declRefType = as<DeclRefType>(extensionDecl->targetType))
                {
                    if (declRefType->getDeclRef().getName()->text == "matrix")
                    {
                        bool found = false;

                        for (auto d : directMemberDecls)
                        {
                            auto ctor = as<ConstructorDecl>(d);
                            if (!ctor)
                                continue;

                            if (ctor->getMembersOfType<ParamDecl>().getCount() == 16)
                            {
                                found = true;
                                break;
                            }
                        }

                        if (found)
                        {
                            int f = 9;
                        }
                    }
                }
            }
        }


        DirectMemberDeclsWriter writer(encoder, &_stringTable);
        for (auto directMemberDecl : directMemberDecls)
        {
            auto declID = getDeclID(directMemberDecl);
            writer.addMemberDecl(directMemberDecl, declID);
        }

        if (value._getTransparentMemberCount())
        {
            Encoder::WithArray withTransparentMembersArray(
                encoder,
                SerialBinary::kASTTransparentMembersFourCC);
            for (auto transparentMemberDecl : value._getTransparentMembers())
                encode(transparentMemberDecl);
        }

        writer.finishWriting();
    }

    template<typename T, int N>
    void encodeValue(ShortList<T, N> const& array)
    {
        Encoder::WithArray withArray(encoder);
        for (auto element : array)
        {
            encode(element);
        }
    }


    template<typename T>
    void encode(List<T> const& array)
    {
        Encoder::WithArray withArray(encoder);
        for (auto element : array)
        {
            encode(element);
        }
    }

    template<typename T, size_t N>
    void encode(T const (&array)[N])
    {
        Encoder::WithArray withArray(encoder);
        for (auto element : array)
        {
            encode(element);
        }
    }

    template<typename K, typename V>
    void encode(OrderedDictionary<K, V> const& dictionary)
    {
        Encoder::WithArray withArray(encoder);
        for (auto p : dictionary)
        {
            Encoder::WithKeyValuePair withPair(encoder);
            encode(p.key);
            encode(p.value);
        }
    }

    template<typename K, typename V>
    void encode(Dictionary<K, V> const& dictionary)
    {
        Encoder::WithArray withArray(encoder);
        for (auto p : dictionary)
        {
            Encoder::WithKeyValuePair withPair(encoder);
            encode(p.first);
            encode(p.second);
        }
    }

    template<typename T>
    void encode(T const& value)
    {
        encodeValue(value);
    }

    // for each class of node, we generate
    // code to recursively serialize each
    // of its fields.

#if 0 // FIDDLE TEMPLATE:
%for _,T in ipairs(Slang.NodeBase.subclasses) do
    void _encodeDataOf($T* obj)
    {
%if T.directSuperClass then
        _encodeDataOf(static_cast<$(T.directSuperClass)*>(obj));
%end
%for _,f in ipairs(T.directFields) do
        encode(obj->$f);
%end
    }
%end
#else // FIDDLE OUTPUT:
#define FIDDLE_GENERATED_OUTPUT_ID 0
#include "slang-serialize-ast.cpp.fiddle"
#endif // FIDDLE END
};

void writeSerializedModuleAST(
    Encoder* encoder,
    ModuleDecl* moduleDecl,
    SerialSourceLocWriter* sourceLocWriter)
{
    Encoder::WithObject withObject(encoder);

    // TODO: we should have a more careful pass here,
    // where we only encode the public declarations
    //

    ASTEncodingContext context(encoder, moduleDecl, sourceLocWriter);
    context.getDeclID(moduleDecl);
    context.flush();
}

class ASTDecodingContext : public RefObject
{
public:
    ASTDecodingContext(
        Linkage* linkage,
        ASTBuilder* astBuilder,
        DiagnosticSink* sink,
        RefPtr<RiffContainerObject> riff,
        RiffContainer::Chunk* rootChunk,
        SerialSourceLocReader* sourceLocReader,
        SourceLoc requestingSourceLoc)
        : _linkage(linkage)
        , _astBuilder(astBuilder)
        , _sink(sink)
        , _riff(riff)
        , _rootChunk(static_cast<RiffContainer::ListChunk*>(rootChunk))
        , _sourceLocReader(sourceLocReader)
        , _requestingSourceLoc(requestingSourceLoc)
    {
    }

    Linkage* _linkage = nullptr;
    DiagnosticSink* _sink = nullptr;
    RefPtr<RiffContainerObject> _riff;
    RefPtr<SerialSourceLocReader> _sourceLocReader = nullptr;
    SourceLoc _requestingSourceLoc;

    MangledNameTableReader _exportsTable;
    StringTableReader _stringTable;

    SlangResult init()
    {
        // We want to do as little as possible at this step,
        // so that we don't spend too much time...

        // There are a few different top-level chunks that
        // hold different arrays that we need in order
        // to decode the entire module hierarchy.
        //
        // Basically, these lists correspond to the kinds
        // of nodes in the AST hierarchy for which back-references
        // are allowed (all other nodes should, barring
        // weird corner cases, form a single tree-structured
        // ownership hierarchy, rooted at the `ModuleDecl`.
        //

        // First there is the list that actually encodes
        // for the declarations in the module, including
        // the `ModuleDecl` itself, which should be the
        // first entry in the list.
        //
        auto declChunk = _rootChunk->findListChunk(SerialBinary::kASTDeclListFourCC);
        SLANG_ASSERT(declChunk != nullptr);

        // Next there is a list of all the declarations
        // referenced inside of the module that need to
        // be imported in from outside.
        //
        auto importedDeclChunk =
            _rootChunk->findListChunk(SerialBinary::kASTImportedDeclListFourCC);
        SLANG_ASSERT(importedDeclChunk != nullptr);

        // Then there are all the `Val`-derived nodes that
        // are needed by the module, which will need to be
        // deduplicated so that they are unique within the
        // current compilation context.
        //
        auto valChunk = _rootChunk->findListChunk(SerialBinary::kASTValListFourCC);
        SLANG_ASSERT(valChunk != nullptr);

        // The process of decoding the module is then spread
        // over a number of steps.
        //
        // The first step is to process all of the imported
        // declarations, so that other nodes can refer to
        // them.
        //
        SLANG_RETURN_ON_FAIL(initImportedDecls(importedDeclChunk));

        // Next we process the declarations that are within
        // the module itself, first creating an "empty shell"
        // of each declaration that has the right size in
        // memory (and the right `ASTNodeType` tag), so that
        // we can wire up references to it (including circular
        // references)... so long as nothing here tries to
        // look *inside* the empty shell along the way.
        //
        SLANG_RETURN_ON_FAIL(initDecls(declChunk));

        // Once all the `Decl`s that might be needed have
        // been allocated, we can process all the `Val`s
        // that might reference those`Decl`s (and one another).
        //
        // The nature of the `Val` representation ensures
        // that there cannot be cirularities in the references
        // between `Val`s, and the encoding process will have
        // sorted the entries so that a `Val` only ever appears
        // *after* its operands.
        //
        SLANG_RETURN_ON_FAIL(initVals(valChunk));


        // In addition to the required chunks handled above,
        // there is also an additional *optional* chunk that
        // can provide a list of declarations in the module that
        // need to be registered as builtins via the `ASTBuilder`.
        //
        if (auto builtinsChunk = _rootChunk->findListChunk(SerialBinary::kASTBuiltinDeclListFourCC))
        {
            Decoder decoder(builtinsChunk);
            Decoder::WithArray withArray(decoder, SerialBinary::kASTBuiltinDeclListFourCC);

            while (decoder.hasElements())
            {
                Decl* decl = nullptr;
                decode(decl, decoder);

                registerBuiltinDecl(_linkage->getSessionImpl(), decl);
            }
        }

        // Fetch the sections needed to implement fast lookup based
        // of exports based on their mangled names.
        //
        auto exportsChunk = _rootChunk->findListChunk(SerialBinary::kASTExportsFourCC);
        _exportsTable.init(exportsChunk);

        auto stringTableChunk = _rootChunk->findListChunk(SerialBinary::kStringTableFourCc);
        _stringTable.init(stringTableChunk);


#if 0
        // Once all the back-reference-able objects have been
        // instantiated in memory, we can go back through the
        // `Decl`s in the module and fill in those empty shells.
        //
        SLANG_RETURN_ON_FAIL(fillEmptyShells(declChunk));

        // As a final pass,  we perform any special cleanup actions
        // that might be required to make the output valid for consumers.
        //
        // For example, this is where we set the `DeclCheckState` of everything
        // we are loading to reflect the fact that everything we deserialize
        // is (supposed to be) fully cheked.
        //
        SLANG_RETURN_ON_FAIL(cleanUpNodes());
#endif

        return SLANG_OK;
    }

    typedef Int DeclID;
    Decl* getDeclByID(DeclID id)
    {
        if (id >= 0)
        {
            return _getLocalDeclByIndex(id);
        }
        else
        {
            return _getImportedDeclByIndex(~id);
        }
    }

    Decl* getDirectMemberDeclByIndex(Index index, void const* containerOnDemandDecodeData)
    {
        auto membersChunk = (RiffContainer::ListChunk*)containerOnDemandDecodeData;
        DirectMemberDeclsReader reader(membersChunk);

        auto memberDeclID = reader.getDeclID(index);

        return getDeclByID(memberDeclID);
    }

    Decl* findDirectMemberDeclByName(Name* name, void const* containerOnDemandDecodeData)
    {
        auto membersChunk = (RiffContainer::ListChunk*)containerOnDemandDecodeData;
        DirectMemberDeclsReader reader(membersChunk);

        auto memberIDs = reader.findDeclsByName(name, &_stringTable);

        Decl* result = nullptr;
        for (auto memberID : memberIDs)
        {
            auto memberDecl = getDeclByID(memberID);
            memberDecl->nextInContainerWithSameName = result;
            result = memberDecl;
        }
        return result;
    }


    Decl* findExportedDeclByMangledName(UnownedStringSlice const& mangledName)
    {
        Int declID = 0;
        if (_exportsTable.findEntry(mangledName, declID))
        {
            return getDeclByID(declID);
        }
        return nullptr;
    }


private:
    struct UnhandledCase
    {
    };

    ASTBuilder* _astBuilder = nullptr;
    RiffContainer::ListChunk* _rootChunk = nullptr;

    struct DeclInfo
    {
        Decl* decl = nullptr;
        RiffContainer::Chunk* chunk = nullptr;
    };

    struct ValInfo
    {
        Val* val = nullptr;
        RiffContainer::Chunk* chunk = nullptr;
    };

    List<DeclInfo> _decls;
    List<DeclInfo> _importedDecls;
    List<ValInfo> _vals;

    typedef Int ValID;

    Val* getValByID(ValID id)
    {
        auto& info = _vals[id];
        if (auto val = info.val)
            return val;

        Decoder decoder(info.chunk);

        // TODO: this can end up going recursive
        // to a somewhat arbitrary depth. We should
        // be building up a list of the entries that
        // need to be processed and *then* decoding
        // them all.

        Val* val = decodeValNode(decoder);
        info.val = val;

        return val;
    }

    SlangResult initImportedDecls(RiffContainer::Chunk* importedDeclChunk)
    {
        Decoder decoder(importedDeclChunk);

        Decoder::WithArray withArray(decoder, SerialBinary::kASTImportedDeclListFourCC);
        while (decoder.hasElements())
        {
            auto chunk = decoder.getCursor();
            decoder.skip();

            DeclInfo info;
            info.chunk = chunk;
            _importedDecls.add(info);
        }
        return SLANG_OK;
    }

    Decl* _getImportedDeclByIndex(Index index)
    {
        auto& info = _importedDecls[index];
        if (auto decl = info.decl)
            return decl;

        Decoder decoder(info.chunk);
        info.decl = _decodeImportedDecl(decoder);
        return info.decl;
    }

    Decl* _decodeImportedDecl(Decoder& decoder)
    {
        Decoder::WithKeyValuePair withPair(decoder);

        DeclID importedFromModuleDeclID;
        decode(importedFromModuleDeclID, decoder);

        if (importedFromModuleDeclID == 0)
        {
            Name* moduleName = nullptr;
            decode(moduleName, decoder);

            Decl* importedModule = getImportedModule(moduleName);
            return importedModule;
        }
        else
        {
            auto importedFromModuleDecl = as<ModuleDecl>(getDeclByID(importedFromModuleDeclID));
            auto importedFromModule = importedFromModuleDecl->module;

            String mangledName;
            decode(mangledName, decoder);

            auto importedNode =
                importedFromModule->findExportFromMangledName(mangledName.getUnownedSlice());
            auto importedDecl = as<Decl>(importedNode);
            return importedDecl;
        }
    }

    ModuleDecl* getImportedModule(Name* moduleName)
    {
        Module* module = _linkage->findOrImportModule(moduleName, _requestingSourceLoc, _sink);
        if (!module)
        {
            SLANG_ABORT_COMPILATION("failed to load an imported module during deserialization");
        }

        return module->getModuleDecl();
    }

    SlangResult initVals(RiffContainer::Chunk* valChunk)
    {
        Decoder decoder(valChunk);

        Decoder::WithArray withArray(decoder, SerialBinary::kASTValListFourCC);
        while (decoder.hasElements())
        {
            auto chunk = decoder.getCursor();
            decoder.skip();

            ValInfo info;
            info.chunk = chunk;
            _vals.add(info);
#if 0
            Val* val = decodeValNode(decoder);
            _vals.add(val);
#endif
        }
        return SLANG_OK;
    }

    SlangResult initDecls(RiffContainer::Chunk* declChunk)
    {
        Decoder decoder(declChunk);

        Decoder::WithArray withArray(decoder, SerialBinary::kASTDeclListFourCC);
        while (decoder.hasElements())
        {
            auto chunk = decoder.getCursor();
            decoder.skip();

            DeclInfo info;
            info.chunk = chunk;
            _decls.add(info);
        }
        return SLANG_OK;
    }

#if SLANG_DEBUG_ON_DEMAND_LOADING_STATS
    int _loadedDeclCount = 0;
#endif

    Decl* _getLocalDeclByIndex(Index index)
    {
        auto& info = _decls[index];
        if (auto decl = info.decl)
            return decl;

        // Each of the declarations is expected to take
        // the form of an object with a first field
        // that holds the node type.
        //
        ASTNodeType nodeType;
        {
            Decoder decoder(info.chunk);

            Decoder::WithObject withObject(decoder);
            decode(nodeType, decoder);
        }

        auto emptyShell = createEmptyShell(nodeType);
        auto decl = as<Decl>(emptyShell);
        SLANG_ASSERT(decl);

        info.decl = decl;

        // TODO: need to avoid recursion in the process
        // of filling in the shells...

        {
            Decoder decoder(info.chunk);
            decodeASTNodeContent(decl, decoder);
        }

#if SLANG_DEBUG_ON_DEMAND_LOADING_STATS
        _loadedDeclCount++;

        fprintf(
            stderr,
            "[DEMAND] on-demand loaded '%s' module declaration #%d",
            getDeclByID(0)->getName()->text.getBuffer(),
            int(index));

        fprintf(
            stderr,
            ", have so far loaded %d of %d (%f%%)",
            int(_loadedDeclCount),
            int(_decls.getCount()),
            (100 * float(_loadedDeclCount)) / float(_decls.getCount()));

        fprintf(stderr, "\n");
#endif

        return decl;
    }

    Val* decodeValNode(Decoder& decoder)
    {
        Decoder::WithObject withObject(decoder);

        ASTNodeType nodeType;
        decode(nodeType, decoder);

        ValNodeDesc desc;
        desc.type = SyntaxClass<NodeBase>(nodeType);

        Decoder::WithArray withArray(decoder);
        while (decoder.hasElements())
        {
            ValNodeOperand operand;
            decode(operand, decoder);
            desc.operands.add(operand);
        }

        desc.init();

        auto val = _astBuilder->_getOrCreateImpl(_Move(desc));

        // Values created during deserialization are
        // not expected to ever resolve further, because
        // they should be coming from fully checked code.
        //
        // val->resolve();
        // val->_setUnique();

        return val;
    }

    NodeBase* createEmptyShell(ASTNodeType nodeType)
    {
        return SyntaxClass<NodeBase>(nodeType).createInstance(_astBuilder);
    }

#if 0
    SlangResult fillEmptyShells(RiffContainer::Chunk* declChunk)
    {
        Index declIndex = 0;

        Decoder decoder(declChunk);
        Decoder::WithArray withArray(decoder);
        while (decoder.hasElements())
        {
            auto declEmptyShell = _decls[declIndex++];
            decodeASTNodeContent(declEmptyShell, decoder);
        }

        return SLANG_OK;
    }

    SlangResult cleanUpNodes()
    {
        for (auto decl : _decls)
        {
            decl->checkState = DeclCheckState::CapabilityChecked;
        }

        return SLANG_OK;
    }
#endif


    void assignGenericParameterIndices(GenericDecl* genericDecl)
    {
        int parameterCounter = 0;
        for (auto m : genericDecl->getMembers())
        {
            if (auto typeParam = as<GenericTypeParamDeclBase>(m))
            {
                typeParam->parameterIndex = parameterCounter++;
            }
            else if (auto valParam = as<GenericValueParamDecl>(m))
            {
                valParam->parameterIndex = parameterCounter++;
            }
        }
    }


    void cleanUpASTNode(NodeBase* node)
    {
        if (auto expr = as<Expr>(node))
        {
            expr->checked = true;
        }
        else if (auto decl = as<Decl>(node))
        {
            decl->checkState = DeclCheckState::FullyChecked;

            if (auto genericDecl = as<GenericDecl>(node))
            {
                assignGenericParameterIndices(genericDecl);
            }
            else if (auto syntaxDecl = as<SyntaxDecl>(node))
            {
                syntaxDecl->parseCallback = &parseSimpleSyntax;
                syntaxDecl->parseUserData = (void*)syntaxDecl->syntaxClass.getInfo();
            }
            else if (auto namespaceLikeDecl = as<NamespaceDeclBase>(node))
            {
                auto declScope = _astBuilder->create<Scope>();
                declScope->containerDecl = namespaceLikeDecl;
                namespaceLikeDecl->ownedScope = declScope;
            }
        }
    }

    void decodeASTNodeContent(NodeBase* node, Decoder& decoder)
    {
        Decoder::WithObject withObject(decoder);

        ASTNodeDispatcher<NodeBase, void>::dispatch(
            node,
            [&](auto n) { _decodeDataOf(n, decoder); });

        cleanUpASTNode(node);
    }

    DeclID decodeDeclID(Decoder& decoder)
    {
        DeclID result = decoder.decode<DeclID>();
        return result;
    }

    ValID decodeValID(Decoder& decoder)
    {
        ValID result = decoder.decode<ValID>();
        return result;
    }

    template<typename T>
    void decodeASTNode(T*& node, Decoder& decoder)
    {
        ASTNodeType nodeType;
        auto saved = decoder.getCursor();
        {
            Decoder::WithObject withObject(decoder);
            decode(nodeType, decoder);
        }
        decoder.setCursor(saved);

        auto shell = createEmptyShell(nodeType);
        decodeASTNodeContent(shell, decoder);

        node = as<T>(shell);
    }

    void decodePtr(Name*& name, Decoder& decoder, Name*)
    {
        String text;
        decode(text, decoder);

        name = _astBuilder->getNamePool()->getName(text);
    }

    void decodePtr(DeclAssociationList*& outList, Decoder& decoder, DeclAssociationList*)
    {
        // Mirroring the encoding logic, we decode this
        // as a list of key-value pairs.
        //
        auto list = RefPtr(new DeclAssociationList());
        Decoder::WithArray withArray(decoder);
        while (decoder.hasElements())
        {
            auto association = RefPtr(new DeclAssociation());

            Decoder::WithKeyValuePair withPair(decoder);
            decode(association->kind, decoder);
            decode(association->decl, decoder);

            list->associations.add(association);
        }

        outList = list.detach();
    }

    void decodePtr(DiagnosticInfo const*& info, Decoder& decoder, DiagnosticInfo const*)
    {
        Int id;
        decode(id, decoder);
        info = getDiagnosticsLookup()->getDiagnosticById(id);
    }

    void decodePtr(MarkupEntry*& markupEntry, Decoder&, MarkupEntry*)
    {
        // TODO: is this case needed?
        markupEntry = nullptr;
    }

    void decodePtr(CandidateExtensionList*& list, Decoder& decoder, CandidateExtensionList*)
    {
        auto result = RefPtr(new CandidateExtensionList());
        decode(result->candidateExtensions, decoder);
        list = result.detach();
    }

    void decodePtr(WitnessTable*& witnessTable, Decoder& decoder, WitnessTable*)
    {
        Decoder::WithObject withObject(decoder);
        auto wt = RefPtr(new WitnessTable());
        decode(wt->baseType, decoder);
        decode(wt->witnessedType, decoder);
        decode(wt->isExtern, decoder);
        decode(wt->m_requirementDictionary, decoder);
        witnessTable = wt.detach();
    }

    void decodeValue(RequirementWitness& witness, Decoder& decoder)
    {
        Decoder::WithKeyValuePair withPair(decoder);
        decodeEnum(witness.m_flavor, decoder);
        switch (witness.m_flavor)
        {
        case RequirementWitness::Flavor::none:
            break;

        case RequirementWitness::Flavor::declRef:
            decode(witness.m_declRef, decoder);
            break;

        case RequirementWitness::Flavor::val:
            decode(witness.m_val, decoder);
            break;

        case RequirementWitness::Flavor::witnessTable:
            {
                RefPtr<WitnessTable> object;
                decode(object, decoder);
                witness.m_obj = object;
            }
            break;
        }
    }

    template<typename T>
    void decodePtr(T*& node, Decoder& decoder, Val*)
    {
        ValID id = decodeValID(decoder);
        node = static_cast<T*>(getValByID(id));
    }

    template<typename T>
    void decodePtr(T*& node, Decoder& decoder, Decl*)
    {
        DeclID id = decodeDeclID(decoder);
        node = static_cast<T*>(getDeclByID(id));
    }

    template<typename T>
    void decodePtr(T*& node, Decoder& decoder, DeclBase*)
    {
        if (decoder.getTag() == SerialBinary::kInt64FourCC)
        {
            DeclID id = decodeDeclID(decoder);
            node = static_cast<T*>(getDeclByID(id));
        }
        else
        {
            decodeASTNode(node, decoder);
        }
    }

    template<typename T>
    void decodePtr(T*& node, Decoder& decoder, NodeBase*)
    {
        decodeASTNode(node, decoder);
    }


    void decodeValue(UnhandledCase, Decoder& decoder);

    void decodeValue(String& value, Decoder& decoder) { value = decoder.decodeString(); }

    void decodeValue(Token& value, Decoder& decoder)
    {
        decode(value.type, decoder);
        decode(value.flags, decoder);
        decode(value.loc, decoder);
        if (decoder.decodeNull())
        {
        }
        else
        {
            Name* name = nullptr;
            decode(name, decoder);
            value.setName(name);
        }
    }

    void decodeValue(NameLoc& value, Decoder& decoder) { decode(value.name, decoder); }

    void decodeValue(SemanticVersion& value, Decoder& decoder)
    {
        SemanticVersion::IntegerType rawValue = decoder.decode<SemanticVersion::IntegerType>();
        value.setFromInteger(rawValue);
    }

    void decodeValue(CapabilitySet& value, Decoder& decoder)
    {
        Decoder::WithArray withArray(decoder);
        while (decoder.hasElements())
        {
            CapabilityTargetSet targetSet;
            decode(targetSet, decoder);
            value.getCapabilityTargetSets()[targetSet.target] = targetSet;
        }
    }

    void decodeValue(CapabilityTargetSet& value, Decoder& decoder)
    {
        Decoder::WithKeyValuePair withPair(decoder);
        decode(value.target, decoder);

        Decoder::WithArray withArray(decoder);
        while (decoder.hasElements())
        {
            CapabilityStageSet stageSet;
            decode(stageSet, decoder);
            value.shaderStageSets[stageSet.stage] = stageSet;
        }
    }

    void decodeValue(CapabilityStageSet& value, Decoder& decoder)
    {
        Decoder::WithKeyValuePair withPair(decoder);
        decode(value.stage, decoder);
        decode(value.atomSet, decoder);
    }

    void decodeValue(CapabilityAtomSet& value, Decoder& decoder)
    {
        Decoder::WithArray withArray(decoder);
        while (decoder.hasElements())
        {
            CapabilityAtom atom;
            decode(atom, decoder);
            value.add(UInt(atom));
        }
    }

    template<typename T>
    void decodeValue(std::optional<T>& outValue, Decoder& decoder)
    {
        if (decoder.decodeNull())
        {
            outValue.reset();
        }
        else
        {
            T value;
            decode(value, decoder);
            outValue = value;
        }
    }

    void decodeValue(SyntaxClass<NodeBase>& syntaxClass, Decoder& decoder)
    {
        ASTNodeType nodeType;
        decode(nodeType, decoder);
        syntaxClass = SyntaxClass<NodeBase>(nodeType);
    }

    template<typename T>
    void decodeValue(DeclRef<T>& declRef, Decoder& decoder)
    {
        decode(declRef.declRefBase, decoder);
    }

    void decodeValue(ValNodeOperand& value, Decoder& decoder)
    {
        Decoder::WithKeyValuePair withPair(decoder);

        decodeEnum(value.kind, decoder);
        switch (value.kind)
        {
        case ValNodeOperandKind::ConstantValue:
            decode(value.values.intOperand, decoder);
            break;

        case ValNodeOperandKind::ValNode:
            {
                Val* val = nullptr;
                decode(val, decoder);
                value.values.nodeOperand = val;
            }
            break;

        case ValNodeOperandKind::ASTNode:
            {
                Decl* decl = nullptr;
                decode(decl, decoder);
                value.values.nodeOperand = decl;
            }
            break;
        }
    }

    void decodeValue(TypeExp& value, Decoder& decoder) { decode(value.type, decoder); }

    void decodeValue(QualType& value, Decoder& decoder)
    {
        Decoder::WithObject withObject(decoder);
        decode(value.type, decoder);
        decode(value.isLeftValue, decoder);
        decode(value.hasReadOnlyOnTarget, decoder);
        decode(value.isWriteOnly, decoder);
    }

    void decodeValue(MatrixCoord& value, Decoder& decoder)
    {
        Decoder::WithObject withObject(decoder);
        decode(value.row, decoder);
        decode(value.col, decoder);
    }

    void decodeValue(SPIRVAsmOperand::Flavor& value, Decoder& decoder)
    {
        decodeEnum(value, decoder);
    }

    void decodeValue(SPIRVAsmOperand& value, Decoder& decoder)
    {
        Decoder::WithObject withObject(decoder);
        decode(value.flavor, decoder);
        decode(value.token, decoder);
        decode(value.expr, decoder);
        decode(value.bitwiseOrWith, decoder);
        decode(value.knownValue, decoder);
        decode(value.wrapInId, decoder);
        decode(value.type, decoder);
    }

    void decodeValue(SPIRVAsmInst& value, Decoder& decoder)
    {
        Decoder::WithObject withObject(decoder);
        decode(value.opcode, decoder);
        decode(value.operands, decoder);
    }


    template<typename T>
    void decodeEnum(T& value, Decoder& decoder)
    {
        value = T(decoder.decode<Int32>());
    }

    template<typename T>
    void decodeSimpleValue(T& value, Decoder& decoder)
    {
        value = decoder.decode<T>();
    }

    void decodeValue(bool& value, Decoder& decoder) { value = decoder.decodeBool(); }
    void decodeValue(Int32& value, Decoder& decoder) { decodeSimpleValue(value, decoder); }
    void decodeValue(Int64& value, Decoder& decoder) { decodeSimpleValue(value, decoder); }
    void decodeValue(UInt32& value, Decoder& decoder) { decodeSimpleValue(value, decoder); }
    void decodeValue(UInt64& value, Decoder& decoder) { decodeSimpleValue(value, decoder); }
    void decodeValue(float& value, Decoder& decoder) { decodeSimpleValue(value, decoder); }
    void decodeValue(double& value, Decoder& decoder) { decodeSimpleValue(value, decoder); }

    void decodeValue(uint8_t& value, Decoder& decoder)
    {
        value = uint8_t(decoder.decode<UInt32>());
    }

    void decodeValue(DeclVisibility& value, Decoder& decoder) { decodeEnum(value, decoder); }
    void decodeValue(BaseType& value, Decoder& decoder) { decodeEnum(value, decoder); }
    void decodeValue(BuiltinRequirementKind& value, Decoder& decoder)
    {
        decodeEnum(value, decoder);
    }
    void decodeValue(ASTNodeType& value, Decoder& decoder) { decodeEnum(value, decoder); }
    void decodeValue(ImageFormat& value, Decoder& decoder) { decodeEnum(value, decoder); }
    void decodeValue(TypeTag& value, Decoder& decoder) { decodeEnum(value, decoder); }
    void decodeValue(TryClauseType& value, Decoder& decoder) { decodeEnum(value, decoder); }
    void decodeValue(CapabilityAtom& value, Decoder& decoder) { decodeEnum(value, decoder); }
    void decodeValue(PreferRecomputeAttribute::SideEffectBehavior& value, Decoder& decoder)
    {
        decodeEnum(value, decoder);
    }
    void decodeValue(LogicOperatorShortCircuitExpr::Flavor& value, Decoder& decoder)
    {
        decodeEnum(value, decoder);
    }
    void decodeValue(TreatAsDifferentiableExpr::Flavor& value, Decoder& decoder)
    {
        decodeEnum(value, decoder);
    }
    void decodeValue(DeclAssociationKind& value, Decoder& decoder) { decodeEnum(value, decoder); }
    void decodeValue(TokenType& value, Decoder& decoder) { decodeEnum(value, decoder); }


    void decodeValue(SourceLoc& value, Decoder& decoder)
    {
        if (!decoder.decodeNull())
        {
            SerialSourceLocData::SourceLoc intermediate;
            decoder.decode(intermediate);

            if (_sourceLocReader)
            {
                auto sourceLoc = _sourceLocReader->getSourceLoc(intermediate);
                value = sourceLoc;
            }
        }
    }

    template<typename T>
    void decodeValue(T*& ptr, Decoder& decoder)
    {
        if (decoder.decodeNull())
            ptr = nullptr;
        else
            decodePtr(ptr, decoder, (T*)nullptr);
    }

    template<typename T>
    void decodeValue(RefPtr<T>& ptr, Decoder& decoder)
    {
        if (decoder.decodeNull())
            ptr = nullptr;
        else
        {
            // Hi Future Tess,
            //
            // The next step here is decoding logic for `WitnessTable`s.
            //

            decodePtr(*ptr.writeRef(), decoder, (T*)nullptr);
        }
    }

    void decodeValue(Modifiers& modifiers, Decoder& decoder)
    {
        Modifier** link = &modifiers.first;

        Decoder::WithArray withArray(decoder);
        while (decoder.hasElements())
        {
            Modifier* modifier = nullptr;
            decode(modifier, decoder);

            *link = modifier;
            link = &modifier->next;
        }
    }

    void decodeValue(ContainerDeclMembers& members, Decoder& decoder)
    {
        // Because we are doing on-demand decoding,
        // we don't want to actually do anything beyond the bare
        // minimum here.
        //

        auto chunk = decoder.getCursor();
        decoder.skip();

        auto listChunk = as<RiffContainer::ListChunk>(chunk);
        SLANG_ASSERT(listChunk != nullptr);

        DirectMemberDeclsReader reader(listChunk);
        auto directMemberCount = reader.getDeclCount();

        members._initForOnDemandDecode(directMemberCount, listChunk, this);
    }

    template<typename T, int N>
    void decodeValue(ShortList<T, N>& array, Decoder& decoder)
    {
        Decoder::WithArray withArray(decoder);
        while (decoder.hasElements())
        {
            T element;
            decode(element, decoder);
            array.add(element);
        }
    }


    template<typename T>
    void decode(List<T>& array, Decoder& decoder)
    {
        Decoder::WithArray withArray(decoder);
        while (decoder.hasElements())
        {
            T element;
            decode(element, decoder);
            array.add(element);
        }
    }

    template<typename T, size_t N>
    void decode(T (&array)[N], Decoder& decoder)
    {
        Decoder::WithArray withArray(decoder);
        for (auto& element : array)
        {
            decode(element, decoder);
        }
    }

    template<typename K, typename V>
    void decode(OrderedDictionary<K, V>& dictionary, Decoder& decoder)
    {
        Decoder::WithArray withArray(decoder);
        while (decoder.hasElements())
        {
            Decoder::WithKeyValuePair withPair(decoder);

            K key;
            V value;
            decode(key, decoder);
            decode(value, decoder);

            dictionary.add(key, value);
        }
    }

    template<typename K, typename V>
    void decode(Dictionary<K, V>& dictionary, Decoder& decoder)
    {
        Decoder::WithArray withArray(decoder);
        while (decoder.hasElements())
        {
            Decoder::WithKeyValuePair withPair(decoder);

            K key;
            V value;
            decode(key, decoder);
            decode(value, decoder);

            dictionary.add(key, value);
        }
    }

    template<typename T>
    void decode(T& outValue, Decoder& decoder)
    {
        decodeValue(outValue, decoder);
    }

#if 0 // FIDDLE TEMPLATE:
%for _,T in ipairs(Slang.NodeBase.subclasses) do
        void _decodeDataOf($T* obj, Decoder& decoder)
        {
%   if T.directSuperClass then
            _decodeDataOf(static_cast<$(T.directSuperClass)*>(obj), decoder);
%   end
%   for _,f in ipairs(T.directFields) do
            decode(obj->$f, decoder);
%   end
        }
%end
#else // FIDDLE OUTPUT:
#define FIDDLE_GENERATED_OUTPUT_ID 1
#include "slang-serialize-ast.cpp.fiddle"
#endif // FIDDLE END
};

ModuleDecl* readSerializedModuleAST(
    Linkage* linkage,
    ASTBuilder* astBuilder,
    DiagnosticSink* sink,
    RefPtr<RiffContainerObject> riff,
    RiffContainer::Chunk* chunk,
    SerialSourceLocReader* sourceLocReader,
    SourceLoc requestingSourceLoc)
{
    auto deserializer = RefPtr(new ASTDecodingContext(
        linkage,
        astBuilder,
        sink,
        riff,
        chunk,
        sourceLocReader,
        requestingSourceLoc));

    deserializer->init();

    auto node = deserializer->getDeclByID(0);

#if 0

    ASTDecodingContext
        context(linkage, astBuilder, sink, chunk, sourceLocReader, requestingSourceLoc);

    // The essence of on-demand deserialization is that we *won't*
    // decode everything at once...
//    context.decodeAll();
    auto node = context.getDeclByID(0);
#endif
    auto moduleDecl = as<ModuleDecl>(node);
    return moduleDecl;
}

Decl* ContainerDeclMembers::findExportedDeclByMangledNameInBinaryModule(
    UnownedStringSlice const& mangledName)
{
    auto context = as<ASTDecodingContext>(onDemandDecodeContext);
    return context->findExportedDeclByMangledName(mangledName);
}

Decl* ContainerDeclMembers::getDirectMemberDeclByIndexInBinaryModule(Index index)
{
    auto context = as<ASTDecodingContext>(onDemandDecodeContext);
    return context->getDirectMemberDeclByIndex(index, onDemandDecodeData);
}

Decl* ContainerDeclMembers::findDirectMemberDeclByNameInBinaryModule(Name* name)
{
    auto context = as<ASTDecodingContext>(onDemandDecodeContext);
    return context->findDirectMemberDeclByName(name, onDemandDecodeData);
}

} // namespace Slang
