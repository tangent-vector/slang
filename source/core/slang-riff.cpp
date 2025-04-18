#include "slang-riff.h"

#include "slang-com-helper.h"
#include "slang-hex-dump-util.h"

namespace Slang
{

/* static */ int64_t RiffUtil::calcChunkTotalSize(const RiffHeader& chunk)
{
    size_t size = chunk.size + sizeof(RiffHeader);
    return getPaddedSize(size);
}

/* static */ SlangResult RiffUtil::skip(
    const RiffHeader& chunk,
    Stream* stream,
    int64_t* remainingBytesInOut)
{
    int64_t chunkSize = calcChunkTotalSize(chunk);
    if (remainingBytesInOut)
    {
        *remainingBytesInOut -= chunkSize;
    }

    // Skip the payload (we don't need to skip the Chunk because that was already read
    SLANG_RETURN_ON_FAIL(stream->seek(SeekOrigin::Current, chunkSize - sizeof(RiffHeader)));
    return SLANG_OK;
}

/* static */ SlangResult RiffUtil::readChunk(Stream* stream, RiffHeader& outChunk)
{
    size_t readBytes;
    SLANG_RETURN_ON_FAIL(stream->read(&outChunk, sizeof(RiffHeader), readBytes));
    // TODO(JS): Could handle endianness issues here...
    return (readBytes == sizeof(RiffHeader)) ? SLANG_OK : SLANG_FAIL;
}

/* static */ SlangResult RiffUtil::writeData(
    const RiffHeader* header,
    size_t headerSize,
    const void* payload,
    size_t payloadSize,
    Stream* out)
{
    SLANG_ASSERT(uint64_t(payloadSize) <= uint64_t(0xfffffffff));
    SLANG_ASSERT(headerSize >= sizeof(RiffHeader));

    // TODO(JS): Could handle endianness here

    RiffHeader chunk;
    chunk.type = header->type;
    chunk.size = uint32_t(headerSize - sizeof(RiffHeader) + payloadSize);

    // The chunk
    SLANG_RETURN_ON_FAIL(out->write(&chunk, sizeof(RiffHeader)));

    // Remainder of header
    if (headerSize > sizeof(RiffHeader))
    {
        // The rest of the header
        SLANG_RETURN_ON_FAIL(out->write(header + 1, headerSize - sizeof(RiffHeader)));
    }

    // Write the payload
    SLANG_RETURN_ON_FAIL(out->write(payload, payloadSize));

    // The riff spec requires all chunks are 4 byte aligned (even if size is not)
    size_t padSize = getPaddedSize(payloadSize);
    if (padSize - payloadSize)
    {
        uint8_t end[kRiffPadSize] = {0};
        SLANG_RETURN_ON_FAIL(out->write(end, padSize - payloadSize));
    }

    return SLANG_OK;
}

/* static */ SlangResult RiffUtil::readPayload(
    Stream* stream,
    size_t size,
    void* outData,
    size_t& outReadSize)
{
    outReadSize = 0;

    SLANG_RETURN_ON_FAIL(stream->readExactly(outData, size));

    const size_t alignedSize = getPaddedSize(size);
    // Skip to the alignment
    if (alignedSize > size)
    {
        SLANG_RETURN_ON_FAIL(stream->seek(SeekOrigin::Current, alignedSize - size));
    }
    outReadSize = alignedSize;
    return SLANG_OK;
}

/* static */ SlangResult RiffUtil::readData(
    Stream* stream,
    RiffHeader* outHeader,
    size_t headerSize,
    List<uint8_t>& data)
{
    RiffHeader chunk;
    SLANG_RETURN_ON_FAIL(readChunk(stream, chunk));
    if (chunk.size < headerSize)
    {
        return SLANG_FAIL;
    }

    *outHeader = chunk;

    // Read the header
    if (headerSize > sizeof(RiffHeader))
    {
        SLANG_RETURN_ON_FAIL(stream->readExactly(outHeader + 1, headerSize - sizeof(RiffHeader)));
    }

    const size_t payloadSize = chunk.size - (headerSize - sizeof(RiffHeader));
    size_t readSize;
    data.setCount(payloadSize);
    return readPayload(stream, payloadSize, data.getBuffer(), readSize);
}

/* static */ SlangResult RiffUtil::readHeader(Stream* stream, RiffListHeader& outHeader)
{
    // Need to read the chunk header
    SLANG_RETURN_ON_FAIL(readChunk(stream, outHeader.chunk));
    outHeader.subType = 0;

    if (isListType(outHeader.chunk.type))
    {
        // Read the sub type
        SLANG_RETURN_ON_FAIL(
            stream->readExactly(&outHeader.subType, sizeof(RiffListHeader) - sizeof(RiffHeader)));
    }

    return SLANG_OK;
}

namespace
{ // anonymous

struct RiffDumpContext
{
    typedef RiffContainer::Chunk Chunk;
    typedef RiffContainer::ListChunk ListChunk;
    typedef RiffContainer::DataChunk DataChunk;

    void dumpChunk(Chunk* chunk)
    {
        if (auto listChunk = as<ListChunk>(chunk))
        {
            _dumpListChunk(listChunk);
        }
        else if (auto dataChunk = as<DataChunk>(chunk))
        {
            _dumpDataChunk(dataChunk);
        }
        else
        {
            SLANG_UNEXPECTED("RIFF chunk was neither list nor data");
        }
    }

    void _dumpListChunk(ListChunk* list)
    {
        _dumpIndent();
        // If it's the root it's 'riff'
        _dumpRiffType(list == m_rootChunk ? RiffFourCC::kRiff : RiffFourCC::kList);
        m_writer.put(" ");
        _dumpRiffType(list->getType());
        m_writer.put("\n");

        m_indent++;
        for (auto chunk = list->m_firstChild; chunk; chunk = chunk->m_next)
        {
            dumpChunk(chunk);
        }
        m_indent--;
    }

    void _dumpDataChunk(DataChunk* data)
    {
        _dumpIndent();
        // Write out the name
        _dumpRiffType(data->getType());
        m_writer.put(" ");


        FNV1a32::Hasher hasher;
        data->hashInto(hasher);
        auto hash = hasher.getResult();

        // We don't know in general what the contents is or means... but we can display a hash
        HexDumpUtil::dump(uint32_t(hash), m_writer.getWriter());
        m_writer.put(" ");

        m_writer.put("\n");
    }

    RiffDumpContext(WriterHelper writer, Chunk* rootChunk)
        : m_writer(writer), m_indent(0), m_rootChunk(rootChunk)
    {
    }

    void _dumpIndent()
    {
        for (int i = 0; i < m_indent; ++i)
        {
            m_writer.put("  ");
        }
    }
    void _dumpRiffType(FourCC fourCC)
    {
        char c[5];
        for (int i = 0; i < 4; ++i)
        {
            c[i] = char(fourCC);
            fourCC >>= 8;
        }
        c[4] = 0;
        m_writer.put(c);
    }

    Chunk* m_rootChunk;

    int m_indent;
    WriterHelper m_writer;
};

} // namespace

/* static */ void RiffUtil::dump(RiffContainer::Chunk* chunk, WriterHelper writer)
{
    RiffDumpContext context(writer, chunk);
    context.dumpChunk(chunk);
}

/* static */ SlangResult RiffUtil::write(
    RiffContainer::ListChunk* list,
    bool isRoot,
    Stream* stream)
{
    RiffListHeader listHeader;

    listHeader.chunk.type = isRoot ? RiffFourCC::kRiff : RiffFourCC::kList;
    listHeader.chunk.size = uint32_t(list->getPayloadSize());
    listHeader.subType = list->getType();

    // Write the header
    SLANG_RETURN_ON_FAIL(stream->write(&listHeader, sizeof(listHeader)));

    // Write the contained chunks
    Chunk* chunk = list->m_firstChild;
    while (chunk)
    {
        switch (chunk->m_kind)
        {
        case Chunk::Kind::List:
            {
                auto listChunk = static_cast<ListChunk*>(chunk);
                // It's a container
                SLANG_RETURN_ON_FAIL(write(listChunk, false, stream));
                break;
            }
        case Chunk::Kind::Data:
            {
                auto dataChunk = static_cast<DataChunk*>(chunk);

                // Must be a regular chunk with data
                RiffHeader chunkHeader;
                chunkHeader.type = dataChunk->getType();
                chunkHeader.size = uint32_t(dataChunk->getPayloadSize());

                SLANG_RETURN_ON_FAIL(stream->write(&chunkHeader, sizeof(chunkHeader)));

                RiffContainer::DataBlock* dataBlock = dataChunk->m_firstDataBlock;
                while (dataBlock)
                {
                    SLANG_RETURN_ON_FAIL(
                        stream->write(dataBlock->getPayload(), dataBlock->getSize()));

                    // Next but of data
                    dataBlock = dataBlock->m_next;
                }

                // Need to write for alignment
                const size_t remainingSize =
                    getPaddedSize(dataChunk->getPayloadSize()) - dataChunk->getPayloadSize();

                if (remainingSize)
                {
                    static const uint8_t trailing[kRiffPadSize] = {0};
                    SLANG_RETURN_ON_FAIL(stream->write(trailing, remainingSize));
                }
            }
        default:
            break;
        }

        // Next
        chunk = chunk->m_next;
    }

    return SLANG_OK;
}

/* static */ SlangResult RiffUtil::write(RiffContainer* container, Stream* stream)
{
    return write(container->getRootChunk(), true, stream);
}

/* static */ SlangResult RiffUtil::read(Stream* stream, RiffContainer& outContainer)
{
    typedef RiffBuilder::ScopeChunk ScopeChunk;
    outContainer.reset();

    RiffBuilder builder(outContainer);

    size_t remaining;
    {
        RiffListHeader header;

        SLANG_RETURN_ON_FAIL(readHeader(stream, header));
        if (!isListType(header.chunk.type))
        {
            return SLANG_FAIL;
        }

        remaining =
            getPaddedSize(header.chunk.size) - (sizeof(RiffListHeader) - sizeof(RiffHeader));
        builder.beginChunk(Chunk::Kind::List, header.subType);
    }

    List<size_t> remainingStack;
    while (true)
    {
        // It must be the end
        if (remaining == 0)
        {
            // If it's a container then we pop container
            builder.endChunk();
            if (remainingStack.getCount() <= 0)
            {
                break;
            }

            remaining = remainingStack.getLast();
            remainingStack.removeLast();
        }
        else
        {
            RiffListHeader header;
            SLANG_RETURN_ON_FAIL(readHeader(stream, header));

            // The amount of data can't be larger than what remains
            if (header.chunk.size > remaining)
            {
                return SLANG_FAIL;
            }

            if (header.chunk.type == RiffFourCC::kList)
            {
                if (header.chunk.size & kRiffPadMask)
                {
                    SLANG_ASSERT(!"A list chunk can only have divisible by 2 size");
                    return SLANG_FAIL;
                }

                // Work out the pad size
                const size_t padSize = getPaddedSize(header.chunk.size);

                // Subtract the size of this chunk from remaining of the current chunk
                remaining -= sizeof(RiffHeader) + padSize;
                // Push it, for when we hit the end
                remainingStack.add(remaining);

                // Work out how much remains in this container
                remaining = padSize - (sizeof(RiffListHeader) - sizeof(RiffHeader));

                // Start a container
                builder.beginListChunk(header.subType);
            }
            else
            {
                ScopeChunk scopeChunk(builder, Chunk::Kind::Data, header.chunk.type);
                RiffContainer::DataBlock* data = builder.addDataBlock();

                builder.setPayload(data, nullptr, header.chunk.size);

                size_t readSize;
                SLANG_RETURN_ON_FAIL(
                    readPayload(stream, header.chunk.size, data->getPayload(), readSize));

                // All read sizes must end up aligned
                SLANG_ASSERT((readSize & kRiffPadMask) == 0);

                // Correct remaining
                remaining -= sizeof(RiffHeader) + readSize;
            }
        }
    }

    return SLANG_OK;
    //    return outContainer.isFullyConstructed() ? SLANG_OK : SLANG_FAIL;
}

// !!!!!!!!!!!!!!!!!!!!!!!!!!!!!! RiffContainer::Chunk !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

#if 0
SlangResult RiffContainer::Chunk::visit(Visitor* visitor)
{
    switch (m_kind)
    {
    case Kind::Data:
        {
            return visitor->handleData(static_cast<DataChunk*>(this));
        }
    case Kind::List:
        {
            auto list = static_cast<ListChunk*>(this);
            SLANG_RETURN_ON_FAIL(visitor->enterList(list));

            Chunk* chunk = list->m_containedChunks;
            while (chunk)
            {
                SLANG_RETURN_ON_FAIL(chunk->visit(visitor));

                chunk = chunk->m_next;
            }

            SLANG_RETURN_ON_FAIL(visitor->leaveList(list));
            return SLANG_OK;
        }
    default:
        return SLANG_FAIL;
    }
}

SlangResult RiffContainer::Chunk::visitPreOrder(VisitorCallback callback, void* data)
{
    switch (m_kind)
    {
    case Kind::Data:
        {
            return callback(this, data);
        }
    case Kind::List:
        {
            auto list = static_cast<ListChunk*>(this);
            // Do this containing node first
            SLANG_RETURN_ON_FAIL(callback(this, data));

            // Do the contents next
            Chunk* chunk = list->m_containedChunks;
            while (chunk)
            {
                SLANG_RETURN_ON_FAIL(chunk->visitPreOrder(callback, data));
                chunk = chunk->m_next;
            }
            return SLANG_OK;
        }
    default:
        return SLANG_FAIL;
    }
}

SlangResult RiffContainer::Chunk::visitPostOrder(VisitorCallback callback, void* data)
{
    switch (m_kind)
    {
    case Kind::Data:
        {
            return callback(this, data);
        }
    case Kind::List:
        {
            auto list = static_cast<ListChunk*>(this);

            // Do the contents first
            Chunk* chunk = list->m_containedChunks;
            while (chunk)
            {
                SLANG_RETURN_ON_FAIL(chunk->visitPostOrder(callback, data));
                chunk = chunk->m_next;
            }
            // Then the list node (so a post order)
            SLANG_RETURN_ON_FAIL(callback(this, data));
            return SLANG_OK;
        }
    default:
        return SLANG_FAIL;
    }
}
#endif

size_t RiffContainer::Chunk::getTotalSize() const
{
    _validateCachedSize();
    return m_cachedTotalSize;
}

void RiffContainer::Chunk::_validateCachedSize() const
{
    if (m_cachedTotalSize != 0)
        return;

    switch (m_kind)
    {
    case Kind::Data:
        {
            // A `DataChunk` with no `DataBlock`s is allowed to
            // have a payload size of zero, so check for that
            // first, just in case:
            //
            auto dataChunk = static_cast<DataChunk const*>(this);
            m_cachedTotalSize = dataChunk->_calcTotalSize();
        }
        break;

    case Kind::List:
        {
            auto listChunk = static_cast<ListChunk const*>(this);
            m_cachedTotalSize = listChunk->_calcTotalSize();
        }
        break;

    default:
        SLANG_UNEXPECTED("RIFF chunk was neither list nor data");
        break;
    }
}

void RiffContainer::Chunk::_invalidateCachedSize() const
{
    auto chunk = this;
    while (chunk)
    {
        if (chunk->m_cachedTotalSize == 0)
            return;

        chunk->m_cachedTotalSize = 0;
        chunk = chunk->m_parent;
    }
}

#if 0
RiffContainer::DataBlock* RiffContainer::Chunk::getSingleData() const
{
    return (m_kind == Kind::Data) ? static_cast<const DataChunk*>(this)->getSingleData() : nullptr;
}
#endif

// !!!!!!!!!!!!!!!!!!!!!!!!!!! RiffContainer::ListChunk !!!!!!!!!!!!!!!!!!!!!!

size_t RiffContainer::ListChunk::_calcTotalSize() const
{
    // Have to include the part of the header not taken up by the RiffHeader
    size_t totalSize = sizeof(RiffListHeader);
    Chunk* chunk = m_firstChild;
    while (chunk)
    {
        size_t chunkSize = chunk->getTotalSize();
        size_t paddedChunkSize = RiffUtil::getPaddedSize(chunkSize);

        totalSize += paddedChunkSize;

        chunk = chunk->m_next;
    }
    return totalSize;
}

RiffContainer::Chunk* RiffContainer::ListChunk::findChunk(FourCC type) const
{
    Chunk* chunk = m_firstChild;
    while (chunk)
    {
        if (chunk->getType() == type)
        {
            return chunk;
        }
        chunk = chunk->m_next;
    }
    return nullptr;
}

#if 0
void RiffContainer::ListChunk::findContained(FourCC type, List<ListChunk*>& out)
{
    Chunk* chunk = m_firstChild;
    while (chunk)
    {
        if (chunk->m_fourCC == type && chunk->m_kind == Chunk::Kind::List)
        {
            out.add(static_cast<ListChunk*>(chunk));
        }
        chunk = chunk->m_next;
    }
}

void RiffContainer::ListChunk::findContained(FourCC type, List<DataChunk*>& out)
{
    Chunk* chunk = m_firstChild;
    while (chunk)
    {
        if (chunk->m_fourCC == type && chunk->m_kind == Chunk::Kind::Data)
        {
            out.add(static_cast<DataChunk*>(chunk));
        }
        chunk = chunk->m_next;
    }
}
#endif

RiffContainer::ListChunk* RiffContainer::ListChunk::findListChunk(FourCC type) const
{
    for (auto chunk = m_firstChild; chunk; chunk = chunk->m_next)
    {
        if (chunk->getType() != type)
            continue;

        auto listChunk = as<ListChunk>(chunk);
        if (!listChunk)
            continue;

        return listChunk;
    }
    return nullptr;
}

RiffContainer::DataChunk* RiffContainer::ListChunk::findDataChunk(FourCC type) const
{
    for (auto chunk = m_firstChild; chunk; chunk = chunk->m_next)
    {
        if (chunk->getType() != type)
            continue;

        auto dataChunk = as<DataChunk>(chunk);
        if (!dataChunk)
            continue;

        return dataChunk;
    }
    return nullptr;
}

RiffContainer::DataBlock* RiffContainer::ListChunk::_findDataBlock(FourCC type) const
{
    auto dataChunk = findDataChunk(type);
    if (!dataChunk)
        return nullptr;

    DataBlock* dataBlock = dataChunk->m_firstDataBlock;
    if (!dataBlock)
        return nullptr;

    // This operation does not support the case where the allocation
    // for the data is not contiguous.
    //
    // TODO: It should probably be made to handle that case, since
    // there is already support for *making* the data contiguous.
    //
    if (dataBlock->m_next)
        return nullptr;

    return dataBlock;
}

void* RiffContainer::ListChunk::findData(FourCC type, size_t minSize) const
{
    DataBlock* dataBlock = _findDataBlock(type);
    if (!dataBlock)
        return nullptr;

    if (dataBlock->m_size < minSize)
        return nullptr;

    return dataBlock->getPayload();
}

void* RiffContainer::ListChunk::_findDataArray(FourCC type, size_t elementSize, Count& outCount)
    const
{
    SLANG_ASSERT(elementSize >= 0);

    DataBlock* dataBlock = _findDataBlock(type);
    if (!dataBlock)
    {
        outCount = 0;
        return nullptr;
    }

    auto elementCount = dataBlock->getSize() / elementSize;
    outCount = Count(elementCount);
    return dataBlock->getPayload();
}

static RiffContainer::ListChunk* _findListRec(RiffContainer::ListChunk* list, FourCC subType)
{
    RiffContainer::Chunk* chunk = list->m_firstChild;
    while (chunk)
    {
        if (auto childList = as<RiffContainer::ListChunk>(chunk))
        {
            // Test if the child is the subtype, if so we are done
            if (childList->getType() == subType)
            {
                return childList;
            }
            auto found = _findListRec(childList, subType);
            if (found)
            {
                return found;
            }
        }
        chunk = chunk->m_next;
    }
    return nullptr;
}

/* static */ RiffContainer::ListChunk* RiffContainer::ListChunk::findListChunkRec(FourCC subType)
{
    return (getType() == subType) ? this : _findListRec(this, subType);
}

// !!!!!!!!!!!!!!!!!!!!!!!!!!! RiffContainer::DataChunk !!!!!!!!!!!!!!!!!!!!!!

RiffContainer::DataBlock* RiffContainer::DataChunk::getSingleData() const
{
    DataBlock* data = m_firstDataBlock;
    return (data && data->m_next == nullptr) ? data : nullptr;
}

RiffReadHelper RiffContainer::DataChunk::asReadHelper() const
{
    DataBlock* data = getSingleData();
    if (data)
    {
        return RiffReadHelper((const uint8_t*)data->getPayload(), data->getSize());
    }
    return RiffReadHelper(nullptr, 0);
}

void RiffContainer::DataChunk::hashInto(FNV1a32::Hasher& hasher) const
{
    DataBlock* data = m_firstDataBlock;
    while (data)
    {
        // This is a little contrived (in that we don't use the function getHashCode), but the
        // reason to be careful is we want the same result however many Data blocks there are.
        const char* buffer = (const char*)data->getPayload();
        const size_t size = data->getSize();

        hasher(buffer, size);

        data = data->m_next;
    }
}

size_t RiffContainer::DataChunk::_calcTotalSize() const
{
    size_t totalSize = sizeof(RiffHeader);
    DataBlock* data = m_firstDataBlock;
    while (data)
    {
        totalSize += data->getSize();
        data = data->m_next;
    }
    return totalSize;
}

void RiffContainer::DataChunk::getPayload(void* outData, size_t size) const
{
    SLANG_ASSERT(getPayloadSize() >= size);

    uint8_t* dst = (uint8_t*)outData;
    size_t sizeRemainingToRead = size;

    DataBlock* dataBlock = m_firstDataBlock;
    while (sizeRemainingToRead)
    {
        SLANG_ASSERT(dataBlock);
        auto blockSize = dataBlock->getSize();

        auto sizeToRead = blockSize;
        if (sizeToRead >= sizeRemainingToRead)
            sizeToRead = sizeRemainingToRead;

        ::memcpy(dst, dataBlock->getPayload(), sizeToRead);

        dst += sizeToRead;
        sizeRemainingToRead -= sizeToRead;

        dataBlock = dataBlock->m_next;
    }
}

bool RiffContainer::DataChunk::isEqual(const void* inData, size_t count) const
{
    const uint8_t* src = (const uint8_t*)inData;

    DataBlock* data = m_firstDataBlock;
    while (data)
    {
        const size_t size = data->getSize();
        // Can't have more content than remaining
        // Contents must match
        if (size > count || ::memcmp(src, data->getPayload(), size) != 0)
        {
            return false;
        }

        src += size;
        count -= size;

        // Next data block
        data = data->m_next;
    }

    // If match must be at the end
    return count == 0;
}

// !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! RiffContainer !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

RiffContainer::RiffContainer()
    : m_arena(4096)
{
    m_rootList = nullptr;
    //    m_listChunk = nullptr;
    //    m_dataChunk = nullptr;
}

void RiffContainer::reset()
{
    m_arena.reset();

    m_rootList = nullptr;
    //    m_listChunk = nullptr;
    //    m_dataChunk = nullptr;
}

RiffContainer::ListChunk* RiffContainer::_newListChunk(FourCC subType)
{
    SLANG_ASSERT(!RiffUtil::isListType(subType));

    ListChunk* chunk = (ListChunk*)m_arena.allocate(sizeof(ListChunk));
    chunk->init(subType);
    return chunk;
}

RiffContainer::DataChunk* RiffContainer::_newDataChunk(FourCC type)
{
    SLANG_ASSERT(!RiffUtil::isListType(type));

    DataChunk* chunk = (DataChunk*)m_arena.allocate(sizeof(DataChunk));
    chunk->init(type);
    return chunk;
}

void RiffContainer::ListChunk::_addChunk(Chunk* chunk)
{
    chunk->m_parent = this;
    Chunk*& next = m_lastChild ? m_lastChild->m_next : m_firstChild;
    SLANG_ASSERT(next == nullptr);

    next = chunk;
    m_lastChild = chunk;
}

void RiffBuilder::_addChunk(Chunk* chunk)
{
    if (auto parentChunk = as<ListChunk>(_currentChunk))
    {
        parentChunk->_addChunk(chunk);
    }
}

void RiffBuilder::setCurrentChunk(Chunk* chunk)
{
    SLANG_ASSERT(chunk);

    _currentChunk = chunk;
}

void RiffBuilder::beginChunk(Chunk::Kind kind, FourCC type)
{
    switch (kind)
    {
    case Chunk::Kind::Data:
        beginDataChunk(type);
        break;

    case Chunk::Kind::List:
        beginListChunk(type);
        break;
    }
}

void RiffBuilder::beginDataChunk(FourCC type)
{
    auto parentChunk = as<ListChunk>(_currentChunk);
    SLANG_ASSERT(parentChunk);

    _currentChunk = _container->addDataChunk(parentChunk, type);
}

/// Start a data chunk within an existing list chunk.
RiffContainer::DataChunk* RiffContainer::addDataChunk(ListChunk* parent, FourCC type)
{
    SLANG_ASSERT(parent != nullptr);

    DataChunk* chunk = _newDataChunk(type);
    parent->_addChunk(chunk);
    return chunk;
}


void RiffBuilder::beginListChunk(FourCC type)
{
    if (!_currentChunk)
    {
        _currentChunk = _container->addRootChunk(type);
        return;
    }

    auto parentChunk = as<ListChunk>(_currentChunk);
    SLANG_ASSERT(parentChunk);

    _currentChunk = _container->addListChunk(parentChunk, type);
}

/// Add a root list chunk.
RiffContainer::ListChunk* RiffContainer::addRootChunk(FourCC type)
{
    SLANG_ASSERT(!m_rootList);

    ListChunk* chunk = _newListChunk(type);
    m_rootList = chunk;
    return chunk;
}

RiffContainer::ListChunk* RiffContainer::addListChunk(ListChunk* parent, FourCC type)
{
    SLANG_ASSERT(parent);

    ListChunk* chunk = _newListChunk(type);
    parent->_addChunk(chunk);
    return chunk;
}

void RiffBuilder::endChunk()
{
    SLANG_ASSERT(_currentChunk);
    _currentChunk = _currentChunk->m_parent;
}

void RiffBuilder::addDataChunk(FourCC type, const void* payloadData, size_t payloadSize)
{
    beginDataChunk(type);
    addData(payloadData, payloadSize);
    endChunk();
}

void RiffBuilder::setPayload(DataBlock* data, const void* payload, size_t size)
{
    // We must be in a data chunk
    auto dataChunk = as<DataChunk>(_currentChunk);
    SLANG_ASSERT(dataChunk);

    _container->setPayload(dataChunk, data, payload, size);
}

void RiffContainer::setPayload(
    DataChunk* dataChunk,
    DataBlock* data,
    const void* payload,
    size_t size)
{
    // The data shouldn't be set up
    SLANG_ASSERT(data->m_ownership == Ownership::Uninitialized);

    dataChunk->_invalidateCachedSize();

    data->m_ownership = Ownership::Arena;
    data->m_size = size;

    if (size)
    {
        data->m_payload = m_arena.allocateAligned(size, kPayloadMinAlignment);
    }

    if (payload)
    {
        ::memcpy(data->m_payload, payload, size);
    }
}

void RiffBuilder::setUnowned(DataBlock* data, void* payload, size_t size)
{
    // We must be in a data chunk
    auto dataChunk = as<DataChunk>(_currentChunk);
    SLANG_ASSERT(dataChunk);

    // The data shouldn't be set up
    SLANG_ASSERT(data->m_ownership == RiffContainer::Ownership::Uninitialized);

    dataChunk->_invalidateCachedSize();

    data->m_ownership = RiffContainer::Ownership::NotOwned;
    data->m_size = size;
    data->m_payload = payload;
}

RiffContainer::DataBlock* RiffBuilder::addDataBlock()
{
    // We must be in a data chunk
    auto dataChunk = as<DataChunk>(_currentChunk);
    SLANG_ASSERT(dataChunk);

    return _container->addDataBlock(dataChunk);
}

RiffContainer::DataBlock* RiffContainer::addDataBlock(DataChunk* dataChunk)
{
    auto& arena = getMemoryArena();
    DataBlock* data = (DataBlock*)arena.allocate(sizeof(DataBlock));
    data->init();

    DataBlock*& next = dataChunk->m_lastDataBlock ? dataChunk->m_lastDataBlock->m_next
                                                  : dataChunk->m_firstDataBlock;
    SLANG_ASSERT(next == nullptr);

    // Add to linked list
    next = data;
    // Make this the new end
    dataChunk->m_lastDataBlock = data;
    return data;
}

void* RiffContainer::getPayload(DataChunk* dataChunk)
{
    auto dataBlock = makeSingleDataBlock(dataChunk);
    if (!dataBlock)
        return nullptr;

    SLANG_ASSERT(!dataBlock->m_next);

    return dataBlock->getPayload();
}

RiffContainer::DataBlock* RiffContainer::makeSingleDataBlock(DataChunk* dataChunk)
{
    // There is no data
    if (dataChunk->m_firstDataBlock == nullptr)
    {
        return nullptr;
    }

    if (dataChunk->m_firstDataBlock->m_next == nullptr)
    {
        return dataChunk->m_firstDataBlock;
    }

    {
        DataBlock* dataBlock = dataChunk->m_firstDataBlock;

        // Okay lets combine all into one block
        const size_t payloadSize = dataChunk->getPayloadSize();

        void* dst = m_arena.allocateAligned(payloadSize, kPayloadMinAlignment);
        dataChunk->getPayload(dst, payloadSize);

        // Remove other data blocks
        dataBlock->m_next = nullptr;
        // Make this the end
        dataChunk->m_lastDataBlock = dataBlock;

        // Point to the block with all of the data
        dataBlock->m_ownership = Ownership::Arena;
        dataBlock->m_payload = dst;
        dataBlock->m_size = payloadSize;

        return dataBlock;
    }
}

void RiffDataChunkBuilder::writeData(const void* data, size_t size)
{
    _container->addData(_chunk, data, size);
}

void RiffBuilder::addData(const void* data, size_t size)
{
    // We must be in a data chunk
    auto dataChunk = as<DataChunk>(_currentChunk);
    SLANG_ASSERT(dataChunk);

    _container->addData(dataChunk, data, size);
}

void RiffContainer::addData(DataChunk* dataChunk, const void* data, size_t size)
{
    auto& arena = getMemoryArena();

    dataChunk->_invalidateCachedSize();

    // Get the last data block
    DataBlock* endData = dataChunk->m_lastDataBlock;
    if (endData)
    {
        uint8_t* end = ((uint8_t*)endData->m_payload) + endData->m_size;
        // See if can just add to end of current data
        if (end == arena.getCursor() && arena.allocateCurrentUnaligned(size))
        {
            ::memcpy(end, data, size);
            endData->m_size += size;
            return;
        }
    }

    auto dataBlock = addDataBlock(dataChunk);
    setPayload(dataChunk, dataBlock, data, size);
}

/* static */ bool RiffContainer::isChunkOk(Chunk* chunk)
{
    if (auto listChunk = as<ListChunk>(chunk))
    {
        for (auto childChunk = listChunk->m_firstChild; childChunk; childChunk = childChunk->m_next)
        {
            if (!isChunkOk(childChunk))
                return false;
        }
    }

    if (auto cachedSize = chunk->m_cachedTotalSize)
    {
        chunk->_invalidateCachedSize();
        if (cachedSize != chunk->getTotalSize())
            return false;
    }

    return true;
}

#if 0
/* static */ void RiffContainer::calcAndSetSize(Chunk* chunk)
{
    if (auto listChunk = as<ListChunk>(chunk))
    {
        for (auto childChunk = listChunk->m_firstChild; childChunk; childChunk = childChunk->m_next)
        {
            calcAndSetSize(childChunk);
        }
    }

    chunk->m_payloadSize = chunk->calcPayloadSize();
}
#endif


} // namespace Slang
