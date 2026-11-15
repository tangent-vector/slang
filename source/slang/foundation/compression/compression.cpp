// Compression implements the shared buffer interface with raw DEFLATE and LZ4 block codecs.
// Each section defines one concrete singleton and its conversion operations. We copy miniz's
// temporary allocation into a blob and release that allocation with `mz_free`. LZ4 writes into a
// `Slang::ScopedAllocation` whose ownership transfers to the result blob.

#include "slang/foundation/compression/compression.h"

#include "core/slang-blob.h"
#include "core/slang-smart-pointer.h"
#include "slang-com-helper.h"
#include "slang-com-ptr.h"

// TODO(tfoley-refactor): Document why this translation unit suppresses miniz's zlib-compatible
// function names before including the buffer APIs.
#define MINIZ_NO_ZLIB_COMPATIBLE_NAMES 1
#include <lz4.h>
#include <miniz.h>

namespace slang::foundation::compression
{

// # Raw DEFLATE Buffers

// TODO(tfoley-refactor): Explain how interface discovery and constant reference-count results
// relate to the concrete singleton's static lifetime.
/// A `DeflateCompressionSystemImpl` adapts raw DEFLATE buffers to `ICompressionSystem`.

class DeflateCompressionSystemImpl : public Slang::RefObject, public ICompressionSystem
{
public:
    // ISlangUnknown
    // override ref counting, as singleton
    /// Expose the inherited unknown interface and the shared compression interface.
    SLANG_IUNKNOWN_QUERY_INTERFACE

    /// Return one without changing the singleton's lifetime.
    SLANG_NO_THROW uint32_t SLANG_MCALL addRef() SLANG_OVERRIDE { return 1; }
    /// Return one without changing the singleton's lifetime.
    SLANG_NO_THROW uint32_t SLANG_MCALL release() SLANG_OVERRIDE { return 1; }

    // ICompressionSystem
    /// Return the identifier for raw DEFLATE buffers.
    virtual SLANG_NO_THROW CompressionSystemType SLANG_MCALL getSystemType() SLANG_OVERRIDE
    {
        return CompressionSystemType::Deflate;
    }
    /// Compress the input as raw DEFLATE, ignoring `style`.
    virtual SLANG_NO_THROW SlangResult SLANG_MCALL compress(
        const CompressionStyle* style,
        const void* src,
        size_t srcSizeInBytes,
        ISlangBlob** outBlob) SLANG_OVERRIDE;
    /// Decompress raw DEFLATE into the caller's output buffer.
    virtual SLANG_NO_THROW SlangResult SLANG_MCALL decompress(
        const void* compressed,
        size_t compressedSizeInBytes,
        size_t decompressedSizeInBytes,
        void* outDecompressed) SLANG_OVERRIDE;

protected:
    /// Return this compression interface for the unknown or compression UUID, otherwise null.
    ICompressionSystem* getInterface(const Slang::Guid& guid);
};

ICompressionSystem* DeflateCompressionSystemImpl::getInterface(const Slang::Guid& guid)
{
    return (guid == ISlangUnknown::getTypeGuid() || guid == ICompressionSystem::getTypeGuid())
               ? static_cast<ICompressionSystem*>(this)
               : nullptr;
}

// TODO(tfoley-refactor): Explain the raw DEFLATE flag selection and the allocation/failure
// sequence around the existing miniz-to-blob ownership explanation.
SlangResult DeflateCompressionSystemImpl::compress(
    const CompressionStyle* style,
    const void* src,
    size_t srcSizeInBytes,
    ISlangBlob** outBlob)
{
    SLANG_UNUSED(style);

    size_t compressedSizeInBytes;

    const int flags = 0;
    void* compressed =
        tdefl_compress_mem_to_heap(src, srcSizeInBytes, &compressedSizeInBytes, flags);

    if (!compressed)
    {
        return SLANG_FAIL;
    }

    // `compressed` is owned by miniz's allocator (allocated via `MZ_MALLOC`). Copy it into a
    // Slang-owned blob and release the original with miniz's own `mz_free`, rather than handing
    // it to `Slang::ScopedAllocation` (which always reclaims via `StandardAllocator`). This keeps
    // allocation and deallocation paired if miniz is ever built with a custom allocator (e.g.
    // redefined `MZ_MALLOC`/`MZ_FREE`).
    Slang::ComPtr<ISlangBlob> blob;
    const SlangResult res = Slang::RawBlob::tryCreate(compressed, compressedSizeInBytes, blob);
    mz_free(compressed);
    SLANG_RETURN_ON_FAIL(res);

    *outBlob = blob.detach();
    return SLANG_OK;
}

// TODO(tfoley-refactor): Explain the whole-buffer decoding strategy and failure sentinel.
// TODO(tfoley-refactor): Settle whether the original-size precondition also calls for
// checking miniz's returned byte count before changing the current success condition.
SlangResult DeflateCompressionSystemImpl::decompress(
    const void* compressed,
    size_t compressedSizeInBytes,
    size_t decompressedSizeInBytes,
    void* outDecompressed)
{
    const int flags = TINFL_FLAG_USING_NON_WRAPPING_OUTPUT_BUF;

    size_t size = tinfl_decompress_mem_to_mem(
        outDecompressed,
        decompressedSizeInBytes,
        compressed,
        compressedSizeInBytes,
        flags);
    if (size == TINFL_DECOMPRESS_MEM_TO_MEM_FAILED)
    {
        return SLANG_FAIL;
    }

    return SLANG_OK;
}

/* static */ ICompressionSystem* DeflateCompressionSystem::getSingleton()
{
    static DeflateCompressionSystemImpl impl;
    return &impl;
}

// # LZ4 Blocks

// TODO(tfoley-refactor): Explain how interface discovery and constant reference-count results
// relate to the concrete singleton's static lifetime.
/// An `LZ4CompressionSystemImpl` adapts LZ4 block buffers to `ICompressionSystem`.

class LZ4CompressionSystemImpl : public Slang::RefObject, public ICompressionSystem
{
public:
    // ISlangUnknown
    // override ref counting, as singleton
    /// Expose the inherited unknown interface and the shared compression interface.
    SLANG_IUNKNOWN_QUERY_INTERFACE
    /// Return one without changing the singleton's lifetime.
    SLANG_NO_THROW uint32_t SLANG_MCALL addRef() SLANG_OVERRIDE { return 1; }
    /// Return one without changing the singleton's lifetime.
    SLANG_NO_THROW uint32_t SLANG_MCALL release() SLANG_OVERRIDE { return 1; }

    // ICompressionSystem
    /// Return the identifier for LZ4 block buffers.
    virtual SLANG_NO_THROW CompressionSystemType SLANG_MCALL getSystemType() SLANG_OVERRIDE
    {
        return CompressionSystemType::LZ4;
    }
    /// Compress the input as an LZ4 block, ignoring `style`.
    virtual SLANG_NO_THROW SlangResult SLANG_MCALL compress(
        const CompressionStyle* style,
        const void* src,
        size_t srcSizeInBytes,
        ISlangBlob** outBlob) SLANG_OVERRIDE;
    /// Decompress an LZ4 block into the caller's output buffer.
    virtual SLANG_NO_THROW SlangResult SLANG_MCALL decompress(
        const void* compressed,
        size_t compressedSizeInBytes,
        size_t decompressedSizeInBytes,
        void* outDecompressed) SLANG_OVERRIDE;

protected:
    /// Return this compression interface for the unknown or compression UUID, otherwise null.
    ICompressionSystem* getInterface(const Slang::Guid& guid);
};

ICompressionSystem* LZ4CompressionSystemImpl::getInterface(const Slang::Guid& guid)
{
    return (guid == ISlangUnknown::getTypeGuid() || guid == ICompressionSystem::getTypeGuid())
               ? static_cast<ICompressionSystem*>(this)
               : nullptr;
}

// TODO(tfoley-refactor): Explain how the codec bound determines the allocation and why the
// buffer is resized before its ownership transfers into the result blob.
// TODO(tfoley-refactor): Audit the unchecked size narrowing and allocation and codec failures
// before changing this path's unconditional success result.
SlangResult LZ4CompressionSystemImpl::compress(
    const CompressionStyle* style,
    const void* src,
    size_t srcSizeInBytes,
    ISlangBlob** outBlob)
{
    SLANG_UNUSED(style);
    const size_t compressedBound = LZ4_compressBound(int(srcSizeInBytes));

    Slang::ScopedAllocation alloc;
    void* compressedData = alloc.allocate(compressedBound);

    const int compressedSize = LZ4_compress_default(
        (const char*)src,
        (char*)compressedData,
        int(srcSizeInBytes),
        int(compressedBound));
    alloc.reallocate(compressedSize);

    auto blob = Slang::RawBlob::moveCreate(alloc);

    *outBlob = blob.detach();
    return SLANG_OK;
}

// TODO(tfoley-refactor): Explain the whole-block codec call and its narrowed size inputs
// before the existing explanation of the decoded-size check.
SlangResult LZ4CompressionSystemImpl::decompress(
    const void* compressed,
    size_t compressedSizeInBytes,
    size_t decompressedSizeInBytes,
    void* outDecompressed)
{
    const int decompressedSize = LZ4_decompress_safe(
        (const char*)compressed,
        (char*)outDecompressed,
        int(compressedSizeInBytes),
        int(decompressedSizeInBytes));
    // LZ4_decompress_safe returns the number of bytes written, or a negative
    // value on malformed input. Reject a negative result or a short/long
    // decompression instead of returning success with partially initialized
    // bytes, which the caller would hand to the deserializer.
    if (decompressedSize < 0 || size_t(decompressedSize) != decompressedSizeInBytes)
        return SLANG_FAIL;

    return SLANG_OK;
}

/* static */ ICompressionSystem* LZ4CompressionSystem::getSingleton()
{
    static LZ4CompressionSystemImpl impl;
    return &impl;
}

} // namespace slang::foundation::compression
