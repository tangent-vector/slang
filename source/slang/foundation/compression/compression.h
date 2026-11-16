#ifndef SLANG_FOUNDATION_COMPRESSION_H
#define SLANG_FOUNDATION_COMPRESSION_H

// Compression converts byte buffers using raw DEFLATE or the LZ4 block format.
// Compression returns owned blobs; decompression writes into storage supplied by the caller.
// Archive structure and the original uncompressed byte count belong to the caller.

#include "slang.h"

namespace slang::foundation::compression
{

// # Encoding Selection and Preferences

/// A `CompressionStyle` requests a compression level or a speed/size preference.
///
/// The buffer implementations in this module ignore the request. Archive writers may interpret it.
struct CompressionStyle
{
    /// The kind of compression preference selected by `m_type`.
    enum class Type
    {
        Level,           ///< Request the compression level in `m_level`.
        BestSpeed,       ///< Request a preference for compression speed.
        BestCompression, ///< Request a preference for compressed size.
        Default,         ///< Request the implementation's default balance of speed and size.
    };

    /// The requested kind of compression preference.
    Type m_type = Type::Default;

    /// The requested level from zero (least compression) to one (most compression).
    ///
    /// This field is ignored unless `m_type` is `Type::Level`.
    float m_level = 1.0f;
};

/// A `CompressionSystemType` identifies an encoding or uncompressed archive storage.
///
/// RIFF archive metadata stores these encoding identifiers.
enum class CompressionSystemType
{
    None,    ///< Uncompressed storage, without a compression-system instance.
    Deflate, ///< Raw DEFLATE data, without a zlib header or trailer.
    LZ4,     ///< An LZ4 block, without an LZ4 frame header.
    CountOf, ///< The sentinel after the encoding identifiers.
};

// # Buffer Conversion

/// An `ICompressionSystem` converts buffers using one identified encoding.
///
/// The inherited `ISlangUnknown` methods provide interface discovery and reference counting
/// according to the implementation's lifetime contract. The two singleton accessors below return
/// statically stored implementations.
class ICompressionSystem : public ISlangUnknown
{
    SLANG_COM_INTERFACE(
        0xcc935840,
        0xe059,
        0x4bb8,
        {0xa2, 0x2d, 0x92, 0x7b, 0x3c, 0x73, 0x8f, 0x85})

    /// Return the encoding used by this compression system.
    virtual SLANG_NO_THROW CompressionSystemType SLANG_MCALL getSystemType() = 0;

    /// Compress the supplied input into an owned blob.
    ///
    /// Requires `srcSizeInBytes` readable bytes at `src` during the call and a writable `outBlob`.
    /// On `SLANG_OK`, `*outBlob` receives a blob reference that the caller releases. The result may
    /// be larger than the input and need not have a terminating zero byte. A failure result does
    /// not imply that `*outBlob` was written.
    /// The supplied implementations do not retain `src` after returning.
    /// The supplied DEFLATE and LZ4 implementations ignore `style`, which may be null for them.
    /// The LZ4 implementation requires `srcSizeInBytes` to be at most `0x7e000000` bytes.
    virtual SLANG_NO_THROW SlangResult SLANG_MCALL compress(
        const CompressionStyle* style,
        const void* src,
        size_t srcSizeInBytes,
        ISlangBlob** outBlob) = 0;

    /// Decompress a buffer using this system's encoding into caller-provided storage.
    ///
    /// Requires `compressedSizeInBytes` readable bytes at `compressed` and writable storage for
    /// `decompressedSizeInBytes` bytes at `outDecompressed`, valid for the duration of the call.
    /// `decompressedSizeInBytes` must equal the original uncompressed byte count. The supplied LZ4
    /// implementation requires both byte counts to fit in `int`.
    /// The supplied implementations provide no general guarantee for overlapping ranges.
    /// Raw DEFLATE can copy between the input and output with `memcpy`, whose copied spans must
    /// not overlap. LZ4's supported in-place layouts are codec-specific; readable input and
    /// writable output alone do not establish that an overlapping layout is valid.
    /// Returns `SLANG_OK` on success and a failure result for a detected decoding error. The output
    /// may have been modified on failure.
    /// The supplied implementations do not retain either buffer after returning.
    virtual SLANG_NO_THROW SlangResult SLANG_MCALL decompress(
        const void* compressed,
        size_t compressedSizeInBytes,
        size_t decompressedSizeInBytes,
        void* outDecompressed) = 0;
};

// # Available Systems

/// `DeflateCompressionSystem` provides access to the raw DEFLATE compression system.
// TODO(tfoley-refactor): Give this singleton-access class a name that distinguishes it from an
// `ICompressionSystem` instance.
class DeflateCompressionSystem
{
public:
    /// Return the statically stored raw DEFLATE system in this library or executable.
    ///
    /// The pointer is valid until static destruction or unloading of the containing library.
    /// Its `addRef` and `release` return one and do not extend or control that lifetime.
    static ICompressionSystem* getSingleton();
};

/// `LZ4CompressionSystem` provides access to the LZ4 block compression system.
// TODO(tfoley-refactor): Give this singleton-access class a name that distinguishes it from an
// `ICompressionSystem` instance.
class LZ4CompressionSystem
{
public:
    /// Return the statically stored LZ4 block system in this library or executable.
    ///
    /// The pointer is valid until static destruction or unloading of the containing library.
    /// Its `addRef` and `release` return one and do not extend or control that lifetime.
    static ICompressionSystem* getSingleton();
};

} // namespace slang::foundation::compression

#endif // SLANG_FOUNDATION_COMPRESSION_H
