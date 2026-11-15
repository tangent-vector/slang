// Digests provides fixed-size digest values, SHA1 computation, and value append adapters.
// The `slang::foundation::digests` names refer to the same declarations as the `Slang` names.

#pragma once
#include "core/slang-blob.h"
#include "core/slang-list.h"
#include "core/slang-string.h"
#include "slang.h"

namespace Slang
{

// # Digest Values and Conversion

/// `DigestUtil` converts between digest bytes and hexadecimal text.
struct DigestUtil
{
    /// Convert `digestSize` bytes to lowercase hexadecimal text.
    ///
    /// Returns a string with twice `digestSize` characters in the stored byte order.
    /// Requires a nonnegative `digestSize` and a non-null `digest` addressing that many readable
    /// bytes.
    /// Twice `digestSize` must fit in `SlangInt`.
    static String digestToString(const void* digest, SlangInt digestSize);

    /// Convert hexadecimal text to `digestSize` bytes in `digest`.
    ///
    /// Requires nonnegative lengths, a non-null `str` addressing `strLength` readable characters,
    /// and a non-null `digest` addressing `digestSize` writable bytes.
    /// Twice `digestSize` must fit in `SlangInt`.
    /// The input and output ranges must not overlap.
    /// Returns `true` if `strLength` is twice `digestSize` and every character is a hexadecimal
    /// digit; both uppercase and lowercase digits are accepted. Otherwise, clears all
    /// `digestSize` output bytes to zero and returns `false`.
    // TODO(tfoley-refactor): Review the name and output-parameter naming so the existing bool
    // success result and mutation of the output buffer are apparent at call sites.
    static bool stringToDigest(
        const char* str,
        SlangInt strLength,
        void* digest,
        SlangInt digestSize);
};

/// A `HashDigest<N>` stores a fixed-size digest of `N` bytes inline.
///
/// `N` must be a multiple of four. Equality and binary or hexadecimal conversion use every byte.
/// `toString` and the constructors that parse text require twice `N` to fit in `SlangInt`.
/// For inputs that meet a parsing constructor's preconditions, an invalid length or hexadecimal
/// digit produces an all-zero value.
template<SlangInt N>
class HashDigest
{
public:
    // TODO(tfoley-refactor): Clarify whether zero-byte digests are supported; the size constraint
    // accepts zero, but `getHashCode` requires a first stored word.
    static_assert(N % 4 == 0, "size must be multiple of 4");

    /// The `N` digest bytes are stored as words and initialized to zero.
    uint32_t data[N / 4] = {0};

    /// Create a digest whose bytes are all zero.
    HashDigest() = default;

    /// Parse a non-null, null-terminated hexadecimal string.
    ///
    /// Requires the string length to fit in `SlangInt`.
    /// An invalid length or hexadecimal digit produces an all-zero digest.
    // TODO(tfoley-refactor): Review how the parsing constructors report length or digit errors;
    // they discard the converter's bool result, making those errors indistinguishable from a zero
    // digest.
    HashDigest(const char* str) { DigestUtil::stringToDigest(str, ::strlen(str), data, N); }

    /// Parse the complete hexadecimal contents of `str`.
    ///
    /// An invalid length or hexadecimal digit produces an all-zero digest.
    HashDigest(const String& str)
    {
        DigestUtil::stringToDigest(str.getBuffer(), str.getLength(), data, N);
    }

    /// Parse the hexadecimal character range in `str`.
    ///
    /// Requires `str.begin()` to be non-null, even for an empty range.
    /// An invalid length or hexadecimal digit produces an all-zero digest.
    /// The character storage is not retained.
    HashDigest(const UnownedStringSlice& str)
    {
        DigestUtil::stringToDigest(str.begin(), str.getLength(), data, N);
    }

    /// Create a digest from a non-null blob.
    ///
    /// Copies the contents when the blob contains exactly `N` bytes; a different size produces
    /// an all-zero digest. The blob is not retained.
    HashDigest(ISlangBlob* blob)
    {
        if (blob->getBufferSize() == N)
        {
            ::memcpy(data, blob->getBufferPointer(), N);
        }
    }

    /// Return lowercase hexadecimal text for all stored bytes.
    String toString() const { return DigestUtil::digestToString(data, N); }

    /// Return an owning blob containing a copy of all stored bytes.
    ///
    /// May return null if blob storage cannot be allocated.
    ComPtr<ISlangBlob> toBlob() const { return RawBlob::create(data, sizeof(data)); }

    /// Test whether every stored byte equals the corresponding byte in `other`.
    bool operator==(const HashDigest& other) const
    {
        return ::memcmp(data, other.data, sizeof(data)) == 0;
    }

    /// Test whether any stored byte differs from the corresponding byte in `other`.
    bool operator!=(const HashDigest& other) const { return !(*this == other); }

    /// Return the first stored word as a lookup hash.
    ///
    /// Requires `N` to be at least four.
    uint32_t getHashCode() const { return data[0]; }
};

// # SHA1 Computation

/// A `SHA1` object represents an incremental SHA1 computation.
///
/// Construction and `init` start an empty computation. `update` appends bytes, and `finalize`
/// pads and changes the accumulated state to produce its digest. Starting a new independent
/// computation requires `init`.
///
/// The algorithm implementation follows https://www.ietf.org/rfc/rfc3174.txt.
class SHA1
{
public:
    /// `Digest` is the twenty-byte result of a SHA1 computation.
    using Digest = HashDigest<20>;

    /// Start an initialized empty computation.
    SHA1();

    /// Restart an initialized empty computation.
    void init();

    /// Append `size` bytes to the computation.
    ///
    /// A null `data` pointer or a zero `size` leaves the computation unchanged.
    /// Otherwise, `data` must address `size` readable bytes.
    void update(const void* data, SlangSizeT size);

    /// Finalize the accumulated computation and return its digest.
    ///
    /// This operation changes the accumulated state. A new independent computation requires
    /// `init`.
    Digest finalize();

    /// Compute the digest of one byte range.
    ///
    /// Requires a nonnegative `size`. A null `data` pointer produces the empty-input digest;
    /// otherwise, `data` must address `size` readable bytes.
    // TODO(tfoley-refactor): Review the signed byte-count signature; forwarding to `update`
    // converts `size` to its unsigned count type without checking negative input.
    static Digest compute(const void* data, SlangInt size);

private:
    void addByte(uint8_t x);
    void processBlock(const uint8_t* ptr);

    uint32_t m_index;
    uint64_t m_bits;
    uint32_t m_state[5];
    uint8_t m_buf[64];
};

// # Appending Values

/// A `DigestBuilder<Hash>` adapts values to the byte input of a stored digest computation.
///
/// `Hash` must be default-constructible and provide `update`, `finalize`, and a `Digest` type.
/// Each append overload requires its byte range to meet the preconditions of `Hash::update`.
/// The append operations add no lengths or separators. Callers that combine values supply any
/// framing they need. Arithmetic, enum, and list inputs contribute their host object
/// representation.
template<typename Hash>
struct DigestBuilder
{
public:
    /// Append `size` bytes from `data` through `Hash::update`.
    ///
    /// Requires a nonnegative `size` and the input range required by `Hash::update`.
    // TODO(tfoley-refactor): Review the signed byte-count signature and its conversion to the
    // count type accepted by `Hash::update`.
    void append(const void* data, SlangInt size) { m_hash.update(data, size); }

    /// Append the object bytes of an arithmetic or enum value passed by value.
    ///
    /// The byte representation depends on the input type and host environment.
    template<
        typename T,
        typename std::enable_if<std::is_arithmetic<T>::value || std::is_enum<T>::value, int>::type =
            0>
    void append(const T value)
    {
        append(&value, sizeof(T));
    }

    /// Append the characters in `str` without a terminator or length prefix.
    void append(const String& str) { append(str.getBuffer(), str.getLength()); }

    /// Append the character range in `str` without a terminator or length prefix.
    void append(const StringSlice& str) { append(str.begin(), str.getLength()); }

    /// Append the borrowed character range in `str` without a terminator or length prefix.
    void append(const UnownedStringSlice& str) { append(str.begin(), str.getLength()); }

    /// Append the contents of a non-null blob.
    ///
    /// Requires the blob's byte count to fit in `SlangInt`.
    // TODO(tfoley-refactor): Review the implicit conversion of the blob's byte count to the
    // signed count accepted by the byte-range overload.
    void append(ISlangBlob* blob) { append(blob->getBufferPointer(), blob->getBufferSize()); }

    /// Append every stored byte of `digest`.
    template<SlangInt N>
    void append(const HashDigest<N>& digest)
    {
        append(digest.data, sizeof(digest.data));
    }

    /// Append the object bytes of every element in `list`, in element order.
    ///
    /// Requires an element type with unique object representations and a total byte count that
    /// fits in `SlangInt`.
    // TODO(tfoley-refactor): Review the count multiplication and conversion to the signed byte
    // count accepted by the byte-range overload.
    template<typename T, std::enable_if_t<std::has_unique_object_representations_v<T>, int> = 0>
    void append(const List<T>& list)
    {
        append(list.getBuffer(), list.getCount() * sizeof(T));
    }

    /// Finalize the stored computation and return its digest.
    ///
    /// State changes and restart requirements follow `Hash::finalize`.
    typename Hash::Digest finalize() { return m_hash.finalize(); }

private:
    Hash m_hash;
};
} // namespace Slang

namespace slang::foundation::digests
{

using ::Slang::DigestBuilder;
using ::Slang::DigestUtil;
using ::Slang::HashDigest;
using ::Slang::SHA1;

} // namespace slang::foundation::digests
