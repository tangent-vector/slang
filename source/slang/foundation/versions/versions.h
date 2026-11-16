// Versions provides numeric major/minor/patch values and best-effort candidate selection.
// The first section defines values, text conversion, ordering, and compatibility; the second
// defines requests and the operations that select supplied candidates and format request text.

#ifndef SLANG_SEMANTIC_VERSION_H
#define SLANG_SEMANTIC_VERSION_H

#include "core/slang-basic.h"
#include "slang/foundation/hashing/hashing.h"

namespace Slang
{

// # Numeric Version Values

// TODO(tfoley-refactor): Reconsider the `SemanticVersion` name so it does not imply versioning
// rules beyond the numeric representation and compatibility policy actually provided.

/// A numeric version with unsigned major, minor, and patch components.
///
/// Major and minor have 16 bits each, and patch has 32 bits. The all-zero value also represents
/// an unset version. The comparison operators order major, then minor, then patch; pre-release
/// identifiers and build metadata are not represented. Compatibility uses the directional policy
/// documented on `isBackwardsCompatibleWith`, rather than every rule of an external versioning
/// standard.
struct SemanticVersion
{
    /// The numeric version type.
    typedef SemanticVersion ThisType;

    /// The unsigned 64-bit numeric encoding of the three components.
    typedef UInt64 RawValue;

    /// Construct the all-zero, unset version.
    SemanticVersion()
        : m_major(0), m_minor(0), m_patch(0)
    {
    }

    // TODO(tfoley-refactor): Reconsider silent component narrowing in this constructor and the
    // setters of both version types without changing existing call-site conversions accidentally.

    /// Construct a version by narrowing the supplied integer components to their field widths.
    ///
    /// Missing minor and patch arguments default to zero. Arguments are converted directly to
    /// unsigned fields, including negative arguments; this constructor does not perform `set`'s
    /// nonnegative-input assertion.
    SemanticVersion(int inMajor, int inMinor = 0, int inPatch = 0)
        : m_major(uint16_t(inMajor)), m_minor(uint16_t(inMinor)), m_patch(uint32_t(inPatch))
    {
    }

    /// Replace all components with zero, making the version unset.
    void reset()
    {
        m_major = 0;
        m_minor = 0;
        m_patch = 0;
    }

    /// Return whether any component is nonzero.
    ///
    /// All zeros means nothing is set; there is no separate presence tag.
    bool isSet() const { return m_major || m_minor || m_patch; }

    /// Return the numeric encoding with major in bits 63-48, minor in 47-32, and patch in 31-0.
    ///
    /// Every stored component is represented. The encoding does not specify object byte order,
    /// and the signed conversion in `setRawValue` limits which encodings can be imported again.
    RawValue getRawValue() const
    {
        return (RawValue(m_major) << 48) | (RawValue(m_minor) << 32) | m_patch;
    }

    // TODO(tfoley-refactor): Remove the signed-int bottleneck in raw import after reviewing the
    // full-width encoding contract and callers; the present conversion cannot round-trip every
    // stored unsigned patch through `setRawValue` or `fromRaw`.

    /// Replace the components from a raw encoding, subject to the signed import limitation.
    ///
    /// The extracted patch is converted to `int` before storage. If that conversion produces a
    /// negative value, `set` applies its nonnegative assertion or optimizer assumption. Although
    /// `RawValue` can encode every unsigned 32-bit patch, this operation cannot import that entire
    /// range.
    void setRawValue(RawValue v)
    {
        set(int(v >> 48), int((v >> 32) & 0xffff), int(v & 0xffffffff));
    }

    /// Construct a version from a raw encoding with the same limitation as `setRawValue`.
    ///
    /// If converting the extracted patch to `int` produces a negative value, `set` applies its
    /// nonnegative assertion or optimizer assumption; full-width unsigned patch import is not
    /// supported.
    static SemanticVersion fromRaw(RawValue rawValue)
    {
        SemanticVersion result;
        result.setRawValue(rawValue);
        return result;
    }

    /// Replace the components after asserting that all supplied integers are nonnegative.
    ///
    /// Requires nonnegative arguments. Values beyond a field's width are still narrowed rather
    /// than rejected; patch defaults to zero. The assertion becomes an optimizer assumption in
    /// configurations without debug checks.
    void set(int major, int minor, int patch = 0)
    {
        SLANG_ASSERT(major >= 0 && minor >= 0 && patch >= 0);

        m_major = uint16_t(major);
        m_minor = uint16_t(minor);
        m_patch = uint32_t(patch);
    }

    /// Return the hash of the numeric raw encoding, equal for equal component triples.
    HashCode getHashCode() const { return Slang::getHashCode(getRawValue()); }

    /// Parse dot-separated components into `outVersion`, leaving it zero on failure.
    ///
    /// Uses the notation, component limits, and trailing-separator behavior of the overload
    /// accepting `separatorChar`, with `'.'` as the separator. Returns `SLANG_OK` on success
    /// and a failure result otherwise. The input characters are borrowed only for the call.
    static SlangResult parse(const UnownedStringSlice& value, SemanticVersion& outVersion);

    /// Parse components separated by `separatorChar` into `outVersion`.
    ///
    /// Splits `value` at `separatorChar` into one to three integer components. Each component uses
    /// decimal or `0x`/`0X` hexadecimal notation, optionally preceded by a sign. At least one digit
    /// is required, and whitespace is rejected. Values must be nonnegative, at most `0xffff` for
    /// major/minor and `0x7fffffff` for patch; signed zero is accepted. Missing lower components
    /// become zero. Parsing fails for empty interior components, invalid digits, integer overflow,
    /// or excess components. A trailing separator after one or two components is accepted, but one
    /// after three components fails: `"1."` and `"1.2."` succeed, while `"1.2.3."` fails for `'.'`.
    /// Resets `outVersion` before parsing and leaves it zero on every failure. Returns `SLANG_OK`
    /// on success and a failure result otherwise; borrows the input characters only for the call.
    static SlangResult parse(
        const UnownedStringSlice& value,
        char separatorChar,
        SemanticVersion& outVersion);

    /// Return the earliest supplied version, or the all-zero version when `count <= 0`.
    ///
    /// Positive counts require `versions` to identify that many readable elements. Input order
    /// and duplicates do not affect the result, and the input elements are not modified.
    static ThisType getEarliest(const ThisType* versions, Count count);

    /// Return the latest supplied version, or the all-zero version when `count <= 0`.
    ///
    /// Positive counts require `versions` to identify that many readable elements. Input order
    /// and duplicates do not affect the result, and the input elements are not modified.
    static ThisType getLatest(const ThisType* versions, Count count);

    /// Return whether this version has the same major and at least the minor of `otherVersion`.
    ///
    /// Patch is ignored, including when the other patch is larger, and the same rule applies
    /// when major is zero. This directional policy applies when code for this version loads data
    /// produced by `otherVersion` or provides its API, provided the service or format follows that
    /// policy. For example, when this version is `1.2.0`, returns true for `1.1.99` and `1.2.9`,
    /// but false for `1.3.0` and `2.0.0`.
    bool isBackwardsCompatibleWith(const ThisType& otherVersion) const;

    /// Append the decimal major and minor components, and the patch only when nonzero.
    ///
    /// Preserves the existing contents of `buf`. For example, appends `"1.2"` for `1.2.0` and
    /// `"1.2.3"` for `1.2.3`. Formatting supports the stored unsigned patch width, which
    /// exceeds the range accepted by parsing and raw import.
    void append(StringBuilder& buf) const;

    /// Return whether this version follows `rhs` in major/minor/patch order.
    bool operator>(const ThisType& rhs) const { return getRawValue() > rhs.getRawValue(); }

    /// Return whether this version equals or follows `rhs` in major/minor/patch order.
    bool operator>=(const ThisType& rhs) const { return getRawValue() >= rhs.getRawValue(); }

    /// Return whether this version precedes `rhs` in major/minor/patch order.
    bool operator<(const ThisType& rhs) const { return getRawValue() < rhs.getRawValue(); }

    /// Return whether this version equals or precedes `rhs` in major/minor/patch order.
    bool operator<=(const ThisType& rhs) const { return getRawValue() <= rhs.getRawValue(); }

    /// Return whether all three components equal those of `rhs`.
    bool operator==(const ThisType& rhs) const { return getRawValue() == rhs.getRawValue(); }

    /// Return whether any component differs from that of `rhs`.
    bool operator!=(const ThisType& rhs) const { return getRawValue() != rhs.getRawValue(); }

    /// The unsigned 16-bit major component, compared before minor and patch.
    uint16_t m_major;

    /// The unsigned 16-bit minor component, compared after major and before patch.
    uint16_t m_minor;

    /// The unsigned 32-bit patch component, which can be large for some code bases.
    ///
    /// Direct field storage and formatting support more patch values than parsing and raw import.
    uint32_t m_patch;
};

// # Version Match Requests

/// A preference for selecting a candidate version by age or specified numeric components.
///
/// Numeric requests specify major, major/minor, or major/minor/patch. `findAnyBest` can select
/// outside the specified components when no candidate lies in its preferred span; a request is
/// not a compatibility requirement. No text parser or formatting round-trip is provided.
struct MatchSemanticVersion
{
    /// The version match request type.
    typedef MatchSemanticVersion ThisType;

    /// The age preference or number of numeric components specified by a request.
    enum class Kind
    {
        /// An unspecified preference for the latest of multiple candidates.
        Unknown,

        /// A past preference for the earliest of multiple candidates.
        Past,

        /// A future preference for the latest of multiple candidates.
        Future,

        /// A numeric preference with only major specified.
        Major,

        /// A numeric preference with major and minor specified.
        MajorMinor,

        /// A numeric preference with all three components specified.
        MajorMinorPatch,
    };

    /// Return whether the request kind specifies all three components, even if they are zero.
    bool hasCompleteVersion() const { return m_kind == Kind::MajorMinorPatch; }

    /// Return whether the request kind specifies any numeric version components.
    bool hasVersion() const { return Index(m_kind) >= Index(Kind::Major); }

    /// Set a major-only request, with zero minor and patch components.
    ///
    /// Converts `major` to `int` and constructs the stored version with field-width narrowing,
    /// without the nonnegative assertion in `SemanticVersion::set`.
    void set(Index major)
    {
        m_kind = Kind::Major;
        m_version = SemanticVersion(int(major), 0, 0);
    }

    /// Set a major/minor request, with a zero patch component.
    ///
    /// Converts each argument to `int` and constructs the stored version with field-width
    /// narrowing, without the nonnegative assertion in `SemanticVersion::set`.
    void set(Index major, Index minor)
    {
        m_kind = Kind::MajorMinor;
        m_version = SemanticVersion(int(major), int(minor), 0);
    }

    /// Set a complete major/minor/patch request.
    ///
    /// Converts each argument to `int` and constructs the stored version with field-width
    /// narrowing, without the nonnegative assertion in `SemanticVersion::set`.
    void set(Index major, Index minor, Index patch)
    {
        m_kind = Kind::MajorMinorPatch;
        m_version = SemanticVersion(int(major), int(minor), int(patch));
    }

    /// Append the preference word or specified decimal components to `buf`.
    ///
    /// Preserves the existing builder contents. For nonnumeric preferences, appends `"unknown"`,
    /// `"past"`, or `"future"`. For numeric requests, appends their specified components; for a
    /// complete request, delegates to `SemanticVersion::append` and omits a zero patch without
    /// changing the kind. For example, appends `"1.2"` for a complete `1.2.0` request.
    void append(StringBuilder& buf) const;

    /// Construct a future preference with an all-zero stored version.
    static MatchSemanticVersion makeFuture()
    {
        MatchSemanticVersion version;
        version.m_kind = Kind::Future;
        return version;
    }

    /// Select a best-effort candidate, possibly outside the requested numeric components.
    ///
    /// Returns an all-zero version when `count <= 0`, and the sole candidate when `count == 1`
    /// regardless of the request. Positive counts require `versions` to identify that many readable
    /// elements; unsorted and duplicate candidates are accepted, and the input elements are not
    /// modified. For multiple candidates, returns the earliest for `Kind::Past` and the latest
    /// for `Kind::Unknown` or `Kind::Future`. For numeric requests, uses zero for unspecified
    /// components in the lower bound and increments the last specified component for an exclusive
    /// upper bound. The increment can narrow or wrap at a field's maximum rather than carry into
    /// another component. Returns the greatest candidate in that half-open span. If none lies in
    /// the span, returns the least candidate at or above the lower bound, or the greatest below it
    /// when no such following candidate exists. For example, prefers `2.1.7` over `2.1.3` for a
    /// `2.1` request, but can select `2.2` if no `2.1` candidate is available. An all-zero result
    /// may also be a real candidate.
    static SemanticVersion findAnyBest(
        const SemanticVersion* versions,
        Count count,
        const ThisType& matchVersion);

    /// Construct an unknown preference with an all-zero stored version.
    MatchSemanticVersion()
        : m_kind(Kind::Unknown)
    {
    }

    /// Construct a request retaining the supplied kind and stored version unchanged.
    ///
    /// Components not specified by `kind` do not affect formatting or selection.
    MatchSemanticVersion(Kind kind, const SemanticVersion& version)
        : m_kind(kind), m_version(version)
    {
    }

    /// The age preference or numeric specificity used for formatting and selection.
    Kind m_kind;

    /// The stored numeric components, interpreted according to `m_kind`.
    ///
    /// `append` and `findAnyBest` ignore all components for nonnumeric preferences and use only
    /// the specified components for numeric preferences.
    SemanticVersion m_version;
};

} // namespace Slang

namespace slang::foundation::versions
{

// We reexport the existing types because compiler descriptors and stored formats use their
// canonical `Slang` identities and representations.
using ::Slang::SemanticVersion;
using ::Slang::MatchSemanticVersion;

} // namespace slang::foundation::versions

#endif
