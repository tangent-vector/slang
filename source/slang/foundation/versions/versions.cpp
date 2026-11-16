// Versions implements numeric version conversion, extrema, compatibility, and candidate selection.
// Numeric value operations come first: parsing and formatting, earliest/latest queries, then the
// directional compatibility policy. The methods on `MatchSemanticVersion` then select among
// supplied candidates and append request text. Tool discovery and target policy remain with
// callers.

#include "slang/foundation/versions/versions.h"

#include "core/slang-string-util.h"
#include "slang-com-helper.h"

namespace Slang
{

// # Numeric Version Operations

// TODO(tfoley-refactor): Explain the parsing body's bounded split, temporary validation, and
// publication after all components succeed; its reset-before-parse behavior must remain clear.

SlangResult SemanticVersion::parse(
    const UnownedStringSlice& value,
    char separatorChar,
    SemanticVersion& outVersion)
{
    outVersion.reset();

    UnownedStringSlice slices[3];
    Index splitCount;
    // TODO(tfoley-refactor): Resolve the trailing-separator asymmetry with Text's bounded split
    // contract: `"1."` and `"1.2."` succeed, but `"1.2.3."` fails with the default separator.
    SLANG_RETURN_ON_FAIL(StringUtil::split(value, separatorChar, 3, slices, splitCount));
    if (splitCount <= 0)
    {
        return SLANG_FAIL;
    }

    Int ints[3] = {0, 0, 0};
    for (Index i = 0; i < splitCount; i++)
    {
        SLANG_RETURN_ON_FAIL(StringUtil::parseInt(slices[i], ints[i]));

        const Int max = (i == 2) ? 0x7fffffff : 0xffff;
        if (ints[i] < 0 || ints[i] > max)
        {
            return SLANG_FAIL;
        }
    }

    outVersion.m_major = uint16_t(ints[0]);
    outVersion.m_minor = uint16_t(ints[1]);
    outVersion.m_patch = uint32_t(ints[2]);

    return SLANG_OK;
}

SlangResult SemanticVersion::parse(const UnownedStringSlice& value, SemanticVersion& outVersion)
{
    return parse(value, '.', outVersion);
}

void SemanticVersion::append(StringBuilder& buf) const
{
    buf << Int32(m_major) << "." << Int32(m_minor);
    if (m_patch != 0)
    {
        buf << "." << UInt32(m_patch);
    }
}

// TODO(tfoley-refactor): Explain the extrema scans' initial candidate and running minimum/maximum
// invariant, including the all-zero result for nonpositive counts.
/* static */ SemanticVersion SemanticVersion::getEarliest(const ThisType* versions, Count count)
{
    if (count <= 0)
    {
        return SemanticVersion();
    }

    SemanticVersion bestVersion = versions[0];
    for (const auto version : makeConstArrayView(versions + 1, count - 1))
    {
        if (version < bestVersion)
        {
            bestVersion = version;
        }
    }
    return bestVersion;
}

/* static */ SemanticVersion SemanticVersion::getLatest(const ThisType* versions, Count count)
{
    if (count <= 0)
    {
        return SemanticVersion();
    }

    SemanticVersion bestVersion = versions[0];
    for (const auto version : makeConstArrayView(versions + 1, count - 1))
    {
        if (version > bestVersion)
        {
            bestVersion = version;
        }
    }
    return bestVersion;
}

bool SemanticVersion::isBackwardsCompatibleWith(const ThisType& otherVersion) const
{
    // Compatibility is not guaranteed across major revisions.
    //
    if (m_major != otherVersion.m_major)
        return false;

    // Within a given major revision, a version with a higher
    // minor revision is backwards-compatible with one that
    // has a lower minor revision, but not vice-versa.
    //
    if (m_minor < otherVersion.m_minor)
        return false;

    // If the major and minor revisions pass our check, then
    // we consider it a match. Note that this intentionally
    // doesn't check the patch version at all.
    //
    return true;
}


// # Match Selection and Request Formatting

// TODO(tfoley-refactor): Reconsider `findAnyBest`'s name so callers can see its best-effort
// preference and outside-span fallback rather than infer a strict compatibility search.
// TODO(tfoley-refactor): Explain the selection body's lower/upper bounds, sorted-copy scans, and
// fallback cases, including why empty and single-candidate ranges bypass the request.
// TODO(tfoley-refactor): Replace the maximum-component bound arithmetic only after choosing its
// intended selection policy; current increments narrow or wrap without carrying to another field.

/* static */ SemanticVersion MatchSemanticVersion::findAnyBest(
    const SemanticVersion* versions,
    Count count,
    const ThisType& matchVersion)
{
    // If there aren't any we are done
    if (count <= 0)
    {
        return SemanticVersion();
    }

    // If there is only one it must be the best
    if (count == 1)
    {
        return versions[0];
    }

    // Define a version range [start, end)
    SemanticVersion start, end;

    switch (matchVersion.m_kind)
    {
    case Kind::Past:
        {
            return SemanticVersion::getEarliest(versions, count);
        }
    case Kind::Unknown:
    case Kind::Future:
        {
            // If it's unknown, we just get the latest
            return SemanticVersion::getLatest(versions, count);
        }
    case Kind::Major:
        {
            start = SemanticVersion(matchVersion.m_version.m_major, 0, 0);
            end = SemanticVersion(matchVersion.m_version.m_major + 1, 0, 0);
            break;
        }
    case Kind::MajorMinor:
        {
            start =
                SemanticVersion(matchVersion.m_version.m_major, matchVersion.m_version.m_minor, 0);
            end = SemanticVersion(
                matchVersion.m_version.m_major,
                matchVersion.m_version.m_minor + 1,
                0);
            break;
        }
    case Kind::MajorMinorPatch:
        {
            start = SemanticVersion(matchVersion.m_version);
            end = SemanticVersion(
                matchVersion.m_version.m_major,
                matchVersion.m_version.m_minor,
                matchVersion.m_version.m_patch + 1);
            break;
        }
    default:
        break;
    }

    List<SemanticVersion> sortedVersions;
    sortedVersions.addRange(versions, count);

    // Sort into increasing values
    sortedVersions.sort(
        [&](const SemanticVersion& a, const SemanticVersion& b) -> bool { return a < b; });

    Index startIndex = 0;
    for (; startIndex < count && sortedVersions[startIndex] < start; ++startIndex)
        ;

    Index endIndex = startIndex;
    for (; endIndex < count && sortedVersions[endIndex] < end; ++endIndex)
        ;

    // If we have a span of versions, get the last in the span
    if (startIndex < endIndex)
    {
        // Get the last one
        return sortedVersions[endIndex - 1];
    }

    // Get the next greatest if there is one
    if (endIndex < count)
    {
        return sortedVersions[endIndex];
    }

    // Get the prior prior to the start
    if (startIndex > 0)
    {
        return sortedVersions[startIndex - 1];
    }

    // All cases should be covered, but return the last one
    return sortedVersions[count - 1];
}

void MatchSemanticVersion::append(StringBuilder& buf) const
{
    switch (m_kind)
    {
    default:
    case Kind::Unknown:
        buf << "unknown";
        break;
    case Kind::Past:
        buf << "past";
        break;
    case Kind::Future:
        buf << "future";
        break;
    case Kind::Major:
        {
            buf << m_version.m_major;
            break;
        }
    case Kind::MajorMinor:
        {
            buf << m_version.m_major << "." << m_version.m_minor;
            break;
        }
    case Kind::MajorMinorPatch:
        {
            m_version.append(buf);
            break;
        }
    }
}

} // namespace Slang
