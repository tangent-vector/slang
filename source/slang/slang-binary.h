// slang-binary.h
#ifndef SLANG_BINARY_H
#define SLANG_BINARY_H

#include "../core/slang-hash.h"
#include "slang-serialize.h"

namespace Slang
{
    namespace Binary
    {
        using HashCode = FNV1a32::HashCode;

        SLANG_FORCE_INLINE HashCode hash(
            UnownedStringSlice const& text)
        {
            return FNV1a32::hash(text.begin(), text.getLength());
        }

        struct StringTableEntry
        {
            UInt32 endOffsetOfData;
        };

        struct MangledNameTableEntry
        {
            UInt32 parentEntryIndex;
            UInt32 sizeInBytesOfPrefixSharedWithParentEntry;
            UInt32 endOffsetOfOfDataForSuffix;
            HashCode hash;
            UInt32 declID;
        };
    }

} // namespace Slang

#endif
