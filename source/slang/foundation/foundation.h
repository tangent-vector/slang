#ifndef SLANG_FOUNDATION_H
#define SLANG_FOUNDATION_H

// Foundation assembles the public APIs of its migrated children.
// The remaining services described in `foundation.md` still use their legacy headers.

#include "slang/foundation/base-types/base-types.h"
#include "slang/foundation/compression/compression.h"
#include "slang/foundation/digests/digests.h"
#include "slang/foundation/hashing/hashing.h"
#include "slang/foundation/random-numbers/random-numbers.h"

namespace slang::foundation
{

// We reexport Base Types' scalar declarations without introducing new types or maximum objects.
using base_types::Int32;
using base_types::UInt32;
using base_types::Int64;
using base_types::UInt64;
using base_types::Int;
using base_types::UInt;
using base_types::kMaxUInt;
using base_types::kMaxInt;
using base_types::PtrInt;
using base_types::Index;
using base_types::kMaxIndex;
using base_types::UIndex;
using base_types::Count;
using base_types::UCount;
using base_types::Byte;
using base_types::Size;
using base_types::Char8;
using base_types::Char16;
using base_types::Char32;

using compression::CompressionStyle;
using compression::CompressionSystemType;
using compression::DeflateCompressionSystem;
using compression::ICompressionSystem;
using compression::LZ4CompressionSystem;

using digests::DigestBuilder;
using digests::DigestUtil;
using digests::HashDigest;
using digests::SHA1;

using hashing::HashCode64;
using hashing::HashCode;
using hashing::HashCode32;
using hashing::Hash;
using hashing::HasSlangHash;
using hashing::HasWyhash;
using hashing::DetectAvalanchingHash;
using hashing::BitCastHash;
using hashing::getHashCode;
using hashing::hashObjectBytes;
using hashing::combineHash;
using hashing::symmetricHash;
using hashing::Hasher;
using hashing::StableHashCode64;
using hashing::StableHashCode32;
using hashing::getStableHashCode64;
using hashing::getStableHashCode32;
using hashing::combineStableHash;

using random_numbers::DefaultRandomGenerator;
using random_numbers::Mt19937RandomGenerator;
using random_numbers::Random;
using random_numbers::RandomGenerator;

} // namespace slang::foundation

#endif // SLANG_FOUNDATION_H
