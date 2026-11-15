#ifndef SLANG_FOUNDATION_H
#define SLANG_FOUNDATION_H

// Foundation assembles the public APIs of its migrated children.
// The remaining services described in `foundation.md` still use their legacy headers.

#include "slang/foundation/compression/compression.h"
#include "slang/foundation/digests/digests.h"
#include "slang/foundation/hashing/hashing.h"
#include "slang/foundation/random-numbers/random-numbers.h"

namespace slang::foundation
{

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
