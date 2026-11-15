// Scalar Types supplies integer representations and limits, sequence quantities, raw bytes,
// and character storage. The aliases name ordinary C++ types and do not validate values.

#ifndef SLANG_FOUNDATION_BASE_TYPES_SCALAR_TYPES_H
#define SLANG_FOUNDATION_BASE_TYPES_SCALAR_TYPES_H

// The product configuration hook and pointer-width checks run before our standard includes,
// as they do when clients include `core/slang-common.h`.
#include "slang.h"

#include <cstddef>
#include <stdint.h>

namespace Slang
{

// # Integer Representations and Limits

/// Signed 32-bit integer.
///
/// This type should be used when the exact size in bits is important (e.g., when dealing with
/// explicit binary file formats). Otherwise prefer the plain `Int` type or a semantically
/// richer type like `Count` or `Index`.
typedef int32_t Int32;

/// Unsigned 32-bit integer.
///
/// This type should be used when the exact size in bits is important (e.g., when dealing with
/// explicit binary file formats). Otherwise prefer the plain `Int` type or a semantically
/// richer type like `Count` or `Index`.
typedef uint32_t UInt32;

/// Signed 64-bit integer.
///
/// This type should be used when the exact size in bits is important (e.g., when dealing with
/// explicit binary file formats). Otherwise prefer the plain `Int` type or a semantically
/// richer type like `Count` or `Index`.
typedef int64_t Int64;

/// Unsigned 64-bit integer.
///
/// This type should be used when the exact size in bits is important (e.g., when dealing with
/// explicit binary file formats). Otherwise prefer the plain `Int` type or a semantically
/// richer type like `Count` or `Index`.
typedef uint64_t UInt64;

/// Default signed integer type for the Slang implementation.
///
/// This is the product API's `SlangInt`, with the same size as `void*` in the C++ implementation.
/// It is the preferred integer type when no more specific representation or sequence convention
/// applies.
typedef SlangInt Int;

/// Default unsigned integer type for the Slang implementation.
///
/// This is the product API's `SlangUInt`, with the same width as `Int`.
/// It is intended for uses that require an unsigned integer; the ordinary default is `Int`.
typedef SlangUInt UInt;

/// The maximum value representable by `UInt`.
static const UInt kMaxUInt = ~UInt(0);

/// The maximum value representable by `Int`.
static const Int kMaxInt = Int(kMaxUInt >> 1);

/// A signed integer type capable of representing a converted object pointer.
///
/// This is `intptr_t`, which need not be the same C++ type as the equally wide `Int`.
typedef intptr_t PtrInt;

// # Sequence Quantities

/// Default type for indices.
///
/// This is an alias for `Int`. An `Index` represents a position in a sequence or another ordinal
/// number; the consuming service determines whether negative values have a meaning.
typedef Int Index;

/// The maximum value representable by `Index`, equal to `kMaxInt`.
static const Index kMaxIndex = kMaxInt;

/// Unsigned equivalent of `Index`, with the same type as `UInt`.
///
/// It is intended for indices that specifically require an unsigned representation.
typedef UInt UIndex;

/// Default type for counts.
///
/// This is an alias for `Int`. A `Count` represents the number of elements in a container or
/// another cardinal number; the consuming service determines whether negative values have a
/// meaning.
typedef Int Count;

/// Unsigned equivalent of `Count`, with the same type as `UInt`.
///
/// It is intended for counts that specifically require an unsigned representation.
typedef UInt UCount;

// # Bytes and Byte Sizes

/// An unsigned character for raw byte storage and integer arithmetic.
///
/// `Byte` can access object representations under C++'s character-type aliasing rules.
/// Unlike `std::byte`, which supports bitwise operations, `Byte` also supports ordinary integer
/// arithmetic.
typedef unsigned char Byte;

/// Preferred integer type for sizes measured in bytes.
///
/// This unsigned type is `size_t`, matching the result of `sizeof` so byte sizes can be compared
/// without mixing signed and unsigned representations.
using Size = size_t;

// # Character Storage

// TODO(tfoley-refactor): Choose names for `Char8`, `Char16`, and `Char32` that distinguish
// code-unit storage from validated encoded characters or Unicode code points.

/// A `char` used for character storage, with the host's ordinary `char` signedness.
///
/// The alias does not validate UTF-8 sequences or guarantee that one value holds a complete
/// character.
typedef char Char8;

/// An unsigned 16-bit integer used for character-code-unit storage.
///
/// This is `uint16_t`, not C++'s distinct `char16_t` type. A value need not represent a complete
/// Unicode code point, and the alias does not validate UTF-16 sequences.
typedef uint16_t Char16;

/// An unsigned 32-bit integer capable of holding any Unicode code point.
///
/// This is `uint32_t`, not C++'s distinct `char32_t` type. Values outside the Unicode code-point
/// range are also representable; the alias does not validate them.
typedef uint32_t Char32;

} // namespace Slang

namespace slang::foundation::base_types::scalar_types
{

// We reexport the canonical `Slang` declarations so every qualification denotes the same type
// or translation-unit-local maximum object.
using Slang::Int32;
using Slang::UInt32;
using Slang::Int64;
using Slang::UInt64;
using Slang::Int;
using Slang::UInt;
using Slang::kMaxUInt;
using Slang::kMaxInt;
using Slang::PtrInt;
using Slang::Index;
using Slang::kMaxIndex;
using Slang::UIndex;
using Slang::Count;
using Slang::UCount;
using Slang::Byte;
using Slang::Size;
using Slang::Char8;
using Slang::Char16;
using Slang::Char32;

} // namespace slang::foundation::base_types::scalar_types

#endif // SLANG_FOUNDATION_BASE_TYPES_SCALAR_TYPES_H
