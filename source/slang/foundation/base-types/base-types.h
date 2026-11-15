// Base Types exposes scalar conventions through its migrated Scalar Types child.
// The other services described in `base-types.md` still use their legacy headers.

#ifndef SLANG_FOUNDATION_BASE_TYPES_H
#define SLANG_FOUNDATION_BASE_TYPES_H

#include "slang/foundation/base-types/scalar-types/scalar-types.h"

namespace slang::foundation::base_types
{

// We reexport the child's declarations without introducing new types or maximum objects.
using scalar_types::Int32;
using scalar_types::UInt32;
using scalar_types::Int64;
using scalar_types::UInt64;
using scalar_types::Int;
using scalar_types::UInt;
using scalar_types::kMaxUInt;
using scalar_types::kMaxInt;
using scalar_types::PtrInt;
using scalar_types::Index;
using scalar_types::kMaxIndex;
using scalar_types::UIndex;
using scalar_types::Count;
using scalar_types::UCount;
using scalar_types::Byte;
using scalar_types::Size;
using scalar_types::Char8;
using scalar_types::Char16;
using scalar_types::Char32;

} // namespace slang::foundation::base_types

#endif // SLANG_FOUNDATION_BASE_TYPES_H
