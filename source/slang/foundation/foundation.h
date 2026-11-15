#ifndef SLANG_FOUNDATION_H
#define SLANG_FOUNDATION_H

// Foundation assembles the public APIs of its migrated children.
// The remaining services described in `foundation.md` still use their legacy headers.

#include "slang/foundation/random-numbers/random-numbers.h"

namespace slang::foundation
{

using random_numbers::DefaultRandomGenerator;
using random_numbers::Mt19937RandomGenerator;
using random_numbers::Random;
using random_numbers::RandomGenerator;

} // namespace slang::foundation

#endif // SLANG_FOUNDATION_H
