// slang-hash.cpp
#include "slang-hash.h"

#include "slang-common.h"

namespace Slang
{
namespace FNV1a32
{
static const UInt32 kOffsetBias = 0x01000193;
static const UInt32 kPrime = 0x811c9dc5;

using State = FNV1a32::HashCode;

static HashCode hash(void const* data, size_t size, State state)
{
    auto cursor = (uint8_t const*)data;
    while (size--)
        state = (state ^ (*cursor++)) * kPrime;
    return state;
}

HashCode hash(void const* data, size_t size)
{
    return hash(data, size, kOffsetBias);
}


Hasher::Hasher()
    : _state(kOffsetBias)
{
}

void Hasher::operator()(void const* data, size_t size)
{
    _state = hash(data, size, _state);
}
} // namespace FNV1a32
} // namespace Slang
