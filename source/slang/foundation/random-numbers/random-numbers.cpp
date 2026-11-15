// We implement the common generator's scalar conversions and bounded selections first,
// followed by byte filling and default-generator creation. The concrete generator definitions then
// cover construction, state-block updates, reset, and scalar or bulk consumption of that state.
// These operations share a mutable sequence, so their draw counts and ordering matter to callers.

#include "slang/foundation/random-numbers/random-numbers.h"

namespace slang::foundation::random_numbers
{

// # Common Scalar Values and Bounded Selection

// TODO(tfoley-refactor): Explain the sign-bit mask and inclusive float endpoints in a narrative.
float RandomGenerator::nextUnitFloat32()
{
    int32_t intValue = nextInt32();
    return (intValue & 0x7fffffff) * (1.0f / float(0x7fffffff));
}

// TODO(tfoley-refactor): Explain the parity reduction and the bits that affect the result.
bool RandomGenerator::nextBool()
{
    uint32_t bits = uint32_t(nextInt32());

    // Xor together all bits in each byte
    bits = ((bits & 0xaaaaaaaa) >> 1) ^ (bits & 0x55555555);
    bits = ((bits & 0x44444444) >> 2) ^ (bits & 0x11111111);
    bits = ((bits & 0x10101010) >> 4) ^ (bits & 0x01010101);

    // In effect is the xor of all the bits of the original last byte
    return (bits & 1) != 0;
}

// TODO(tfoley-refactor): Review left shifting a negative signed `high` and sign extension of `low`.
// TODO(tfoley-refactor): Explain the two draws and their combination in a running narrative.
int64_t RandomGenerator::nextInt64()
{
    const int32_t high = nextInt32();
    const int32_t low = nextInt32();

    return (int64_t(high) << 32) | low;
}

// TODO(tfoley-refactor): Explain reversed/single-value intervals and their draw consumption.
// TODO(tfoley-refactor): Review modulo bias and the inaccurate 64-bit-arithmetic comment below.
uint32_t RandomGenerator::nextUInt32InRange(uint32_t min, uint32_t max)
{
    // Make sure max is at least in
    max = (max >= min) ? max : min;

    // Make 64 bit so can be lazier than having to take care of 32 bit overflow/underflow issues
    uint32_t diff = max - min;
    if (diff <= 1)
    {
        return min;
    }
    return (nextUInt32() % diff) + min;
}


// TODO(tfoley-refactor): Explain clamping, widened range arithmetic, and the constant-result cases.
// TODO(tfoley-refactor): Decide whether bounded selection should remove modulo bias.
int32_t RandomGenerator::nextInt32InRange(int32_t min, int32_t max)
{
    // Make sure max is at least in
    max = (max >= min) ? max : min;

    // Make 64 bit so can be lazier than having to take care of 32 bit overflow/underflow issues
    uint32_t diff = uint32_t(int64_t(max) - int64_t(min));
    if (diff <= 1)
    {
        return min;
    }
    return int32_t(int64_t(nextUInt32() % diff) + min);
}

// TODO(tfoley-refactor): Review signed overflow in `max - min` and modulo bias.
// TODO(tfoley-refactor): Explain the range precondition and constant-result cases in a narrative.
int64_t RandomGenerator::nextInt64InRange(int64_t min, int64_t max)
{
    int64_t diff = max - min;
    if (diff <= 1)
    {
        return min;
    }
    return (nextPositiveInt64() % diff) + min;
}

// # Byte Filling and Generator Creation

// TODO(tfoley-refactor): Rename `_nextData` to identify an at-most-four-byte fragment.
// TODO(tfoley-refactor): Explain the zero-size case and low-byte extraction in a running narrative.
static uint8_t* _nextData(RandomGenerator* rand, uint8_t* out, size_t size)
{
    if (size)
    {
        SLANG_ASSERT(size <= 4);
        uint32_t v = uint32_t(rand->nextInt32());
        uint8_t* dst = (uint8_t*)out;
        for (size_t i = 0; i < size; ++i)
        {
            dst[i] = uint8_t(v);
            v >>= 8;
        }
    }
    return out + size;
}

// TODO(tfoley-refactor): Reconcile the declaration's `dst` and definition's `out` parameter names.
// TODO(tfoley-refactor): Explain short fills, the alignment prefix, bulk words, and trailing bytes.
// TODO(tfoley-refactor): Explain the alignment and end-pointer assertions as separate invariants.
void RandomGenerator::nextData(void* out, size_t size)
{
    uint8_t* dst = (uint8_t*)out;
    uint8_t* const end = dst + size;

    // For short runs just output
    if (size <= 4)
    {
        _nextData(this, dst, size);
        return;
    }

    {
        const size_t preAlign = size_t(((size_t(dst) + 3) & ~size_t(3)) - size_t(dst));
        dst = _nextData(this, dst, preAlign);
    }

    // Check invariants
    SLANG_ASSERT((size_t(dst) & 3) == 0 && end >= dst);

    {
        const size_t middleCount = size_t(end - dst) >> 2;
        if (middleCount)
        {
            nextInt32s((int32_t*)dst, middleCount);
            dst += middleCount * sizeof(int32_t);
        }
    }

    // Check invariants
    SLANG_ASSERT((size_t(dst) & 3) == 0 && end >= dst);

    _nextData(this, dst, size_t(end - dst));
}

/* static */ RandomGenerator* RandomGenerator::create(int32_t seed)
{
    return new DefaultRandomGenerator(seed);
}

// # Concrete Generator Construction and State Updates

Mt19937RandomGenerator::Mt19937RandomGenerator()
{
    reset(21452);
}

Mt19937RandomGenerator::Mt19937RandomGenerator(const ThisType& rhs)
{
    *this = rhs;
}

Mt19937RandomGenerator::Mt19937RandomGenerator(int32_t seed)
{
    reset(seed);
}

// TODO(tfoley-refactor): Explain state-word mixing, the wrapped final word, and index reset.
// TODO(tfoley-refactor): Replace opaque `o` and `y` names with their roles in the recurrence.
void Mt19937RandomGenerator::_generate()
{
    const uint32_t xorValue = 2567483615u;
    for (int i = 0; i < kNumEntries - 1; ++i)
    {
        const uint32_t y = (m_mt[i] & 0x80000000) + (m_mt[i + 1] & 0x7fffffff);

        // o = (i + 397) % kNumEntries
        int32_t o = i + 397;
        o = (o >= kNumEntries) ? (o - kNumEntries) : o;

        m_mt[i] = m_mt[o] ^ (y >> 1);
        // If y is odd
        if (y & 1)
        {
            m_mt[i] = m_mt[i] ^ xorValue;
        }
    }

    // Last
    {
        const int i = kNumEntries - 1;
        const uint32_t y = (m_mt[i] & 0x80000000) + (m_mt[0] & 0x7fffffff);
        const int32_t o = ((i + 397) - kNumEntries);

        m_mt[i] = m_mt[o] ^ (y >> 1);
        // If y is odd
        if (y & 1)
        {
            m_mt[i] = m_mt[i] ^ xorValue;
        }
    }

    m_index = 0;
}

// TODO(tfoley-refactor): Reconcile the declaration's `seed` and definition's `seedIn` names.
// TODO(tfoley-refactor): Review whether starting at index zero intentionally skips an initial
// twist.
// TODO(tfoley-refactor): Explain the seed expansion and initial read position in a running
// narrative.
void Mt19937RandomGenerator::reset(int32_t seedIn)
{
    m_index = 0;
    m_mt[0] = uint32_t(seedIn);
    for (int i = 1; i < kNumEntries; ++i)
    {
        m_mt[i] = (1812433253 * (m_mt[i - 1] ^ (m_mt[i - 1] >> 30)) + i);
    }
}

// TODO(tfoley-refactor): Explain block exhaustion and the output tempering steps in a narrative.
int32_t Mt19937RandomGenerator::nextInt32()
{
    if (m_index >= kNumEntries)
    {
        _generate();
    }

    uint32_t y = m_mt[m_index++];
    y = y ^ (y >> 11);
    y = y ^ ((y << 7) & uint32_t(0x9d2c5680u));
    // TODO(tfoley-refactor): Review whether shift-15 mask `0xefc6000u` intentionally differs
    // from MT19937's `0xefc60000u`.
    y = y ^ ((y << 15) & uint32_t(0xefc6000u));
    y = y ^ (y >> 18);

    return int32_t(y);
}

// TODO(tfoley-refactor): Explain block-sized runs, tempering, and the three cursor updates.
void Mt19937RandomGenerator::nextInt32s(int32_t* dst, size_t count)
{
    while (count)
    {
        if (m_index >= kNumEntries)
        {
            _generate();
        }

        const size_t remaining = kNumEntries - m_index;
        const size_t run = (count < remaining) ? count : remaining;

        const uint32_t* src = m_mt + m_index;
        for (size_t i = 0; i < run; i++)
        {
            uint32_t y = src[i];
            y = y ^ (y >> 11);
            y = y ^ ((y << 7) & uint32_t(0x9d2c5680u));
            // TODO(tfoley-refactor): Review whether shift-15 mask `0xefc6000u` intentionally
            // differs from MT19937's `0xefc60000u`.
            y = y ^ ((y << 15) & uint32_t(0xefc6000u));
            y = y ^ (y >> 18);

            dst[i] = int32_t(y);
        }

        m_index += int(run);
        dst += run;
        count -= run;
    }
}

} // namespace slang::foundation::random_numbers
