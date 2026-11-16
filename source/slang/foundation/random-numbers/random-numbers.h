#ifndef SLANG_FOUNDATION_RANDOM_NUMBERS_H
#define SLANG_FOUNDATION_RANDOM_NUMBERS_H

// Random Numbers provides mutable seeded generators, scalar selection, and byte filling.
// The common generator family and the compact `Random` value type have distinct sequences.
// Repeating output requires the same generator, state, operations, and compatible arithmetic;
// these APIs do not specify a portable byte stream or a cryptographic generator.

#include "core/slang-smart-pointer.h"
#include "slang.h"

#include <stdlib.h>
#include <string.h>

namespace slang::foundation::random_numbers
{

// # Common Generator Interface

/// A `RandomGenerator` is a reference-counted interface to a mutable pseudorandom sequence.
///
/// Draws and resets are not synchronized; clients coordinate access to a shared generator.
/// Factory-created and cloned objects may be retained with `Slang::RefPtr`.
class RandomGenerator : public Slang::RefObject
{
public:
    /// Make a new heap-allocated generator at the same sequence state.
    ///
    /// The caller owns the result, which initially has no retained intrusive reference.
    virtual RandomGenerator* clone() = 0;

    /// Restart the sequence using `seed`.
    virtual void reset(int32_t seed) = 0;

    /// Return the next signed 32-bit value and advance the sequence.
    virtual int32_t nextInt32() = 0;

    /// Return a signed 64-bit value and advance the sequence.
    ///
    /// The default implementation consumes two `nextInt32` draws.
    virtual int64_t nextInt64();

    /// Return a floating-point value between zero and one, including both endpoints.
    ///
    /// The default implementation consumes one `nextInt32` draw.
    virtual float nextUnitFloat32();

    /// Return the next boolean value and advance the sequence.
    ///
    /// The default implementation consumes one `nextInt32` draw.
    virtual bool nextBool();

    /// Write the next `count` signed 32-bit values to `dst` and advance the sequence.
    ///
    /// The destination must have room for `count` values. A zero count consumes no draws.
    virtual void nextInt32s(int32_t* dst, size_t count) = 0;

    /// Return the next 32-bit value as unsigned and consume one `nextInt32` draw.
    uint32_t nextUInt32() { return uint32_t(nextInt32()); }

    /// Return a nonnegative 32-bit value and consume one `nextInt32` draw.
    // TODO(tfoley-refactor): Rename `nextPositiveInt32` to reflect that zero is included.
    int32_t nextPositiveInt32() { return nextInt32() & 0x7fffffff; }

    /// Return a nonnegative 64-bit value and consume one `nextInt64` draw.
    // TODO(tfoley-refactor): Rename `nextPositiveInt64` to reflect that zero is included.
    int64_t nextPositiveInt64() { return nextInt64() & SLANG_INT64(0x7fffffffffffffff); }

    /// Return a value from zero up to, but not including, `maxValue`.
    ///
    /// Requires `maxValue > 0`. A bound of one returns zero without consuming a draw.
    // TODO(tfoley-refactor): Decide whether bounded selection should remove modulo bias.
    // TODO(tfoley-refactor): Explain the positive bound and constant-result case in a narrative.
    int32_t nextInt32UpTo(int32_t maxValue)
    {
        SLANG_ASSERT(maxValue > 0);
        return (maxValue <= 1) ? 0 : (nextPositiveInt32() % maxValue);
    }

    /// Return a value from `min` up to, but not including, `max` when `max > min`.
    ///
    /// An empty, reversed, or single-value interval returns `min` without consuming a draw.
    int32_t nextInt32InRange(int32_t min, int32_t max);

    /// Return a value from `min` up to, but not including, `max` when `max > min`.
    ///
    /// An empty, reversed, or single-value interval returns `min` without consuming a draw.
    uint32_t nextUInt32InRange(uint32_t min, uint32_t max);

    /// Return a value from zero up to, but not including, `maxValue`.
    ///
    /// Requires `maxValue > 0`. A bound of one returns zero without consuming a draw.
    // TODO(tfoley-refactor): Decide whether bounded selection should remove modulo bias.
    // TODO(tfoley-refactor): Explain the positive bound and constant-result case in a narrative.
    int64_t nextInt64UpTo(int64_t maxValue)
    {
        SLANG_ASSERT(maxValue > 0);
        return (maxValue <= 1) ? 0 : (nextPositiveInt64() % maxValue);
    }

    /// Return a value from `min` up to, but not including, `max` when `max > min`.
    ///
    /// Requires `max - min` to be representable as `int64_t`. If that difference is at most
    /// one, the result is `min` and the generator consumes no draw.
    int64_t nextInt64InRange(int64_t min, int64_t max);

    /// Fill the writable `size` bytes at `dst` with generated data.
    ///
    /// A zero size consumes no draws. Repeating bytes requires the same generator state,
    /// request sizes and boundaries, destination alignment modulo four, and compatible host
    /// integer representation. Splitting a request can change subsequent draws.
    void nextData(void* dst, size_t size);

    /// Create a heap-allocated default generator initialized with `seed`.
    ///
    /// The caller owns the result, which initially has no retained intrusive reference.
    static RandomGenerator* create(int32_t seed);
};

// # Concrete Generator State and Lifetime

/// An `Mt19937RandomGenerator` is the concrete generator used by the common family.
///
/// It supports stack construction, value copying, and reference-counted heap ownership.
/// Copying or cloning its state permits independent continuation of the same sequence.
/// Its implementation is based on Mersenne Twister: https://en.wikipedia.org/wiki/Mersenne_Twister.
class Mt19937RandomGenerator : public RandomGenerator
{
public:
    /// `ThisType` names the concrete generator type for copying and allocation.
    typedef Mt19937RandomGenerator ThisType;

    /// Constants describing the generator's state storage.
    enum
    {
        /// The number of words in the generator's state vector.
        kNumEntries = 624
    };

    /// Make a new heap-allocated generator at the same sequence state.
    ///
    /// The caller owns the result, which initially has no retained intrusive reference.
    Mt19937RandomGenerator* clone() SLANG_OVERRIDE { return new ThisType(*this); }

    /// Restart the sequence using `seed`.
    void reset(int32_t seed) SLANG_OVERRIDE;

    /// Return the next signed 32-bit value and advance the sequence by one value.
    int32_t nextInt32() SLANG_OVERRIDE;

    /// Write the next `count` values to `dst` in the order of repeated `nextInt32` calls.
    ///
    /// The destination must have room for `count` values. A zero count consumes no draws.
    void nextInt32s(int32_t* dst, size_t count) SLANG_OVERRIDE;

    /// Initialize a generator with the fixed seed `21452`.
    Mt19937RandomGenerator();

    /// Initialize an independent generator at the sequence state of `rhs`.
    Mt19937RandomGenerator(const ThisType& rhs);

    /// Initialize a generator with `seed`.
    explicit Mt19937RandomGenerator(int32_t seed);

    /// Copy the sequence state of `rhs` without changing this object's reference count.
    ///
    /// Self-assignment passes the same state array to `memcpy`, which requires nonoverlapping
    /// copied ranges. This implementation does not provide a self-assignment guarantee.
    // TODO(tfoley-refactor): Review the overlapping `memcpy` ranges during self-assignment.
    // TODO(tfoley-refactor): Consider returning `ThisType&` from assignment for C++ conventions.
    void operator=(const ThisType& rhs)
    {
        m_index = rhs.m_index;
        ::memcpy(m_mt, rhs.m_mt, sizeof(m_mt));
    }

protected:
    /// Replace the state vector with the next block and reset its read index to zero.
    // TODO(tfoley-refactor): Rename `_generate` to identify generation of a state block.
    void _generate();

    /// The words that hold the current block of generator state.
    // TODO(tfoley-refactor): Rename `m_mt` to identify the generator's state vector.
    uint32_t m_mt[kNumEntries];

    /// The next state-vector index to read; a value at least `kNumEntries` requires a new block.
    int m_index;
};

/// `DefaultRandomGenerator` names the concrete generator used by `RandomGenerator::create`.
typedef Mt19937RandomGenerator DefaultRandomGenerator;

// # Compact Scalar Generator

/// A `Random` is a compact seeded scalar generator with a sequence distinct from the common family.
///
/// Value copying copies its current sequence state. Draws are not synchronized.
class Random
{
private:
    unsigned int seed;

public:
    /// Initialize the generator with `seed`.
    Random(int seed) { this->seed = seed; }

    /// Return the next integer between zero and `RandMax`, including both endpoints.
    // TODO(tfoley-refactor): Rename `Next` to use the project's lowerCamelCase convention.
    // TODO(tfoley-refactor): Explain the state recurrence and output mask in a running narrative.
    int Next() { return ((seed = ((seed << 12) + 150889L) % 714025) & 0x7fff); }

    /// Return the next integer from `min` up to, but not including, `max`.
    ///
    /// Requires `max > min` and `max - min` to be representable as `int`.
    /// Each call advances the sequence twice, including a single-value interval.
    // TODO(tfoley-refactor): Rename `Next` to use the project's lowerCamelCase convention.
    // TODO(tfoley-refactor): Review signed/unsigned range conversion and modulo bias separately.
    // TODO(tfoley-refactor): Explain the two state updates and word combination in a narrative.
    int Next(int min, int max)
    {
        unsigned int a = ((seed = ((seed << 12) + 150889L) % 714025) & 0xFFFF);
        unsigned int b = ((seed = ((seed << 12) + 150889L) % 714025) & 0xFFFF);
        unsigned int r = (a << 16) + b;
        return min + r % (max - min);
    }

    /// Return a floating-point value between zero and one using two `Next` calls.
    ///
    /// The order of the two calls is not specified by the API.
    // TODO(tfoley-refactor): Rename `NextFloat` to use the project's lowerCamelCase convention.
    // TODO(tfoley-refactor): Review the unspecified order of the two `Next` calls.
    // TODO(tfoley-refactor): Explain the floating-point construction in a running narrative.
    float NextFloat() { return ((Next() << 15) + Next()) / ((float)(1 << 30)); }

    /// Return one `NextFloat` result scaled by `valMax - valMin` and offset by `valMin`.
    // TODO(tfoley-refactor): Rename `NextFloat` to use the project's lowerCamelCase convention.
    float NextFloat(float valMin, float valMax) { return valMin + (valMax - valMin) * NextFloat(); }

    /// Return the inclusive upper bound of the parameterless `Next` result without drawing.
    // TODO(tfoley-refactor): Rename `RandMax` to identify a query in lowerCamelCase.
    static int RandMax() { return 0x7fff; }
};

} // namespace slang::foundation::random_numbers

#endif // SLANG_FOUNDATION_RANDOM_NUMBERS_H
