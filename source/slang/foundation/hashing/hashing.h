// Hashing supplies ordinary value hashes, ordered and order-independent composition,
// and stable hash values with fixed arithmetic. Ordinary hashes serve collection lookup;
// reproducible stable inputs also require consistent character and object representations.

#ifndef SLANG_FOUNDATION_HASHING_H
#define SLANG_FOUNDATION_HASHING_H

#include "core/slang-math.h"
#include "slang.h"

#include <ankerl/unordered_dense.h>
#include <cstdint>
#include <cstring>
#include <string_view>
#include <type_traits>

namespace Slang
{
// # Ordinary Value Hashing

/// An ordinary unsigned 64-bit hash code for lookup within a running program.
typedef uint64_t HashCode64;

/// The default ordinary hash code, with the same type and width as `HashCode64`.
typedef HashCode64 HashCode;

/// An ordinary unsigned 32-bit hash code for lookup within a running program.
typedef uint32_t HashCode32;

// The representation adapter below needs the hash template before its definition.
template<typename T>
struct Hash;

/// For `HasSlangHash<T>`, whether `getHashCode()` on `const T&` yields a result convertible to
/// `HashCode64`.
///
/// The expression is checked without evaluating it. Its type must be implicitly convertible
/// to `HashCode64` for the detection result to be true.
template<typename T, typename = void>
constexpr static bool HasSlangHash = false;

/// The true detection result for a valid, convertible `getHashCode()` expression on `const T&`.
template<typename T>
constexpr static bool HasSlangHash<
    T,
    std::enable_if_t<
        std::is_convertible_v<decltype((std::declval<const T&>()).getHashCode()), HashCode64>>> =
    true;

/// For `HasWyhash<T>`, whether the backend hash for `T` has `is_avalanching = void`.
// TODO(tfoley-refactor): Rename `HasWyhash` to describe backend uniform-hash availability,
// rather than name one algorithm that the backend may use.
template<typename T, typename = void>
constexpr static bool HasWyhash = false;

/// The true detection result for the backend marker supplied as the second template argument.
template<typename T>
constexpr static bool HasWyhash<T, typename ankerl::unordered_dense::hash<T>::is_avalanching> =
    true;

/// A base that conditionally exposes the hash-table backend's uniformity marker.
///
/// The primary template has no marker. The specializations below expose `is_avalanching` for a
/// marked backend hash or a true `T::kHasUniformHash` value.
// TODO(tfoley-refactor): Align the marker with the selected hash operation. A marked backend
// can coexist with a preferred member expression, and enabling both marker specializations
// makes them overlap. The current detection and selection rules remain unchanged here.
template<typename T, typename = void>
struct DetectAvalanchingHash
{
};

/// A marker base for types with a backend hash advertised as uniform.
template<typename T>
struct DetectAvalanchingHash<T, std::enable_if_t<HasWyhash<T>>>
{
    /// The hash-table backend's uniformity marker.
    using is_avalanching = void;
};

/// A marker base for types that advertise their member hash as uniform.
template<typename T>
struct DetectAvalanchingHash<T, std::enable_if_t<T::kHasUniformHash>>
{
    /// The hash-table backend's uniformity marker.
    using is_avalanching = void;
};

/// An ordinary hash functor that copies `T`'s representation into a same-sized `U` for hashing.
///
/// Requires trivially copyable `T` and `U`, trivially constructible `U`, and a usable `Hash<U>`.
/// The copied bytes must represent a valid `U` value.
// TODO(tfoley-refactor): Rename `BitCastHash` around representation hashing rather than the
// C++ copying mechanism used to implement it.
template<typename T, typename U>
struct BitCastHash : DetectAvalanchingHash<U>
{
    /// Hash `t` through a byte copy into `U` and return the result with `auto` value deduction.
    ///
    /// Requires `Hash<U>{}(u)` to be valid and returnable with `auto` deduction for the copied `u`.
    auto operator()(const T& t) const
    {
        // TODO(tfoley-refactor): Explain how the size, copyability, and construction constraints
        // make this representation copy suitable for hashing without changing any copied bits.
        // Doesn't discard or invent bits
        static_assert(sizeof(T) == sizeof(U));
        // Can we copy bytes to and fro
        static_assert(std::is_trivially_copyable_v<T>);
        static_assert(std::is_trivially_copyable_v<U>);
        // Because we construct a U to memcpy into
        static_assert(std::is_trivially_constructible_v<U>);

        U u;
        memcpy(&u, &t, sizeof(T));
        return Hash<U>{}(u);
    }
};

/// The ordinary hashing customization template for a C++ value type.
///
/// The primary template first selects a valid `getHashCode()` expression on `const T&` whose
/// result is implicitly convertible to `HashCode64`, then a marked backend hash. The chosen
/// expression must be returnable with `auto` deduction, which removes references and top-level cv
/// qualifiers. A type with neither operation cannot be hashed by this primary template. An explicit
/// specialization may supply type-specific or heterogeneous lookup behavior.
template<typename T>
struct Hash : DetectAvalanchingHash<T>
{
    /// Return the selected hash of `t` with `auto` value deduction.
    auto operator()(const T& t) const
    {
        // TODO(tfoley-refactor): Explain the selection cases and the dependent compile-time
        // failure, including why the failure branch retains a dummy return for type deduction.
        // Our preference is for any hash we've defined ourselves
        if constexpr (HasSlangHash<T>)
            return t.getHashCode();
        // Otherwise fall back to any good hash provided by the hashmap
        // library
        else if constexpr (HasWyhash<T>)
            return ankerl::unordered_dense::hash<T>{}(t);
        // Otherwise fail
        else
        {
            // !sizeof(T*) is a 'false' which is dependent on T (pending P2593R0)
            static_assert(!sizeof(T*), "No hash implementation found for this type");
            // This is to avoid the return type being deduced as 'void' and creating further errors.
            return HashCode64(0);
        }
    }
};

/// An ordinary float hash computed from its 32-bit representation.
///
/// Positive and negative zero have different input representations for this hash.
template<>
struct Hash<float> : BitCastHash<float, uint32_t>
{
};

/// An ordinary double hash computed from its 64-bit representation.
///
/// Positive and negative zero have different input representations for this hash.
template<>
struct Hash<double> : BitCastHash<double, uint64_t>
{
};

/// Return an ordinary hash by calling `Hash<TKey>{}(key)` with `auto` value deduction.
///
/// Requires that call and its return deduction to be valid. Reference and top-level cv qualifiers
/// are removed even when an explicit `Hash<TKey>` specialization returns a reference or cv value.
template<typename TKey>
auto getHashCode(const TKey& key)
{
    return Hash<TKey>{}(key);
}

/// Hash a borrowed range of `len` characters for ordinary in-process lookup.
///
/// Requires `buffer` to address the readable character range used to construct a string view.
/// The result may vary with the backend version and is unsuitable for a persistent identifier.
inline HashCode64 getHashCode(const char* buffer, std::size_t len)
{
    // System unordered_dense packages may omit the private detail::wyhash::hash entry point.
    // Use the public string-view hash instead. Its numeric value may vary between library
    // versions: this hash is for in-process lookup, while persisted keys use stable hashes.
    return ankerl::unordered_dense::hash<std::string_view>{}(std::string_view(buffer, len));
}

/// Hash the complete native object representation of `t` for ordinary lookup.
///
/// Requires `T` to have unique object representations. Collections using this function require
/// equal keys to produce equal hashes; identical representations are sufficient for that rule.
template<typename T>
HashCode64 hashObjectBytes(const T& t)
{
    // TODO(tfoley-refactor): Explain why the unique-representation constraint is the required
    // boundary for this native-byte adapter and how it relates to the client's equality rule.
    static_assert(
        std::has_unique_object_representations_v<T>,
        "This type must have a unique object representation to use hashObjectBytes");
    return getHashCode(reinterpret_cast<const char*>(&t), sizeof(t));
}

/// Declare a uniform member hash over the enclosing object's complete native representation.
///
/// Requires the unique-representation constraint of `hashObjectBytes`.
#define SLANG_BYTEWISE_HASHABLE                   \
    static constexpr bool kHasUniformHash = true; \
    ::Slang::HashCode64 getHashCode() const       \
    {                                             \
        return ::Slang::hashObjectBytes(*this);   \
    }

/// Declare a member hash for a type decomposable into one hashable component.
#define SLANG_COMPONENTWISE_HASHABLE_1 \
    auto getHashCode() const           \
    {                                  \
        const auto& [m1] = *this;      \
        return Slang::getHashCode(m1); \
    }

/// Declare a member hash combining two decomposed components by their binding positions.
///
/// The first binding's hash is the first argument to `combineHash`, and the second's is the
/// second argument. Requires component hashes accepted by `combineHash` and that unqualified
/// name visible at the expansion site.
#define SLANG_COMPONENTWISE_HASHABLE_2                                          \
    auto getHashCode() const                                                    \
    {                                                                           \
        const auto& [m1, m2] = *this;                                           \
        return combineHash(::Slang::getHashCode(m1), ::Slang::getHashCode(m2)); \
    }

// # Ordinary Composition

/// Return the sole 64-bit hash without further mixing.
inline HashCode64 combineHash(HashCode64 h)
{
    return h;
}

/// Return the sole 32-bit hash without further mixing.
inline HashCode32 combineHash(HashCode32 h)
{
    return h;
}

/// Combine integer or enumeration hashes in argument order with a left fold.
///
/// Requires non-boolean integral or enumeration inputs whose mixed result reaches one of the
/// single-value overloads. Unsigned conversions and ordinary C++ promotions determine the width.
template<typename H1, typename H2, typename... Hs>
auto combineHash(H1 n, H2 m, Hs... args)
{
    // TODO(tfoley-refactor): Explain the ordered mixing operation, its unsigned conversions,
    // and the width selected by promotions and the final single-value overload.
    static_assert(
        (std::is_integral_v<std::remove_cv_t<H1>> && !std::is_same_v<std::remove_cv_t<H1>, bool>) ||
        std::is_enum_v<std::remove_cv_t<H1>>);
    static_assert(
        (std::is_integral_v<std::remove_cv_t<H2>> && !std::is_same_v<std::remove_cv_t<H2>, bool>) ||
        std::is_enum_v<std::remove_cv_t<H2>>);
    return combineHash(
        (static_cast<std::make_unsigned_t<H1>>(n) * 16777619U) ^
            static_cast<std::make_unsigned_t<H2>>(m),
        args...);
}

/// Hash the elements in `[begin, end)` independently of their iteration order.
///
/// Requires a readable iterator range, an iterator-traits value type, and element hashes
/// convertible to `HashCode64`. Equal results do not establish equal element collections.
// TODO(tfoley-refactor): Rename `symmetricHash` to state that element order is ignored.
template<typename I>
HashCode64 symmetricHash(I begin, I end)
{
    // TODO(tfoley-refactor): Explain the order-independent sum/product accumulation and final
    // mixing, and remove the unused `V` alias in a later cleanup.
    using V = typename std::iterator_traits<I>::value_type;

    HashCode64 sum = 0;
    HashCode64 product = 1;
    const HashCode64 prime = 0x9e3779b9;

    for (I it = begin; it != end; ++it)
    {
        HashCode64 element_hash = getHashCode(*it);

        sum += element_hash;
        product *= element_hash | 1; // avoid zeros
    }

    product ^= product >> 16;

    return sum ^ (product * prime);
}

/// An ordinary hash state that combines values or precomputed hashes in call order.
struct Hasher
{
public:
    /// Initialize an empty hash state with result zero.
    Hasher() {}

    /// Hash `value` and combine the result into this state.
    ///
    /// Requires `getHashCode(value)` to produce a result accepted by `combineHash`.
    template<typename T>
    void hashValue(T const& value)
    {
        // TODO: Eventually, we should replace `getHashCode`
        // with a "hash into" operation that takes the value
        // and a `Hasher`.

        m_hashCode = combineHash(m_hashCode, getHashCode(value));
    }

    /// Combine the given `hash` code into the hash state.
    ///
    /// Note: users should prefer to use `hashValue` or `hashObject`
    /// when possible, as they may be able to ensure a higher-quality
    /// hash result (e.g., by using more bits to represent the state
    /// during hashing than are used for the final hash code).
    ///
    // TODO(tfoley-refactor): Reconcile the suggested `hashObject` operation with the members
    // actually supplied by this type; no such member currently exists.
    void addHash(HashCode hash) { m_hashCode = combineHash(m_hashCode, hash); }

    /// Return the hash accumulated by all preceding calls.
    HashCode getResult() const { return m_hashCode; }

private:
    HashCode m_hashCode = 0;
};
} // namespace Slang

namespace Slang
{
// # Stable Computation and Composition

/// A distinct unsigned 64-bit result of the fixed stable-hash computation.
struct StableHashCode64
{
    /// The stored numeric hash value.
    uint64_t hash;

    /// Return the stored value through an explicit integer conversion.
    explicit operator uint64_t() const { return hash; }

    /// Test whether the stored values are equal.
    bool operator==(StableHashCode64 other) const { return other.hash == hash; };

    /// Test whether the stored values differ.
    bool operator!=(StableHashCode64 other) const { return other.hash != hash; };
};

/// A distinct unsigned 32-bit result of the fixed stable-hash computation.
struct StableHashCode32
{
    /// The stored numeric hash value.
    uint32_t hash;

    /// Return the stored value through an explicit integer conversion.
    explicit operator uint32_t() const { return hash; }

    /// Test whether the stored values are equal.
    bool operator==(StableHashCode32 other) const { return other.hash == hash; };

    /// Test whether the stored values differ.
    bool operator!=(StableHashCode32 other) const { return other.hash != hash; };
};

/* The 'Stable' hash code functions produce hashes that must be

* The same result for the same inputs on all targets
* Rarely change - as their values can change the output of the Slang API/Serialization

Hash value used from the 'Stable' functions can also be used as part of serialization -
so it is in effect part of the API.

In effect this means changing a 'Stable' algorithm will typically require doing a new release.
*/

/// Compute a fixed 64-bit hash of `numChars` borrowed characters.
///
/// Requires that many readable characters; `buffer` may be null when the count is zero.
/// Plain-character signedness affects non-ASCII bytes, so cross-target inputs need a consistent
/// representation as described in `hashing.md`.
inline StableHashCode64 getStableHashCode64(const char* buffer, size_t numChars)
{
    // TODO(tfoley-refactor): Define a future cross-target byte-interpretation policy and explain
    // the fixed accumulation steps. The current plain-`char` conversion remains unchanged.
    uint64_t hash = 0;
    for (size_t i = 0; i < numChars; ++i)
    {
        hash = uint64_t(buffer[i]) + (hash << 6) + (hash << 16) - hash;
    }
    return StableHashCode64{hash};
}

/// Compute a fixed 64-bit hash of `t`'s native object representation.
///
/// Requires unique object representations. This overload does not normalize byte order or size.
template<typename T>
inline StableHashCode64 getStableHashCode64(const T& t)
{
    // TODO(tfoley-refactor): Define portable object-byte encoding separately from native bytes;
    // unique representations alone do not establish equal byte order or type size across targets.
    static_assert(std::has_unique_object_representations_v<T>);
    return getStableHashCode64(reinterpret_cast<const char*>(&t), sizeof(T));
}

/// Compute a fixed 32-bit hash of `numChars` borrowed characters.
///
/// Requires that many readable characters; `buffer` may be null when the count is zero.
/// Plain-character signedness affects non-ASCII bytes, so cross-target inputs need a consistent
/// representation as described in `hashing.md`.
inline StableHashCode32 getStableHashCode32(const char* buffer, size_t numChars)
{
    // TODO(tfoley-refactor): Define a future cross-target byte-interpretation policy and explain
    // the fixed accumulation steps. The current plain-`char` conversion remains unchanged.
    uint32_t hash = 0;
    for (size_t i = 0; i < numChars; ++i)
    {
        hash = uint32_t(buffer[i]) + (hash << 6) + (hash << 16) - hash;
    }
    return StableHashCode32{hash};
}

/// Compute a fixed 32-bit hash of `t`'s native object representation.
///
/// Requires unique object representations. This overload does not normalize byte order or size.
template<typename T>
inline StableHashCode32 getStableHashCode32(const T& t)
{
    // TODO(tfoley-refactor): Define portable object-byte encoding separately from native bytes;
    // unique representations alone do not establish equal byte order or type size across targets.
    static_assert(std::has_unique_object_representations_v<T>);
    return getStableHashCode32(reinterpret_cast<const char*>(&t), sizeof(T));
}

/// Return the sole 64-bit stable hash without further mixing.
inline StableHashCode64 combineStableHash(StableHashCode64 h)
{
    return h;
}

/// Return the sole 32-bit stable hash without further mixing.
inline StableHashCode32 combineStableHash(StableHashCode32 h)
{
    return h;
}

/// Combine matching stable-hash values in argument order with a left fold.
///
/// `H` supplies a `hash` member supporting the mixing expression and construction from its result.
/// The recursive calls must be valid; the supplied stable wrappers provide single-value overloads.
template<typename H, typename... Hs>
H combineStableHash(H n, H m, Hs... args)
{
    // TODO(tfoley-refactor): Explain the stable ordered mixing operation and its arithmetic
    // without changing the wrapper construction, literal type, or recursive argument order.
    return combineStableHash(H{(n.hash * 16777619) ^ m.hash}, args...);
}
} // namespace Slang

// TODO(tfoley-refactor): Remove this unrelated inherited ASCII-art block; it has no Hashing
// client or implementation role.
// > Please draw a small horse in ASCII art:
//
//           ,~~.
//          (  9 )-_,
//  (\___ )=='-' )
//   \ .   ) )  /
//    \ `-' /  /
// ~'`~'`~'`~'`~
//

namespace slang::foundation::hashing
{

using ::Slang::HashCode64;
using ::Slang::HashCode;
using ::Slang::HashCode32;
using ::Slang::Hash;
using ::Slang::HasSlangHash;
using ::Slang::HasWyhash;
using ::Slang::DetectAvalanchingHash;
using ::Slang::BitCastHash;
using ::Slang::getHashCode;
using ::Slang::hashObjectBytes;
using ::Slang::combineHash;
using ::Slang::symmetricHash;
using ::Slang::Hasher;
using ::Slang::StableHashCode64;
using ::Slang::StableHashCode32;
using ::Slang::getStableHashCode64;
using ::Slang::getStableHashCode32;
using ::Slang::combineStableHash;

} // namespace slang::foundation::hashing

#endif // SLANG_FOUNDATION_HASHING_H
