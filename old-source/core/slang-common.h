#pragma once

#include "slang-signal.h"
#include "slang.h"

#include <cstddef>
#include <cstring>
#include <stdint.h>
#include <type_traits>

#define VARIADIC_TEMPLATE

#include "slang/foundation/base-types/scalar-types/scalar-types.h"

namespace Slang
{

template<typename T>
inline T&& _Move(T& obj)
{
    return static_cast<T&&>(obj);
}

template<typename T>
inline void Swap(T& v0, T& v1)
{
    T tmp = _Move(v0);
    v0 = _Move(v1);
    v1 = _Move(tmp);
}

// Make these interfaces have more convenient names
typedef ISlangCastable ICastable;
typedef ISlangClonable IClonable;

// Convenience function for using clonable
template<typename T>
SLANG_FORCE_INLINE T* clone(IClonable* clonable)
{
    return (T*)clonable->clone(T::getTypeGuid());
}

template<typename T>
inline bool isBitSet(T value, T bitToTest)
{
    static_assert(sizeof(T) <= sizeof(uint32_t), "Only support up to 32 bit enums");
    return (T)((uint32_t)value & (uint32_t)bitToTest) == bitToTest;
}

template<typename To, typename From>
typename std::enable_if_t<
    sizeof(To) == sizeof(From) && std::is_trivially_copyable_v<From> &&
        std::is_trivially_copyable_v<To> && std::is_trivially_constructible_v<To>,
    To>
bitCast(const From& src)
{
    To dst;
    std::memcpy(&dst, &src, sizeof(To));
    return dst;
}

} // namespace Slang

// SLANG_DEFER
template<typename F>
class SlangDeferImpl
{
    F f;

public:
    SlangDeferImpl(F&& f)
        : f(Slang::_Move(f))
    {
    }
    ~SlangDeferImpl() { f(); }
};

#ifndef SLANG_DEFER_LAMBDA
#define SLANG_DEFER_LAMBDA(x) auto SLANG_CONCAT(slang_defer_, __LINE__) = SlangDeferImpl(x)
#define SLANG_DEFER(x) auto SLANG_CONCAT(slang_defer_, __LINE__) = SlangDeferImpl([&]() { x; })
#endif

//
// Some macros for avoiding boilerplate
// TODO: could probably deduce the size with templates, and move the whole
// thing into a template
//
#if __cplusplus >= 202002L
#define SLANG_COMPONENTWISE_EQUALITY_1(type) bool operator==(const type& other) const = default;
#define SLANG_COMPONENTWISE_EQUALITY_2(type) bool operator==(const type& other) const = default;
#define SLANG_COMPONENTWISE_EQUALITY_3(type) bool operator==(const type& other) const = default;
#else
#define SLANG_COMPONENTWISE_EQUALITY_1(type) \
    bool operator==(const type& other) const \
    {                                        \
        const auto& [m1] = *this;            \
        const auto& [o1] = other;            \
        return m1 == o1;                     \
    }                                        \
    bool operator!=(const type& other) const \
    {                                        \
        return !(*this == other);            \
    }

#define SLANG_COMPONENTWISE_EQUALITY_2(type) \
    bool operator==(const type& other) const \
    {                                        \
        const auto& [m1, m2] = *this;        \
        const auto& [o1, o2] = other;        \
        return m1 == o1 && m2 == o2;         \
    }                                        \
    bool operator!=(const type& other) const \
    {                                        \
        return !(*this == other);            \
    }

#define SLANG_COMPONENTWISE_EQUALITY_3(type)     \
    bool operator==(const type& other) const     \
    {                                            \
        const auto& [m1, m2, m3] = *this;        \
        const auto& [o1, o2, o3] = other;        \
        return m1 == o1 && m2 == o2 && m3 == o3; \
    }                                            \
    bool operator!=(const type& other) const     \
    {                                            \
        return !(*this == other);                \
    }
#endif

// TODO: Shouldn't these be SLANG_ prefixed?
#ifdef _MSC_VER
#define UNREACHABLE_RETURN(x)
#else
#define UNREACHABLE_RETURN(x) return x;
#endif

#if SLANG_GCC
#define SLANG_EXHAUSTIVE_SWITCH_BEGIN \
    _Pragma("GCC diagnostic push");   \
    _Pragma("GCC diagnostic error \"-Wswitch-enum\"");
#define SLANG_EXHAUSTIVE_SWITCH_END _Pragma("GCC diagnostic pop");
#elif SLANG_CLANG
#define SLANG_EXHAUSTIVE_SWITCH_BEGIN \
    _Pragma("clang diagnostic push"); \
    _Pragma("clang diagnostic error \"-Wswitch-enum\"");
#define SLANG_EXHAUSTIVE_SWITCH_END _Pragma("clang diagnostic pop");
#elif SLANG_VC
#define SLANG_EXHAUSTIVE_SWITCH_BEGIN \
    _Pragma("warning(push)");         \
    _Pragma("warning(error : 4062)");
#define SLANG_EXHAUSTIVE_SWITCH_END _Pragma("warning(pop)");
#else
#define SLANG_EXHAUSTIVE_SWITCH_BEGIN
#define SLANG_EXHAUSTIVE_SWITCH_END
#endif

#if SLANG_GCC
#define SLANG_ALLOW_DEPRECATED_BEGIN \
    _Pragma("GCC diagnostic push");  \
    _Pragma("GCC diagnostic ignored \"-Wdeprecated-declarations\"");
#define SLANG_ALLOW_DEPRECATED_END _Pragma("GCC diagnostic pop");
#elif SLANG_CLANG
#define SLANG_ALLOW_DEPRECATED_BEGIN  \
    _Pragma("clang diagnostic push"); \
    _Pragma("clang diagnostic ignored \"-Wdeprecated-declarations\"");
#define SLANG_ALLOW_DEPRECATED_END _Pragma("clang diagnostic pop");
#elif SLANG_VC
#define SLANG_ALLOW_DEPRECATED_BEGIN \
    _Pragma("warning(push)");        \
    _Pragma("warning(disable : 4996)");
#define SLANG_ALLOW_DEPRECATED_END _Pragma("warning(pop)");
#else
#define SLANG_ALLOW_DEPRECATED_BEGIN
#define SLANG_ALLOW_DEPRECATED_END
#endif

//
// Use `SLANG_ASSUME(myBoolExpression);` to inform the compiler that the condition is true.
// Do not rely on side effects of the condition being performed.
//
#if defined(__cpp_assume)
#define SLANG_ASSUME(X) [[assume(X)]]
#elif SLANG_GCC
#define SLANG_ASSUME(X)              \
    do                               \
    {                                \
        if (!(X))                    \
            __builtin_unreachable(); \
    } while (0)
#elif SLANG_CLANG
#define SLANG_ASSUME(X) __builtin_assume(static_cast<bool>(X))
#elif SLANG_VC
#define SLANG_ASSUME(X) __assume(static_cast<bool>(X))
#else
[[noreturn]] inline void invokeUndefinedBehaviour() {}
#define SLANG_ASSUME(X)                 \
    do                                  \
    {                                   \
        if (!(X))                       \
            invokeUndefinedBehaviour(); \
    } while (0)
#endif

//
// Assertions abort in debug builds, but inform the compiler of true
// assumptions in release builds. __FILE__ / __LINE__ are captured at
// the call site and prepended (as basename only) to the failure message.
//
#ifdef _DEBUG
#define SLANG_ASSERT(VALUE)                                           \
    do                                                                \
    {                                                                 \
        if (!(VALUE)) [[unlikely]]                                    \
            ::Slang::handleAssert(#VALUE, __FILE__, __LINE__, false); \
    } while (0)
#else
#define SLANG_ASSERT(VALUE) SLANG_ASSUME(VALUE)
#endif

#define SLANG_RELEASE_ASSERT(VALUE)                                  \
    do                                                               \
    {                                                                \
        if (!(VALUE)) [[unlikely]]                                   \
            ::Slang::handleAssert(#VALUE, __FILE__, __LINE__, true); \
    } while (0)

template<typename T>
void slang_use_obj(T&)
{
}

#define SLANG_UNREFERENCED_PARAMETER(P) slang_use_obj(P)
#define SLANG_UNREFERENCED_VARIABLE(P) slang_use_obj(P)

#if defined(SLANG_RT_DYNAMIC)
#if defined(_MSC_VER)
#ifdef SLANG_RT_DYNAMIC_EXPORT
#define SLANG_RT_API SLANG_DLL_EXPORT
#else
#define SLANG_RT_API __declspec(dllimport)
#endif
#else
// TODO: need to consider compiler capabilities
// #     ifdef SLANG_RT_DYNAMIC_EXPORT
#define SLANG_RT_API SLANG_DLL_EXPORT
// #     endif
#endif
#endif

#if defined(_MSC_VER)
#define SLANG_ATTR_PRINTF(string_index, varargs_index)
#else
#define SLANG_ATTR_PRINTF(string_index, varargs_index) \
    __attribute__((format(printf, string_index, varargs_index)))
#endif

#ifndef SLANG_RT_API
#define SLANG_RT_API
#endif
