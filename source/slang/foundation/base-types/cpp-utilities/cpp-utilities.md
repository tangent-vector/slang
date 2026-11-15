<!--
SPDX-FileCopyrightText: The Khronos Group, Inc.
SPDX-License-Identifier: CC-BY-4.0
-->

# C++ Utilities

C++ Utilities provides generic type and value operations, simple ranges, deferred cleanup, and compiler compatibility conventions.
It lets implementation clients express type-dependent declarations, manipulate values without knowing their application role, and use the existing compiler annotation and visibility spellings.

The implementation remains in `old-source/core/`.
The leaf described below refines [Base Types' assignment](../base-types.md#decomposition); it is a proposed ownership boundary rather than a completed header extraction.

## Concepts

The [root concepts](../../../slang.md#concepts) define modules, dependencies, and implementation visibility.
[Base Types](../base-types.md#concepts) defines a half-open range.

- A **type trait** answers a compile-time question about C++ types.
  The existing traits query inheritance and conversion, and provide a nested type only when a condition holds.
  The conversion query `IsConvertible<B, D>` checks conversion of a `D` lvalue to `B`; its parameter order matters to clients such as native smart pointers.
- A **scope-exit action** is a callable stored in a local guard and invoked when that guard is destroyed.
  `SLANG_DEFER` captures surrounding variables by reference; their lifetime must cover the invocation.
  The guard supplies invocation timing, while the client supplies the action and any resource-lifetime policy.
- An **object representation** is the sequence of bytes occupied by a C++ object, including any padding.
  `bitCast` copies that representation between equal-sized, trivially copyable types, with a trivially constructible destination.
  It does not perform a numeric-format conversion.
- **Componentwise equality** compares the components of a C++ value using their own equality operations.
  The existing macros select defaulted equality in C++20 and structured-binding implementations for one, two, or three components otherwise.
  Those definitions remain the current language-mode contracts rather than a proposal to replace one implementation with the other.

## Dependencies

- The product header `include/slang.h` supplies compiler-selection macros, token concatenation for the scope-exit macros' local names, and DLL export annotations.
  Those definitions remain product contracts outside this implementation hierarchy.
- The host C++ environment supplies language features and type traits used to select declarations and implement generic operations.
  Its fixed-width integer definitions support the existing bounded bit-mask query, and its `std::memcpy` supplies the representation copy.
- The existing build definitions supply runtime linkage settings.
  `cmake/SlangTarget.cmake` defines `SLANG_RT_DYNAMIC` for shared-runtime consumers and `SLANG_RT_DYNAMIC_EXPORT` for the runtime build, selecting the existing `SLANG_RT_API` annotation.
  These settings govern declarations compiled with the header; they do not make the utility module call Runtime services.

The assigned implementations require no other Foundation service.
Operations selected through a client's C++ type or callable remain client requirements rather than additional module dependencies.
For example, `String` uses `_Move` for its representation pointer, and the Tint compiler adapter uses `SLANG_DEFER` to call its result-release function.
Neither use makes C++ Utilities depend on Text or the adapter.
Likewise, the component comparisons and range comparisons are supplied by the instantiated types.

The current `slang-common.h` includes `slang-signal.h`, but none of the facilities assigned here calls a signal or assertion handler.
The proposed header separation must preserve access to Failure Reporting for its actual clients without importing that dependency into this leaf.

## Principal Services

C++ Utilities is a public leaf of Base Types, with no child modules or separate build artifact.
Its services have four areas:

- **Type-dependent declarations** provide inheritance and conversion queries and conditional nested types.
  Their supporting result and conversion types belong with those queries.
- **Generic values and bounds** provide move casts, exchange through moves, a bounded bit-mask test, object-representation copying, componentwise equality, and half-open range construction and membership.
  `Range<T>` stores two bounds, initialized to zero by default; it neither owns a sequence nor supplies iteration or bound validation.
  For example, the AST printer records parameter-text offsets with this range representation.
- **Scope-exit actions** provide a stored callable and the macros that create a local guard.
  The guard invokes its callable on destruction; resource-specific cleanup remains in the supplied action.
- **Compiler conventions** provide diagnostic push/pop pairs for exhaustive enum switches and deprecated declarations, printf-format annotations, unused-value helpers, a compiler-dependent return macro, and runtime visibility annotations.
  The printf annotation lets the C++ compiler check arguments supplied to a client's formatting function.
  The return macro does not report a failure; its callers invoke Failure Reporting separately when needed.

These small facilities share the task of adapting ordinary C++ declarations and values without acquiring an application-specific service dependency.
The current implementation does not justify a further child boundary.

## Existing Implementation Mapping

Paths in this section are relative to `old-source/core/`.

- The complete `slang-type-traits.h` belongs here: `IsBaseOf`, `IsConvertible`, `EnableIf`, `TraitResultYes`, `TraitResultNo`, and `IsBaseOfTraitHost`, including their members and specialization.
- The complete `slang-range.h` belongs here: `Slang::Range<T>` and `Slang::makeRange`.
  The separately defined, iterable global `Range` in `old-source/slang/slang-reflection-json.cpp` is not this declaration and is outside Base Types' assignment.
- Only the following portions of `slang-common.h` belong here:
  - `_Move`, `Swap`, `isBitSet`, and `bitCast` in `namespace Slang`.
  - The global `SlangDeferImpl` template and `SLANG_DEFER_LAMBDA`/`SLANG_DEFER` macros, including their existing override guard.
  - `SLANG_COMPONENTWISE_EQUALITY_1`, `SLANG_COMPONENTWISE_EQUALITY_2`, and `SLANG_COMPONENTWISE_EQUALITY_3` in both language-mode branches.
  - `VARIADIC_TEMPLATE`, `UNREACHABLE_RETURN`, `SLANG_EXHAUSTIVE_SWITCH_BEGIN`, `SLANG_EXHAUSTIVE_SWITCH_END`, `SLANG_ALLOW_DEPRECATED_BEGIN`, `SLANG_ALLOW_DEPRECATED_END`, and `SLANG_ATTR_PRINTF`.
  - The global `slang_use_obj` helper, `SLANG_UNREFERENCED_PARAMETER`, `SLANG_UNREFERENCED_VARIABLE`, and the guarded definitions of `SLANG_RT_API`.

The common header's scalar aliases and limits belong to [Scalar Types](../scalar-types/scalar-types.md).
Its assertions, assumptions, and assumption fallback belong to [Failure Reporting](../../failure-reporting/failure-reporting.md).
Its `ICastable`/`IClonable` aliases and `clone` helper belong to [Object Interfaces](../../object-interfaces/object-interfaces.md).
The separate numeric, C-runtime compatibility, and bit-scanning services remain with the [other Base Types children](../base-types.md#decomposition).
In particular, the generic bit-mask test and representation copy above are not the most-significant-bit queries or numeric encoding conversions.

## Preservation Constraints

Migration preserves the existing names, signatures, type identities, member order, access, inline choices, and macro expansion and selection rules.
The utility templates are not permission to substitute standard-library implementations or strengthen their contracts.
For example, `isBitSet` uses a 32-bit mask calculation and requires `sizeof(T) <= sizeof(uint32_t)`; the trait queries keep their current result spelling and parameter order.
The scope-exit guard's construction and destruction behavior, reference capture, generated local names, and macro override guard also remain unchanged.

The implementations are header based, but their clients include ABI-bearing declarations such as native smart pointers, strings, and runtime entry points.
The [root's staged migration contracts](../../../slang.md#staged-migration-contracts) still require a binary-interface audit before moving a defining type to another namespace.
The existing global guard and helper declarations and the existing `Slang` declarations cannot be assigned new identities merely because this document calls them implementation utilities.

Runtime visibility needs coordinated preservation across its current definition sites.
The common header selects export, import, or empty annotations according to the compiler and runtime build settings; `old-source/slang-rt/slang-rt.h` also defines `SLANG_RT_API` for its entry-point declarations after including Foundation headers.
Those entry points pass `Slang::String` by value, so C linkage does not remove their C++ ABI requirements.
This leaf owns the common header's compatibility definitions, while Runtime owns its entry-point header; extracting the former must account for the latter's include order and definitions without inventing a new export mechanism.

The current `core` target and enabled `slang-rt` separately compile Foundation code using these headers under their respective settings.
Their source reuse and the broader [ancestor dependency questions](../../../slang.md#unresolved-architectural-dependencies) remain as recorded by the root.
This leaf introduces no additional failure-reporting dependency and does not resolve Foundation's [Failure Reporting, Text, and Memory cycle](../../foundation.md#failure-reporting-and-basic-data-services).
The ownership description does not authorize a code port.
