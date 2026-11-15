<!--
SPDX-FileCopyrightText: The Khronos Group, Inc.
SPDX-License-Identifier: CC-BY-4.0
-->

# Bit Scanning

Bit Scanning finds the highest set bit in an eight-bit or 32-bit unsigned value.
It provides a common result convention across the compiler-specific intrinsic and lookup-table implementations.

The implementation remains in `old-source/core/slang-byte-encode-util.h` and `slang-byte-encode-util.cpp`.
This document refines the public child assigned by [Base Types](../base-types.md#decomposition); it describes proposed ownership rather than a completed code port.

## Concepts

The [root concepts](../../../slang.md#concepts) define modules, dependencies, and implementation visibility.
[Base Types](../base-types.md#concepts) defines the distinction between integer-use conventions and fixed binary widths.

A **most-significant-bit index** is the zero-based position of the highest set bit in an unsigned value, counting from its least significant bit.
For example, the index for `0x81` is 7.
Zero has no set bit, and both queries represent that result with `-1`.
The result is a position, rather than a bit mask or the number of set bits.

## Dependencies

- [Failure Reporting](../../failure-reporting/failure-reporting.md) supplies the assertion and optimizer-assumption contract used to require an eight-bit input for the byte query.
  Under `_DEBUG`, the check calls the existing assertion handler on failure; otherwise the condition becomes an optimizer assumption.
  Assertion handling, including the policy that can suppress non-release assertions, remains with Failure Reporting.
- The product header `include/slang.h` supplies compiler selection and forced-inline annotations.
  Its `SLANG_VC` setting selects the intrinsic implementation; these macros remain existing external contracts rather than declarations assigned for relocation.
- The host C++ environment supplies the fixed-width `uint32_t` and `int8_t` types and, for the selected intrinsic branch, `_BitScanReverse`.
  These declarations use the fixed-width integer types directly, without requiring the sibling Scalar Types aliases.

The current `slang-common.h` include also exposes scalar declarations and generic utilities that the queries do not use.
Those combined includes do not assign a dependency on the assembled Base Types interface or all of its children.

## Principal Services

Bit Scanning is a public leaf of Base Types, with no child modules or separate build artifact.
The complete `ByteEncodeUtil` type and its shared table remain one concern:

| Service           | Existing Contract                                                                                                                 |
| ----------------- | --------------------------------------------------------------------------------------------------------------------------------- |
| Byte query        | `calcMsb8(uint32_t v)` returns `int` and requires `v` in `[0, 255]`; it returns `-1` for zero and an index in `[0, 7]` otherwise. |
| 32-bit query      | `calcMsb32(uint32_t v)` returns `int` for every `uint32_t` input; it returns `-1` for zero and an index in `[0, 31]` otherwise.   |
| Byte lookup table | The public `static const int8_t s_msb8[256]` maps every eight-bit value to the same result as the byte query.                     |

The byte query's wider parameter does not permit larger values or request truncation to eight bits.
Its ordinary assertion checks that precondition under Failure Reporting's existing policy; the service defines no fallback result for an invalid byte input.
Both queries are static methods with forced-inline definitions in the header.
The table has one out-of-line definition in the assigned implementation, even when the inline methods use the intrinsic branch.

`CommandOptions::_addOptionName` records permitted option-prefix lengths as bits in a 32-bit value.
`CommandOptions::findTargetIndexByName` uses the 32-bit query to consider possible prefixes from longest to shortest.
It clears a length's bit before continuing when no match was returned.
The test runner likewise uses the byte query to enumerate selected flags from its current eight rendering APIs.
These callers supply and interpret the bit positions; option lookup and rendering-API policy remain their responsibilities.

## Existing Implementation Mapping

Both files assigned by Base Types belong wholly to this leaf:

- `old-source/core/slang-byte-encode-util.h` declares `Slang::ByteEncodeUtil`, its two static queries, and its public table, and defines both compiler-selected implementations.
  With nonzero `SLANG_VC`, each query handles zero explicitly before calling `_BitScanReverse`.
  Otherwise the byte query indexes the table, and the 32-bit query selects the highest nonzero byte and adds that byte's bit offset to its table result.
  For an all-zero 32-bit input, the table implementation reads the `-1` entry without adding an offset.
- `old-source/core/slang-byte-encode-util.cpp` defines the table and the seven repetition macros used to write its initializer.
  Entry zero is `-1`; positive entries give the highest set-bit position for each value through 255.

The `SLANG_VC` branch is selected by the compiler contract rather than an operating-system check.
The product header normally selects it from `_MSC_VER`, including compilers that define that compatibility macro; externally supplied compiler configuration can override detection.
Both implementations retain the same input and result domains.
The filename does not assign a byte-stream encoding or decoding service to this module.

The static `core` target includes the table implementation through `old-source/core/CMakeLists.txt` and the shared source-globbing helper.
Enabled `slang-rt` separately compiles the same core files through `EXTRA_SOURCE_DIRS` under its runtime export settings.
The header is an implementation header rather than an installed product header, and the declarations carry no export annotation.
The [root's staged migration contracts](../../../slang.md#staged-migration-contracts) still require both compilation consumers and an audit of binary-interface uses before changing a defining namespace.
Existing names, signatures, type layout, the table's public visibility and element type, inline choices, platform selection, and query behavior remain preservation constraints.

## Unresolved Ancestor Constraints

The byte assertion gives this leaf a real dependency on Failure Reporting.
Base Types' [separate lower children](../base-types.md#boundary-conditions) let Text use scalar and utility services without depending on Bit Scanning or splitting `ByteEncodeUtil`.
That separation does not resolve Foundation's [Failure Reporting, Text, and Memory cycle](../../foundation.md#failure-reporting-and-basic-data-services), which remains relevant to this leaf's dependency.
The broader source-reuse artifact question also remains with the [root](../../../slang.md#unresolved-architectural-dependencies).
This refinement introduces no replacement failure mechanism and does not authorize migration while the applicable ancestor constraints remain unresolved.
