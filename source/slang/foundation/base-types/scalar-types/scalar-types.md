<!--
SPDX-FileCopyrightText: The Khronos Group, Inc.
SPDX-License-Identifier: CC-BY-4.0
-->

# Scalar Types

Scalar Types provides integer and byte conventions, character-code-unit types, and maximum default integer and index values for implementation services.
The common names distinguish sequence positions, element counts, and byte sizes from integers whose exact binary width matters.
The implementation remains part of `old-source/core/slang-common.h`; the boundaries below describe the proposed leaf rather than a completed header extraction.

## Concepts

The [Base Types concepts](../base-types.md#concepts) define scalar conventions and character code units.
The [root concepts](../../../slang.md#concepts) define implementation visibility and dependencies.

- An alias gives an existing C++ type another name; it does not introduce a distinct type or validate a value.
  In particular, `Index` expresses an ordinal use and `Count` expresses a cardinal use, but both are signed aliases of `Int`.
  A consuming service determines which negative values, if any, have meaning.
- An explicitly sized integer is appropriate when a stored representation requires a fixed number of bits.
  The default `Int` instead follows the product API's `SlangInt`, whose width is selected for the host pointer size.
  Equal width does not establish equal C++ type identity: the product header deliberately chooses fixed-width integer types rather than `intptr_t` or `size_t` for its default integers.
- Character aliases describe storage capacity rather than a guarantee of valid encoded text.
  `Char8` is `char`, with the host's `char` signedness; `Char16` and `Char32` are unsigned integers rather than C++'s distinct `char16_t` and `char32_t` types.
  `Char32` can hold every Unicode code point, but can also hold values outside that range.

## Dependencies

- The product header `include/slang.h` supplies `SlangInt` and `SlangUInt` and selects their underlying 32-bit or 64-bit integer types through `SLANG_PTR_IS_64`.
  The existing platform detection, configuration override, and compile-time pointer-size check remain product contracts outside this implementation module.
- The host C and C++ environment supplies the fixed-width integer types, `intptr_t`, and `size_t` used by the aliases.
  These inputs supply type definitions; Scalar Types performs no operating-system operation or numeric conversion.

The assigned aliases and constants do not call another implementation service.
Although their current combined header includes `slang-signal.h` and contains assertions and generic utilities, those facilities do not create a service dependency from this leaf to Failure Reporting or C++ Utilities.
Header extraction must separate those declarations before the proposed dependencies describe the physical includes.

## Principal Services

Scalar Types is a public leaf of Base Types, with no child modules or separate build artifact.
Its services fall into four groups:

- Integer representations provide default signed and unsigned integers, explicit 32-bit and 64-bit integers, and a pointer-sized signed integer.
- Sequence conventions name signed indices and counts and their unsigned counterparts.
  The three maximum-value constants bound the default unsigned integer, default signed integer, and signed index.
- Byte conventions provide raw byte storage and an unsigned type for sizes measured in bytes.
  `Byte` remains an unsigned character type so clients can access object representations through byte storage.
- Character storage provides the code-unit representations used by Text.
  Encoding conversion, text validation, and string ownership belong to [Text](../../text/text.md).

Arithmetic, bit manipulation, floating-point classification, and numeric-format conversion belong to the sibling [Numeric Operations](../numeric-operations/numeric-operations.md).
Scalar Types supplies representations and limits without taking ownership of those operations.

## Existing Implementation Mapping

Only the following declarations in `old-source/core/slang-common.h`, all within `namespace Slang`, belong to this leaf:

| Concern                | Declarations and Existing Definitions                                                                          |
| ---------------------- | -------------------------------------------------------------------------------------------------------------- |
| Explicit binary widths | `Int32`, `UInt32`, `Int64`, and `UInt64` alias `int32_t`, `uint32_t`, `int64_t`, and `uint64_t`, respectively. |
| Default integers       | `Int` aliases `SlangInt`, and `UInt` aliases `SlangUInt`.                                                      |
| Pointer-sized integer  | `PtrInt` aliases `intptr_t`.                                                                                   |
| Sequence quantities    | `Index` and `Count` alias `Int`; `UIndex` and `UCount` alias `UInt`.                                           |
| Maximum values         | `kMaxUInt` is `~UInt(0)`, `kMaxInt` is `Int(kMaxUInt >> 1)`, and `kMaxIndex` is `kMaxInt`.                     |
| Bytes and byte sizes   | `Byte` aliases `unsigned char`, and `Size` aliases `size_t`.                                                   |
| Character storage      | `Char8`, `Char16`, and `Char32` alias `char`, `uint16_t`, and `uint32_t`, respectively.                        |

The rest of the common header remains outside this assignment.
[C++ Utilities](../cpp-utilities/cpp-utilities.md) owns its move, swap, bit-test, and bit-copy helpers, deferred cleanup, componentwise equality, compiler compatibility facilities, unused-value helpers, and runtime visibility macros.
[Failure Reporting](../../failure-reporting/failure-reporting.md) owns its assertion and assumption facilities, including the assumption fallback function.
[Object Interfaces](../../object-interfaces/object-interfaces.md) owns `ICastable`, `IClonable`, and `clone`.
The product scalar definitions in `include/slang.h` remain product declarations rather than implementations assigned for relocation.

## Preservation Constraints

Migration preserves each alias's underlying type, signedness, and existing name, together with the maximum constants' types, values, and header-local definitions.
In particular, `Size` remains `size_t` rather than the product API's `SlangSizeT`, and `PtrInt` remains `intptr_t` rather than being replaced by the equally wide `Int`.
The character aliases remain their existing integer and character types; substituting C++'s distinct character types would change overload resolution and signatures.

These declarations carry no export annotation, but their underlying types occur in other services' layouts and signatures.
For example, the runtime-exported `StringRepresentation` in `old-source/core/slang-string.h` stores its length and capacity as `Index`.
The [root's staged migration contracts](../../../slang.md#staged-migration-contracts) therefore still require preservation of canonical type identity and binary interfaces; implementation-only visibility is not evidence that those contracts can change.
The same contracts preserve both the static `core` consumer and enabled `slang-rt`'s separate compilation of Foundation code.

The proposed scalar leaf requires no new service boundary decision from an ancestor.
Its assert-free declarations can be separated without resolving Base Types' [Bit Scanning dependency](../base-types.md#boundary-conditions) or Foundation's existing Failure Reporting, Text, and Memory cycle.
Those ancestor conditions remain open, and this document does not authorize migration.
