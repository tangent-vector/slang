<!--
SPDX-FileCopyrightText: The Khronos Group, Inc.
SPDX-License-Identifier: CC-BY-4.0
-->

# Numeric Operations

Numeric Operations provides arithmetic, numeric bit operations, floating-point classification and comparison, and conversion between numeric encodings.
The operations support scalar calculations, compiler constant construction and emission, and host-side preparation and inspection of numeric data.

The implementation remains in `old-source/core/slang-math.h`.
This document refines [Base Types' assignment](../base-types.md#decomposition) as a proposed leaf; it does not describe a completed extraction or code port.

## Concepts

The [root concepts](../../../slang.md#concepts) define modules, dependencies, and implementation visibility.
[Base Types](../base-types.md#concepts) defines a numeric encoding, and [C++ Utilities](../cpp-utilities/cpp-utilities.md#concepts) defines an object representation.

- **Bit reinterpretation** exposes the same floating-point representation through an integer, or the reverse, without converting the represented number numerically.
  The existing pairs use `float` and `int`, and `double` and `int64_t`.
- A **narrow floating-point encoding** uses fewer bits than the host `float` representation.
  The existing conversions cover binary16 (called Half by the declarations), bfloat16, and the eight-bit E4M3 and E5M2 formats.
  Their integer carriers hold encoding bits rather than the numeric result of an integer conversion.
- **Approximate equality** compares two `double` values using a caller-supplied tolerance.
  The existing operation accepts exact equality first, uses an absolute threshold scaled by the binary64 minimum normal value near zero, and otherwise scales the threshold by the sum of the inputs' magnitudes.
  Selecting the tolerance and deciding what a comparison means for a client remain client policy.

## Dependencies

- The product header `include/slang.h` supplies `SLANG_FORCE_INLINE` and the compiler-selection definitions that choose its expansion.
  It normally includes the host integer definitions used for `int64_t`; configurations that suppress those includes retain their existing responsibility to supply the definitions.
  These are product compilation contracts rather than a dependency on compiler implementation services.
- The host C++ environment supplies built-in arithmetic and integer types, floating-point representations, and `<cmath>`'s NaN and infinity classification.
  The bit reinterpretation and format-conversion implementations rely on the existing host/compiler representation behavior, including the assumed binary32 `float`, binary64 `double`, and integer carrier widths.

The assigned implementations do not call another Foundation service.
The header names built-in and fixed-width host types rather than the aliases declared by [Scalar Types](../scalar-types/scalar-types.md).
The union-based reinterpretation helpers do not call C++ Utilities' `bitCast`, and the integer bit operations do not call [Bit Scanning](../bit-scanning/bit-scanning.md).
There is no assertion or signal call in the numeric implementation.
The arithmetic templates use the C++ operators and conversions available for their instantiated types; client-defined operators remain client requirements rather than dependencies on the client's service.

## Principal Services

Numeric Operations is a public leaf of Base Types, with no child modules or separate build artifact.
Its small operations share scalar arithmetic and numeric representation concerns; the existing callers and internal relationships do not require a further module boundary.
The services have four areas:

- **Arithmetic and bounds** provide absolute value, two- and three-value minimum and maximum, clamping, and conversion to an integer floor.
  Clients include Text's edit-distance calculation and compiler layout calculations.
  The `Math::Pi` declaration also belongs to this area.
- **Classification and comparison** query NaN and infinity for `float` and `double` and compare `double` values with the existing tolerance rule.
  For example, the test runner supplies its selected difference threshold when comparing numeric output.
- **Integer bit operations** provide population count, floor and ceiling base-two logarithm calculations, and isolation of the lowest set bit.
  Collections uses the ceiling logarithm for capacity growth, while compiler alignment calculations use the lowest set bit of a positive stride.
- **Numeric representations** reinterpret floating-point bits and convert between host `float` and the four narrow encodings.
  The same area includes the existing Half limit macros.
  `IRBuilder::getFloatValue` narrows and widens values before storing narrow-format constants, and SPIR-V emission uses the narrowing operations to construct literal bits.
  Host render-data helpers also widen stored encodings for inspection.

The representation helpers and conversions remain together because the conversions use the reinterpretation operations and `Math`'s nested unions directly.
The generic bit test and object-representation copy in [C++ Utilities](../cpp-utilities/cpp-utilities.md#existing-implementation-mapping), and the byte and 32-bit most-significant-bit queries in Bit Scanning, retain their separate assignments.
The seeded `Random` class already belongs to [Random Numbers](../../random-numbers/random-numbers.md).

## Existing Implementation Mapping

The complete active contents of `old-source/core/slang-math.h` belong to this leaf:

| Area                          | Existing Declarations                                                                                                                                         |
| ----------------------------- | ------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| Constants                     | `SLANG_HALF_MAX`, `SLANG_HALF_SUB_NORMAL_MIN`, and the declaration of `Math::Pi`.                                                                             |
| Arithmetic                    | `Math::Abs`, both overloads of `Math::Min` and `Math::Max`, `Math::Clamp`, and the `float` and `double` overloads of `Math::FastFloor`.                       |
| Classification and comparison | The `float` and `double` overloads of `Math::IsNaN` and `Math::IsInf`, and `Math::AreNearlyEqual`.                                                            |
| Integer bits                  | `Math::Ones32`, `Math::Log2Floor`, `Math::Log2Ceil`, and `Math::getLowestBit`.                                                                                |
| Representation support        | `Math`, its nested `FloatIntUnion` and `DoubleInt64Union`, their members and factories, and `FloatAsInt`, `IntAsFloat`, `DoubleAsInt64`, and `Int64AsDouble`. |
| Narrow-format conversions     | `FloatToHalf`, `HalfToFloat`, `FloatToBFloat16`, `BFloat16ToFloat`, `FloatToFloatE4M3`, `FloatE4M3ToFloat`, `FloatToFloatE5M2`, and `FloatE5M2ToFloat`.       |

There is no assigned out-of-line implementation file or partial-file extraction from another legacy header.
The commented-out `Math::Log2(float)` block is inactive text rather than an additional implemented service.
The former random-number declarations are absent from this header and are not part of its assignment.
Compiler constant construction, target emission, render-data interpretation, and test comparison remain operations owned by the consuming modules; their calls do not transfer those services to Numeric Operations.

## Preservation Constraints

Migration preserves the current declaration names, parameter and result types, overloads, nested union identities and layouts, inline annotations, macro definitions, and arithmetic and bit expressions.
For example, the classification wrappers return `int`, the Half and bfloat16 conversions use `unsigned short` carriers, and the eight-bit conversions use `unsigned int` carriers.
The `Math::Pi` declaration does not authorize adding a definition or choosing a value during extraction.

The format conversions have distinct rounding, overflow, subnormal, signed-zero, and NaN behavior.
E4M3 narrowing maps magnitudes above its finite maximum to a NaN encoding, whereas E5M2 narrowing can produce infinity.
Those differences, including the existing NaN bit mappings, remain part of the behavior to preserve rather than a reason to impose one common conversion policy.
An integer carrier is not an input validator: the eight-bit decoders accept `unsigned int` parameters, and the implementation does not assert that only eight bits are supplied.
The observed clients pass encodings produced by the narrowing operations or values read from eight-bit storage; that use does not authorize masking wider arguments or changing their existing handling.

Arithmetic expressions retain their existing C++ operator and conversion rules.
For example, `FastFloor` begins with a cast to `int`.
The expression `val & (-val)` in `getLowestBit` can involve integral promotions or client-defined operators before its result is returned as `T`.
This ownership proposal neither broadens their defined input domains nor adds validation or replaces their arithmetic.
The implementations manipulate integer bit patterns rather than serialized byte streams; no new byte-order conversion or portability guarantee follows from the proposed ownership.

The header contains no product or runtime export annotations, but that alone does not establish that changing a defining namespace is safe.
The [root's staged migration contracts](../../../slang.md#staged-migration-contracts) require an audit of binary-interface uses before moving defining types and preserve both static `core` and enabled `slang-rt`'s separate compilation consumers.
The current target definitions make the legacy header available to those consumers; this leaf introduces no independent library or source-compilation unit.

Numeric Operations adds no failure-reporting dependency and does not resolve Base Types' [Bit Scanning dependency](../base-types.md#boundary-conditions) or Foundation's [Failure Reporting, Text, and Memory cycle](../../foundation.md#failure-reporting-and-basic-data-services).
Its proposed ownership requires no additional ancestor service assignment.
The existing [artifact and source-reuse questions](../../../slang.md#unresolved-architectural-dependencies) remain open, and this document does not authorize migration.
