<!--
SPDX-FileCopyrightText: The Khronos Group, Inc.
SPDX-License-Identifier: CC-BY-4.0
-->

# Base Types

Base Types provides the scalar conventions and operations that other implementation services use to represent and manipulate individual values.
It supplies arithmetic and numeric encodings, simple ranges, generic C++ utilities, and compatibility facilities for the supported C++ compilers and C runtimes.

The implementation remains under `old-source/core/`.
This document proposes lower service boundaries within Foundation's assignment; the [boundary conditions](#boundary-conditions) distinguish that proposal from the current combined headers and unresolved Foundation dependencies.

## Concepts

The [root document](../../slang.md#concepts) defines implementation modules, dependencies, and visibility.
The [Foundation concepts](../foundation.md#concepts) define ownership and borrowed views.

- **Scalar conventions** distinguish an integer's intended use from a required binary width.
  `Int` and `UInt` currently alias the product API's `SlangInt` and `SlangUInt`; `Index` and `Count` alias `Int` to express ordinal and cardinal uses.
  Fixed-width aliases describe explicitly sized integers, `Byte` is an unsigned character used for raw bytes, and `Size` is `size_t` for sizes in bytes.
- A **character code unit** is one element of an encoded character sequence.
  `Char8` and `Char16` need not hold a complete Unicode code point; `Char32` can hold one.
  Character storage and encoding conversion belong to [Text](../text/text.md).
- A **half-open range** includes its beginning and excludes its end.
  `Range<T>` stores these two bounds and tests membership; it does not provide collection iteration or own elements.
- A **numeric encoding** represents a number with a specified bit layout.
  The conversion operations cover binary16, bfloat16, and the E4M3 and E5M2 eight-bit floating-point formats, alongside floating-point/integer bit reinterpretation.
  The encoding and rounding behavior is shared by compiler constant construction, emitted literal bits, and host-side data preparation.

## Dependencies

- [Failure Reporting](../failure-reporting/failure-reporting.md) supplies the assertion used by the Bit Scanning child to require an eight-bit input for the byte-oriented most-significant-bit query.
  The ordinary assertion checks in debug builds and becomes an optimizer assumption otherwise, under Failure Reporting's existing policy.
- The product headers supply the default integer types and compiler/platform contracts, including inline annotations, token concatenation, and DLL visibility macros.
  Those declarations remain product contracts outside this implementation hierarchy.
- The host C and C++ environment supplies fixed-width integers, type traits, memory copying, floating-point classification, and the C-runtime operations adapted by the compatibility header.
  The MSVC bit-scan branch uses a compiler intrinsic; other branches use the module's lookup table.
  Secure-CRT fallbacks call C-runtime memory, string, formatting, and file operations directly.
  They do not call Foundation's Host Services, and their character-buffer operations do not require Foundation's Text representation.
- Repository CMake probes supply the availability definitions that select native secure-CRT functions on MinGW and Cygwin.
  The compatibility header supplies missing functions under those definitions and its platform guards.

Generic operations act on types supplied by their clients.
For example, deferred cleanup invokes its stored callable when the guard is destroyed; the work chosen by that callable does not create a dependency from Base Types to every service a caller may use.

## Decomposition

Base Types assembles the following proposed public children.
Each child exposes a concern used by implementation clients outside Base Types; none is a separate build artifact.

| Child                                                                         | Responsibility                                                                                                   |
| ----------------------------------------------------------------------------- | ---------------------------------------------------------------------------------------------------------------- |
| [Scalar Types](scalar-types/scalar-types.md)                                  | Integer and byte conventions, character-code-unit types, and their limits.                                       |
| [Numeric Operations](numeric-operations/numeric-operations.md)                | Arithmetic, floating-point classification and comparison, numeric bit operations, and numeric-format conversion. |
| [C++ Utilities](cpp-utilities/cpp-utilities.md)                               | Generic type and value utilities, simple ranges, deferred cleanup, and compiler compatibility conventions.       |
| [C Runtime Compatibility](c-runtime-compatibility/c-runtime-compatibility.md) | Existing C-runtime function and macro spellings where the host runtime does not supply them.                     |
| [Bit Scanning](bit-scanning/bit-scanning.md)                                  | Byte and 32-bit most-significant-bit queries, including the byte-input precondition.                             |

The separate Bit Scanning child owns the complete `ByteEncodeUtil` type and its table.
Its checked byte query requires Failure Reporting, while the other children's existing operations do not call that service.
Text uses the lower scalar, C++, numeric, and C-runtime facilities without requiring Bit Scanning.
For example, Text's edit-distance calculation uses an arithmetic minimum operation, and floating-point string append uses CRT formatting and bounded length queries.

The compatibility facilities let clients use the existing spellings across supported environments.
Their purpose does not assign higher-level text formatting, file access, or runtime program execution to Base Types.

## Existing Implementation Mapping

The following paths are relative to `old-source/core/`:

- C++ Utilities owns `slang-type-traits.h`'s compile-time type-relation and conditional-declaration facilities, including their supporting result and conversion types, and `slang-range.h`'s bounded range representation and construction helper.
- Bit Scanning owns `slang-byte-encode-util.h` and `slang-byte-encode-util.cpp`: the byte and 32-bit most-significant-bit queries, compiler-specific implementation branch, and shared byte lookup table.
  Despite the filename, these files do not implement a general byte-stream encoding service.
- Numeric Operations owns `slang-math.h`'s arithmetic and bit utilities, floating-point bit reinterpretation, and numeric-format conversions.
  The separate seeded `Random` class has already moved to [Random Numbers](../random-numbers/random-numbers.md).
- C Runtime Compatibility owns `slang-secure-crt.h`'s platform- and availability-guarded definitions, including memory, character-buffer, formatting, and stdio operations.
- Only part of `slang-common.h` belongs to Base Types.
  Scalar Types owns its scalar and character aliases and limits.
  C++ Utilities owns its move, swap, bit-test, and bit-copy utilities; deferred cleanup; componentwise equality; compiler compatibility, warning, printf-annotation, and unused-value helpers; and runtime visibility macros.
  The assertion and assumption facilities, including the assumption fallback function, belong to Failure Reporting.
  The `ICastable` and `IClonable` aliases and `clone` helper belong to [Object Interfaces](../object-interfaces/object-interfaces.md).

The implementation is predominantly in headers; the byte lookup table is an out-of-line definition.
The current `core` target compiles the legacy implementation, and enabled `slang-rt` separately compiles the same core files under its runtime export settings.
Migration preserves both consumers under the [root's staged migration contracts](../../slang.md#staged-migration-contracts).
The implementation headers are not installed product headers, but that fact does not establish that their declarations are irrelevant to binary compatibility.
Scalar aliases participate in the layout and signatures of other services, and `SLANG_RT_API` controls declarations used across the existing runtime boundary.
The existing alias identities, layouts, declaration spellings, signatures, visibility, and platform selection remain preservation constraints; moving a defining type to a new namespace requires the binary-interface audit specified by the root.
The globally named C-runtime fallbacks and their availability guards also remain part of the current compilation contract.

## Boundary Conditions

Treating all these services as one leaf would expose a cycle through the byte query's assertion.
`ByteEncodeUtil::calcMsb8` uses `SLANG_ASSERT` to require that its input fits in eight bits.
Failure Reporting handles that check through `handleAssert` and `handleSignal`; the signal handler constructs and retains a Text `String`.
Text in turn uses Base Types' scalar aliases, move utilities, and C-runtime compatibility operations.
At that coarse granularity, Base Types depends on Failure Reporting, Failure Reporting depends on Text, and Text depends on Base Types.

The proposed children distinguish the assert-free services used by Text from the checked Bit Scanning service.
Text can depend on those public children without depending on the assembled Base Types interface, and Bit Scanning can depend on Failure Reporting without splitting `ByteEncodeUtil`'s members or changing its assertion behavior.
This is an ownership proposal, not a completed header extraction or code port.
Removing `slang-common.h`'s include of the signal header alone would not establish that separation.

The proposal does not resolve Foundation's existing [Failure Reporting, Text, and Memory cycle](../foundation.md#failure-reporting-and-basic-data-services).
That condition remains relevant to Bit Scanning's failure-reporting dependency.
The child documents still require their own refinement and review, and no migration is authorized by this document.
