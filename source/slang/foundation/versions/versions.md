<!--
SPDX-FileCopyrightText: The Khronos Group, Inc.
SPDX-License-Identifier: CC-BY-4.0
-->

# Versions

Versions provides numeric version values, ordering, compatibility checks, textual conversion, and selection among supplied candidate versions.
Clients decide what a version identifies and whether a particular compatibility or selection rule applies.
Compiler discovery, target requirements, and stored-format validation remain with those clients.

The implementation remains in `old-source/core/`.
This document refines [Foundation's assignment](../foundation.md#existing-implementation-mapping) as a public leaf within the existing artifact.

## Concepts

- A **numeric version** is a major, minor, and patch triple represented by `SemanticVersion`.
  The major and minor fields are unsigned 16-bit integers; the patch field is an unsigned 32-bit integer.
  The representation has no pre-release identifier or build metadata.
  `isSet` reports whether any component is nonzero, so the all-zero value also serves as an unset value rather than having a separate tag.
- A version's **raw value** is the unsigned 64-bit encoding with the major component in the highest 16 bits, the minor component in the next 16 bits, and the patch component in the lowest 32 bits.
  Comparing raw values gives lexicographic ordering of the three stored components; hashing uses this same encoding.
  The raw value is a numeric encoding rather than a declaration of the object's byte order in memory.
- **Backward compatibility** is the directional relation provided by `isBackwardsCompatibleWith`: the receiving version must have the same major component and a minor component at least as large as the argument's.
  The check ignores the patch component, including when the argument has a larger patch, and applies the same rule when the major component is zero.
  A caller uses this relation only when the versioned service or format follows that policy.
- A **match version**, represented by `MatchSemanticVersion`, records an unknown, past, or future preference, or one, two, or three specified numeric components.
  The numeric request is a selection preference rather than a compatibility requirement: selection may return a candidate outside those components.

[Foundation](../foundation.md#concepts) defines borrowed views and string slices.
Version parsing borrows its input characters only for the call; formatting appends to a caller-supplied string builder.

## Dependencies

- [Scalar Types](../base-types/scalar-types/scalar-types.md) supplies integer, count, and index conventions used by the representation and candidate ranges.
- [Text](../text/text.md) supplies bounded character views, splitting and integer parsing, and string-builder formatting.
  Versions defines how those operations interpret and present the version components.
- [Hashing](../hashing/hashing.md) supplies the hash-code type and ordinary integer hashing used by version keys.
- Collections' [Array Views](../collections/array-views/array-views.md) supplies borrowed traversal for earliest/latest queries.
  Collections' [Sequence Storage](../collections/sequence-storage/sequence-storage.md) supplies the copied list and sorting operation used by numeric candidate selection.
- [Failure Reporting](../failure-reporting/failure-reporting.md) supplies the nonnegative-input assertion in `SemanticVersion::set`, which is also reached by raw-value import.
  Its existing debug-check and optimizer-assumption distinction remains applicable.
- The product headers supply `SlangResult`, success/failure codes, and the failure-propagation macro used by parsing.
  The host C++ environment supplies fixed-width integer types and the integer conversion and bit operations used by the representation.

The broad `slang-basic.h` include assembles several Foundation services; it does not assign their implementations to Versions.
The result-checking macro comes from `include/slang-com-helper.h` without requiring a dependency on an object-interface implementation.

## Service Areas

Versions remains a leaf with no child modules.
The value representation, text conversion, and selection rules share the same component ordering and remain one small service:

- **Values and ordering:** construction, reset, component access, raw-value import/export, comparison, hashing, and earliest/latest queries.
  Earliest/latest return an all-zero version for an empty candidate range; that result is not a distinct optional value.
- **Text conversion:** parsing the existing separated numeric notation and appending its formatted form.
  Parsing splits into at most three components using `.` by default or a supplied separator.
  The parser uses `StringUtil::parseInt` for each component, including that routine's decimal and hexadecimal integer notation.
  It rejects negative values or values above `0xffff` for major/minor and `0x7fffffff` for patch.
  Missing lower components remain zero.
  Parsing resets the output before inspecting the input and leaves it zero on failure.
  `SemanticVersion::append` always emits major and minor components, with a patch component only when nonzero.
  `MatchSemanticVersion::append` emits the preference word or the specified numeric components; a complete numeric request delegates to the value formatter and can therefore omit a zero patch without changing its kind.
- **Compatibility and matching:** the directional compatibility relation and best-effort choice from caller-supplied candidates.
  An empty range yields the all-zero version; a single candidate is returned regardless of the request.
  With multiple candidates, a past preference selects the earliest, while an unknown or future preference selects the latest.
  Numeric requests use bounds derived by incrementing the last specified component and a sorted copy of the candidates.
  The selector skips candidates below the lower bound, then scans forward while candidates are below the upper bound.
  It returns the last candidate in that span when the span is nonempty; otherwise it returns the following candidate, or the final preceding candidate if no following one exists.
  For example, a request for `2.1` prefers `2.1.7` over `2.1.3`, but can select `2.2` when no `2.1` candidate is present.

The field widths do not imply that every construction or import entry point accepts every representable bit pattern.
Constructors narrow their integer arguments to the fields, while `set` first asserts that its signed inputs are nonnegative.
`setRawValue` converts the extracted components to signed `int` and calls `set`; its behavior is not an unconditional full-width round-trip guarantee.
Migration preserves the existing conversions and finite-width arithmetic in numeric matching.

## Existing Implementation Mapping

The complete `old-source/core/slang-semantic-version.h` belongs to Versions: `SemanticVersion`, `MatchSemanticVersion`, their aliases and request kinds, inline operations, fields, and declarations.
The complete `old-source/core/slang-semantic-version.cpp` belongs here as well: parsing, formatting, extrema, compatibility, and candidate selection.
Neither file contains a separate compiler or host-discovery service requiring a partial-file assignment.

The following uses remain with their existing owners:

- `DownstreamCompilerUtil::findClosestCompiler` in `old-source/compiler-core/slang-downstream-compiler-util.cpp` collects versions for a compiler kind and uses the generic selector while choosing a compiler object.
  Compiler-kind filtering, discovery, and cross-tool fallback belong to Compiler's [Downstream Tools](../../compiler/downstream-tools/downstream-tools.md).
- The parser's `parseSemanticVersion` helper in `old-source/slang/slang-parser.cpp` reads modifier arguments and calls the numeric parser.
  The compiler uses extension trackers and target code to determine required target versions.
  `IRBuilder::getSemanticVersionValue` in `old-source/slang/slang-ir-insts.h` encodes versions as raw integer operands; version-decoration accessors reconstruct the value through `SemanticVersion::fromRaw`.
- The `serialize` overload in `old-source/slang/slang-serialize-ast.cpp` serializes `SemanticVersion` through its raw value, while `ReproUtil::Header` in `old-source/slang/slang-repro.h` embeds the version object in the repro format.
  The compiler owns those format contracts and their validation; Versions supplies the value and compatibility operation they use.

## Migration Constraints

The current `core` target compiles the implementation, and enabled `slang-rt` separately compiles it from the same legacy directory under runtime export settings.
Migration preserves both consumers under the [root's staged migration contracts](../../slang.md#staged-migration-contracts), without introducing another library or selecting a general source-reuse model.

The canonical `Slang::SemanticVersion` and `Slang::MatchSemanticVersion` identities, declaration names, signatures, layout, request-kind values, and raw encoding remain unchanged.
The namespace restriction is material even though these structs have no individual export annotation: `DownstreamCompilerDesc` embeds a `SemanticVersion`, and the LLVM adapter returns that descriptor through `IDownstreamCompiler::getDesc` across the loaded-library interface.
The raw IR/AST encoding and the repro header's embedded representation are additional preservation obligations.

This leaf does not settle Foundation's [failure/text/ownership cycles](../foundation.md#failure-reporting-and-basic-data-services), its [text adaptation cycles](../foundation.md#text-adaptations-and-aggregate-headers), or the root's [artifact and construction questions](../../slang.md#unresolved-architectural-dependencies).
The relevant dependency boundaries and both compilation consumers still require an agreed migration arrangement before this service is ported.
