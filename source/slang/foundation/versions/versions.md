<!--
SPDX-FileCopyrightText: The Khronos Group, Inc.
SPDX-License-Identifier: CC-BY-4.0
-->

# Versions

Versions provides numeric version values, ordering, compatibility checks, textual conversion, and selection among supplied candidate versions.
Clients decide what a version identifies and whether a particular compatibility or selection rule applies.
Compiler discovery, target requirements, and stored-format validation remain with those clients.

The implementation resides in this directory and remains a public leaf within [Foundation's existing artifact](../foundation.md#existing-implementation-mapping).
`versions.h` defines the canonical `Slang::SemanticVersion` and `Slang::MatchSemanticVersion` types and explicitly reexports those same declarations in `slang::foundation::versions`.
Foundation's primary header includes the leaf primary and reexports those two types in `slang::foundation`.

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

The primary header retains the broad `core/slang-basic.h` prerequisite and directly includes the migrated Hashing primary while the remaining prerequisite services are unported.
The broad aggregate assembles several Foundation services; it does not assign their implementations to Versions or create a semantic dependency on the assembled Foundation parent interface.
The implementation directly includes `core/slang-string-util.h` for the existing parsing operations.
The result-checking macro comes from `include/slang-com-helper.h` without requiring a dependency on an object-interface implementation.

## Service Areas

Versions remains a leaf with no child modules.
The value representation, text conversion, and selection rules share the same component ordering and remain one small service:

- **Values and ordering:** construction, reset, component access, raw-value import/export, comparison, hashing, and earliest/latest queries.
  Earliest/latest return an all-zero version for a nonpositive candidate count; that result is not a distinct optional value.
- **Text conversion:** parsing the existing separated numeric notation and appending its formatted form.
  Parsing splits into at most three components using `.` by default or a supplied separator.
  The parser uses `StringUtil::parseInt` for each component, including that routine's decimal and hexadecimal integer notation.
  It rejects negative values or values above `0xffff` for major/minor and `0x7fffffff` for patch.
  Missing lower components remain zero.
  Parsing resets the output before inspecting the input and leaves it zero on failure.
  The component parser accepts an optional sign without whitespace; `SemanticVersion::parse` accepts signed zero and rejects an effective negative value.
  `StringUtil::split` accepts a trailing separator after one or two components, but rejects one after three: parsing `"1."` and `"1.2."` succeeds, while parsing `"1.2.3."` fails.
  `SemanticVersion::append` always emits major and minor components, with a patch component only when nonzero.
  `MatchSemanticVersion::append` emits the preference word or the specified numeric components; for a complete numeric request, the method delegates to the value formatter and can therefore omit a zero patch without changing the request's kind.
- **Compatibility and matching:** the directional compatibility relation and best-effort choice from caller-supplied candidates.
  A nonpositive count yields the all-zero version; a single candidate is returned regardless of the request.
  Positive counts require a readable input range; unsorted and duplicate candidates are accepted, and selection does not modify the input elements.
  For multiple candidates, `findAnyBest` returns the earliest for a past preference and the latest for an unknown or future preference.
  For numeric requests, `findAnyBest` uses bounds derived by incrementing the last specified component and a sorted copy of the candidates.
  The selector skips candidates below the lower bound, then scans forward while candidates are below the upper bound.
  It returns the greatest candidate in that span when the span is nonempty; otherwise it returns the least candidate at or above the lower bound, or the greatest below it when no such following candidate exists.
  For example, for a `2.1` request, `findAnyBest` prefers `2.1.7` over `2.1.3`, but can select `2.2` when no `2.1` candidate is present.

The field widths do not imply that every construction or import entry point accepts every representable bit pattern.
The integer constructor narrows its arguments directly to the fields without a nonnegative assertion, while `SemanticVersion::set` first asserts that its signed inputs are nonnegative.
The three `MatchSemanticVersion::set` overloads convert their `Index` arguments to `int` and use the narrowing constructor without that assertion.
`setRawValue` converts the extracted components to signed `int` and calls `set`; its behavior is not an unconditional full-width round-trip guarantee.
If that conversion produces a negative patch value, `set` applies its nonnegative assertion or optimizer assumption, even though the patch's bits fit the unsigned encoding.
Numeric matching retains finite-width bound arithmetic: incrementing the maximum component can narrow or wrap without carrying into a higher component.

## Existing Implementation Mapping

`versions.h` owns the complete declarations formerly in `old-source/core/slang-semantic-version.h`: `SemanticVersion`, `MatchSemanticVersion`, their aliases and request kinds, inline operations, and fields.
`versions.cpp` owns the complete implementation formerly in `old-source/core/slang-semantic-version.cpp`: parsing, formatting, extrema, compatibility, and candidate selection.
Both legacy files have been removed; include clients use the new primary rather than a forwarding header.
Neither transferred file contains a separate compiler or host-discovery service requiring a partial-file assignment.

The following uses remain with their existing owners:

- `DownstreamCompilerUtil::findClosestCompiler` in `old-source/compiler-core/slang-downstream-compiler-util.cpp` collects versions for a compiler kind and uses the generic selector while choosing a compiler object.
  Compiler-kind filtering, discovery, and cross-tool fallback belong to Compiler's [Downstream Tools](../../compiler/downstream-tools/downstream-tools.md).
- The parser's `parseSemanticVersion` helper in `old-source/slang/slang-parser.cpp` reads modifier arguments and calls the numeric parser.
  The compiler uses extension trackers and target code to determine required target versions.
  `IRBuilder::getSemanticVersionValue` in `old-source/slang/slang-ir-insts.h` encodes versions as raw integer operands; version-decoration accessors reconstruct the value through `SemanticVersion::fromRaw`.
- The `serialize` overload in `old-source/slang/slang-serialize-ast.cpp` serializes `SemanticVersion` through its raw value, while `ReproUtil::Header` in `old-source/slang/slang-repro.h` embeds the version object in the repro format.
  The compiler owns those format contracts and their validation; Versions supplies the value and compatibility operation they use.

## Migration Constraints

The leaf `CMakeLists.txt` registers the new header and implementation on the existing `core` target and, when enabled, the existing `slang-rt` target.
Foundation's CMake entry enters this child after both legacy target definitions have been evaluated.
Removing the legacy pair removes those paths from the old source globs; runtime separately compiles `versions.cpp` under its runtime export settings instead of linking `core`.
This arrangement preserves both consumers under the [root's staged migration contracts](../../slang.md#staged-migration-contracts), without introducing another library or selecting a general source-reuse model.

The canonical `Slang::SemanticVersion` and `Slang::MatchSemanticVersion` identities, declaration names, signatures, layout, request-kind values, and raw encoding remain unchanged.
The namespace restriction is material even though these structs have no individual export annotation: `DownstreamCompilerDesc` embeds a `SemanticVersion`, and the LLVM adapter returns that descriptor through `IDownstreamCompiler::getDesc` across the loaded-library interface.
The raw IR/AST encoding and the repro header's embedded representation are additional preservation obligations.

This leaf does not settle Foundation's [failure/text/ownership cycles](../foundation.md#failure-reporting-and-basic-data-services), its [text adaptation cycles](../foundation.md#text-adaptations-and-aggregate-headers), or the root's [artifact and construction questions](../../slang.md#unresolved-architectural-dependencies).
The direct prerequisite routes and per-leaf registration provide the concrete staging arrangement for Versions; they do not resolve those broader dependency and construction questions.
