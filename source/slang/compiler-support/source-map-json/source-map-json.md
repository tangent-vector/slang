# Source Map JSON

Source Map JSON converts generated-to-originating location maps between [Source Text's in-memory representation](../source-text/source-text.md) and JSON values or blobs.
Clients can convert an existing JSON value, parse a blob into a map, construct a JSON value from a map, or write a map as a JSON blob.
The module encodes correspondences supplied by its callers; it does not decide which program locations correspond.

This is a proposed public leaf of [Compiler Support](../compiler-support.md).
The implementation remains in `old-source/compiler-core/`; no code has moved into this directory.
Compiler code constructs mappings, Source Text stores and looks up those mappings, and [Artifacts](../artifacts/artifacts.md) chooses when to convert an artifact's map representation to or from a blob.
Those relationships keep JSON interchange above the map representation without introducing a Source Text dependency on JSON.

## Concepts

[Compiler Support](../compiler-support.md#concepts) defines a source map and a JSON value.
[Source Text](../source-text/source-text.md#concepts) defines the map's zero-based generated and originating line/column indices and its separately stored strings.
The adapter transfers those numeric coordinates without converting column units.
[Foundation](../../foundation/foundation.md#concepts) defines blobs and borrowed views; [Text](../../foundation/text/text.md#concepts) defines pool-local string handles.
The adapter's native record uses [native type descriptions](../../foundation/type-descriptions/type-descriptions.md#concepts), rather than shader reflection, to describe its JSON fields.

A **mapping segment** is the encoded sequence for one entry on a generated line in the JSON `mappings` string.
Commas separate segments, and semicolons separate generated lines.
The integer helpers encode signed deltas using five payload bits per character, a continuation bit, and a low sign bit, with the least-significant payload group first.
The field names and handling below describe the existing implementation; they do not establish validation against an external source-map specification.

## Dependencies

- [Source Text](../source-text/source-text.md) supplies `SourceMap`, its entries and pooled strings, and the source manager, file, and view used when parsing a blob.
  Source Text owns map construction, indexing, lookup, comparison, and the map's existing type identity.
- [JSON](../json/json.md) supplies values and containers, JSON/native conversion, lexing, parsing, building, and writing.
  Source Map JSON supplies its own field description and interprets the converted mapping data.
- [Diagnostics](../diagnostics/diagnostics.md) supplies sinks for JSON failures and optional forwarding to a caller's sink.
  The blob reader creates a local sink, and the sink-taking writer obtains its source manager from the supplied sink.
- Foundation's [Text](../../foundation/text/text.md) supplies strings, builders, borrowed slices, splitting, and copied pool storage; [Collections](../../foundation/collections/collections.md) supplies temporary sequences.
  [Type Descriptions](../../foundation/type-descriptions/type-descriptions.md) supplies the structure-field description and conversion operation map used by JSON conversion.
- Foundation's [Native Ownership](../../foundation/memory/native-ownership/native-ownership.md) supplies retained temporary JSON containers, and [Blobs](../../foundation/blobs/blobs.md) supplies the string-backed output blob.
  [Scalar Types](../../foundation/base-types/scalar-types/scalar-types.md) supplies `Index` and `Count`; [Failure Reporting](../../foundation/failure-reporting/failure-reporting.md) supplies assertions on decoded accumulated values.
- The product headers supply `ISlangBlob`, `ComPtr`, result codes, and shared portability declarations.
  The host C++ environment supplies fixed-width integers, byte initialization, and the minimum operation used to limit the contents list.
  These dependencies do not assign the product API or compiler semantics to this module.

## Decomposition

Source Map JSON has no immediate children.
Its principal service areas are:

- **Value conversion:** adapting map metadata and pooled strings to a described JSON object, and converting that object back into independently stored map data.
- **Mapping conversion:** packing and unpacking generated-line entries with the shared signed-delta state described below.
- **Blob conversion:** composing the value conversion with JSON parsing or writing, local source-location storage, and diagnostic routing.

These services form one adaptation: the blob operations call the value operations, and the value operations combine the field record with the mapping codec.
The field record and integer helpers have no separate client surface or independent service assignment that would justify child modules.
Generic JSON conversion remains with JSON, and the in-memory map remains with Source Text.

## Interchange Fields and Mapping Behavior

`JSONSourceMapUtil` uses the private `JSONSourceMap` native record and its `StructRttiInfo` to describe these fields in this order.
“Required” below means that the named-field converter requires the field to be present; it does not imply strict validation of every JSON value accepted by the generic conversion routines.
The three optional fields may be omitted on input, and the generic writer omits them when their values pass the field-default test described by [Type Descriptions](../../foundation/type-descriptions/type-descriptions.md#concepts).

| Field            | Presence | Existing Adaptation                                                                                                                                                                                                                                             |
| ---------------- | -------- | --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| `version`        | Required | An `int32_t` initialized to `3` for encoding. Decoding converts the value but does not check that it is `3` or positive.                                                                                                                                        |
| `file`           | Required | A string copied to or from `SourceMap::m_file`.                                                                                                                                                                                                                 |
| `sourceRoot`     | Optional | A string copied to or from `m_sourceRoot`; the adapter does not prepend it to file names or resolve paths.                                                                                                                                                      |
| `sources`        | Required | An ordered list of file-name slices copied to or from the map's pool-backed file-name list.                                                                                                                                                                     |
| `sourcesContent` | Optional | A list of raw JSON values on input. Decoding retains at most the number of `sources`, copies string-kind contents, and leaves null or other kinds as null pool handles. Encoding writes each stored content string and represents a null handle as JSON `null`. |
| `names`          | Optional | An ordered list of name slices copied to or from the map's pool-backed name list.                                                                                                                                                                               |
| `mappings`       | Required | A string converted to or from generated-line entries by the signed-delta helpers.                                                                                                                                                                               |

The `sources` and `names` list positions remain unchanged even when the pool shares storage for equal spellings.
The adapter does not interpret file names or names as compiler declarations.
The named-field converter rejects unknown fields under the record's existing description policy; other conversion behavior remains with JSON.

Decoding splits `mappings` into lines and segments, then accumulates the decoded deltas.
The generated-column accumulator starts at zero for each generated line.
The originating file index, line, column, and name index start at zero once and continue across generated lines.
After the generated-column delta, any remaining bytes cause the decoder to require the originating file, line, and column deltas; any bytes remaining after that triplet cause it to read one name delta.
An omitted triplet or name delta leaves those accumulators unchanged, and every decoded segment becomes a complete `SourceMap::Entry`.
For example, the one-integer segment `A` adds a zero generated-column delta and uses the current originating and name values.
The decoder does not create a distinct unmapped-entry representation for that segment.

The line count follows the existing map and splitting APIs exactly.
`StringUtil::split` preserves interior empty slices but does not append an empty slice after a trailing separator.
The decoder allocates one more line start than the number of split lines and sets the final start to the entry count.
`SourceMap::getGeneratedLineCount` counts that final start, and the encoder iterates that count unchanged.
For example, decoding `mappings: "A;"` produces one entry and two line starts, and encoding that map writes `A;`.

Encoding preserves entry order within each generated line and uses the same accumulator reset rules.
It always writes a generated-column delta.
The existing condition for writing the originating triplet is exactly `sourceFileDelta || sourceLineDelta || sourceColumnDelta || nameIndex`, where `nameIndex` is the previous name accumulator.
Within that branch, the encoder writes a name delta only when `nameIndexDelta` is nonzero.
A name-only change with a previous name index of zero therefore does not by itself cause the encoder to write that triplet or name delta.
This condition, the line-count arithmetic, and the handling of omitted fields are inherited behavior to preserve during any later port.

## Ownership, Mutation, and Failure

`decode` receives its `JSONValue` argument by value and borrows the container and diagnostic sink for the call.
Copying the value does not retain its backing storage.
The container and any text backing lexeme values must remain available while conversion reads them.
The resulting map owns its copied strings through `String` and its slice pool, so a successful result does not retain the container or its parsed text.
`decode` clears the destination before conversion.
A field-conversion failure leaves that destination cleared; a later mapping failure can leave copied metadata, strings, and earlier entries in a partially populated map.
Failure does not roll the destination back or establish that the partial map is usable.

`encode` borrows a const map and writes values into the supplied container.
Temporary slices refer to the map's strings and a local mapping builder until the generic converter copies their contents into the container.
The returned JSON root relies on the container's lifetime, and an encoding failure does not undo allocations already made in that container.
The direct conversion operations require valid container and sink lifetimes; they do not provide the blob reader's optional-parent-sink contract.
Neither encoding nor writing mutates the supplied map.

`read` clears its destination, creates a local source manager, file/view, sink, and JSON container, parses the blob, and calls `decode`.
Its parent sink is optional; the overload without a sink forwards null to that parent-sink parameter.
The local objects keep parsing state available for the call, but a blob that borrows bytes still requires its caller to keep those bytes valid while reading.
A parse failure leaves the destination cleared, and a decode failure has the partial-mutation effects described above.

The sink-taking `write` overload requires a non-null sink because it immediately obtains that sink's source manager.
The convenience overload creates its own manager and sink.
Writing encodes into a local container, traverses the resulting JSON with `JSONWriter`'s Allman indentation style, and moves the completed string into a `StringBlob`.
`outBlob` is assigned only after encoding succeeds, so a returned encoding failure leaves its previous value unchanged.

The mapping decoder returns failure for a required character that is missing or has a negative decode-table entry.
Those failures need not emit diagnostics.
The code uses assertions for nonnegative accumulated coordinates and indices; it does not add list-index bounds checks, ordering checks, an integer-width limit, or a check for unused bytes after the optional fifth integer.
The decode-table initialization includes the alphabet array's terminating zero byte, so the helper is not a strict alphabet validator.
The encoder retains signed `Index` arithmetic and its fixed eight-byte temporary buffer.
These inherited limits preclude a stronger malformed-input or arbitrary-integer guarantee and are outside this document-only refinement's repair scope.

## Existing Implementation Mapping and Consumers

The assignments cover both complete files under `old-source/compiler-core/`:

- [`slang-json-source-map-util.h`](../../../../old-source/compiler-core/slang-json-source-map-util.h) declares `Slang::JSONSourceMapUtil` and all `decode`, `encode`, `read`, and `write` overloads.
- [`slang-json-source-map-util.cpp`](../../../../old-source/compiler-core/slang-json-source-map-util.cpp) defines the private JSON-facing record and its type description, the alphabet and decode table, both integer helpers, and all utility operations.

No partial-file reassignment is needed for those declarations and definitions.
The generic converters and JSON storage remain in JSON's files, while `slang-source-map.{h,cpp}` remains wholly with Source Text.

The production client is [`slang-artifact-handler-impl.cpp`](../../../../old-source/compiler-core/slang-artifact-handler-impl.cpp).
`_loadSourceMap` chooses JSON-kind artifacts with a source-map payload and loads a blob before calling `read`.
`DefaultArtifactHandler::getOrCreateRepresentation` boxes decoded maps, applies artifact retention, and calls `write` when converting an existing map representation to a blob.
Artifact kind/payload selection, boxing, caching, retention, and materialization remain Artifacts policies.
The existing [`unit-test-source-map.cpp`](../../../../tools/slang-unit-test/unit-test-source-map.cpp) directly calls `decode` and `encode` and checks a read/write/read cycle, copying, and assignment; that coverage does not establish the stronger validation guarantees excluded above.

The current source selection still compiles the codec and its artifact client into the static `compiler-core` target.
The top-level [`CMakeLists.txt`](../../../../CMakeLists.txt) selects the legacy compiler-core directory; its [`CMakeLists.txt`](../../../../old-source/compiler-core/CMakeLists.txt) uses `slang_add_target` without an explicit source list.
[`SlangTarget.cmake`](../../../../cmake/SlangTarget.cmake) and [`Glob.cmake`](../../../../cmake/Glob.cmake) select the directory's implementation files recursively.
The [compiler variants](../../../../old-source/slang/CMakeLists.txt) link `compiler-core`, as does the `slang-unit-test` module in [`tools/CMakeLists.txt`](../../../../tools/CMakeLists.txt), which preserves direct access to the codec's internal definitions.
The [runtime build](../../../../old-source/slang-rt/CMakeLists.txt) separately recompiles legacy `core`, not this compiler-core implementation.
A later migration must preserve these actual consumers and the [root's names, signatures, behavior, and identity contracts](../../slang.md#staged-migration-contracts); this refinement introduces no new library and does not establish migration readiness.
