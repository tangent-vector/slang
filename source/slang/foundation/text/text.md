<!--
SPDX-FileCopyrightText: The Khronos Group, Inc.
SPDX-License-Identifier: CC-BY-4.0
-->

# Text

Text provides character storage and bounded views, encoding conversion, shared spelling storage, text transformation, formatting, and reusable parsing of textual notation.
Clients supply the meaning of the text: compiler declarations, diagnostic messages, paths, option values, and stored formats remain responsibilities of their respective owners.

The implementation remains in `old-source/`.
The children below refine [Foundation's assignment](../foundation.md#existing-implementation-mapping); the [unresolved boundaries](#unresolved-boundaries) prevent treating the proposed decomposition as an accepted dependency graph or a migration plan.

## Concepts

[Foundation](../foundation.md#concepts) defines owned allocations, borrowed views, intrusive references, string slices, blobs, and streams.
The string-slice distinction matters throughout Text: an explicitly bounded range need not end at a zero byte, and a borrowed range does not keep its characters alive.

- A **slice pool** identifies each stored distinct character sequence by a pool-local integer handle.
  The existing default pool style reserves different handles for a null value and an empty string; the empty style starts without reserved entries.
  These handles differ from Compiler Support's [interned names](../../compiler-support/names/names.md#concepts), which identify retained name objects by pointers.
- **Escaping** replaces characters with a notation's escape sequences; **quoting** surrounds the payload with delimiters.
  The selected handler determines whether either operation is needed and which sequences it accepts.
- A **notation token** is an item recognized by the reusable `Slang::Misc` reader, with its own category, owned content string, position, and flags.
  It is separate from Compiler Support's [token](../../compiler-support/tokens/tokens.md#concepts), whose spelling and location follow the compiler's borrowing and location-manager contracts.

Lengths and indices in the narrow string and slice APIs count bytes, rather than Unicode characters.
The APIs commonly interpret those bytes as UTF-8, but constructing or appending a string from bytes does not validate that interpretation.
Wide strings count `wchar_t` units, and encoding conversions have their own input assumptions.

## Dependencies

- [Scalar Types](../base-types/scalar-types/scalar-types.md) supplies byte, character-code-unit, count, and index types.
  [C++ Utilities](../base-types/cpp-utilities/cpp-utilities.md) supplies move, swap, annotations, and runtime visibility declarations.
  [C Runtime Compatibility](../base-types/c-runtime-compatibility/c-runtime-compatibility.md) supplies the bounded C string and formatting wrappers, and [Numeric Operations](../base-types/numeric-operations/numeric-operations.md) supplies the minimum operation used by edit-distance calculation.
- [Memory](../memory/memory.md) supplies intrusive retention for shared string representations and arena storage for slice pools.
  Text determines which character storage to retain or copy; Memory supplies the ownership and allocation mechanisms.
- [Collections](../collections/collections.md) supplies conversion buffers, token sequences, split results, edit-distance working rows, and the slice pools' lookup and index storage.
  Splitting, token recognition, edit-distance calculation, and spelling deduplication are Text operations using those containers.
- [Hashing](../hashing/hashing.md) supplies byte hashing for string keys, stable-hash values formatted by strings, and the stable hash recorded and checked by hexadecimal dumps.
  Text defines the character-content key operations and the dump format.
- [Failure Reporting](../failure-reporting/failure-reporting.md) supplies assertions and signals used for string, pool, encoding, quoting, and output-buffer preconditions, together with the base exception used by notation parsing.
  Notation Reading constructs its own text-format exception when exceptions are enabled and calls the signal path when they are disabled.
- The product headers supply result codes, platform selection, the blob interface, and the writer interface.
  Reading a supplied blob or writing through a supplied interface does not require the compiler implementation.
- [Blobs](../blobs/blobs.md) supplies the concrete string-blob construction called by `StringUtil::createStringBlob`.
  [Output Writers](../output-writers/output-writers.md) supplies the out-of-line writer operations called by marked hexadecimal output.
  Both relationships participate in the unresolved cycles described below.
- The host C and C++ environment supplies character-buffer allocation, byte operations, numeric conversion, variadic formatting, math functions, and the `std::ostream` contract used by Text's insertion adapter.

The broad `slang-basic.h` and `slang-text-io.h` includes do not establish additional service dependencies by themselves.
In particular, wide-character conversion in `slang-string.cpp` uses encoding operations without opening files or using streams.
The byte-reader templates in `slang-char-encode.h` invoke a supplied callable; supplying a stream-reading callable does not make the conversion service depend on Host Services.
Likewise, caller-supplied equality functions in `StringUtil` do not give Text ownership of their comparison policy.

## Decomposition

The following children are public ordinary modules within Foundation.
Compiler, Compiler Support, host utilities, and existing repository tools use their services directly.
Public visibility has the implementation-only meaning defined by the [root document](../../slang.md#concepts).

| Child                                                    | Service                                                                                                                                          |
| -------------------------------------------------------- | ------------------------------------------------------------------------------------------------------------------------------------------------ |
| [Strings](strings/strings.md)                            | Character storage, borrowed and retained slices, character classification, encoding conversion, comparison, and primitive string construction.   |
| [Text Operations](text-operations/text-operations.md)    | Splitting, joining, line traversal, replacement, edit distance, numeric parsing and formatting, and lookup in caller-supplied name/value tables. |
| [String Interning](string-interning/string-interning.md) | Pool-owned spelling storage, pool-local handles, and spelling-to-integer maps.                                                                   |
| [Escaping](escaping/escaping.md)                         | Quoting, escaping, unquoting, and unescaping with selectable textual conventions.                                                                |
| [Notation Reading](notation-reading/notation-reading.md) | Reusable token recognition, token-sequence reading, and typed reads for small textual notations.                                                 |
| [Hexadecimal Text](hexadecimal-text/hexadecimal-text.md) | Rendering byte sequences as hexadecimal text and reading the dump notation, including optional size and hash markers.                            |

Strings groups character ranges, storage, and primitive conversions so clients can acquire, retain, compare, and convert text through one concern.
Numeric append members remain with the complete `String` class contract; the separate reusable formatting operations belong to Text Operations.
Text Operations, String Interning, Escaping, and Notation Reading use Strings.
String Interning additionally uses Memory's arenas and Collections' lookup storage; Text Operations uses Collections for split results and working buffers.
Hexadecimal Text uses Text Operations for line and header parsing and Output Writers for marked output.
These relationships describe the proposed responsibilities; each child still requires its own refinement, and none removes the cross-service cycles.

## Storage and Text Contracts

### Owned and Borrowed Characters

`String` retains a backing `StringRepresentation` when one is present; copy construction and copy assignment share that representation.
A zero-length `String` can have either no representation or an allocated representation.
Ordinary append and length-reduction operations separate shared storage before modifying characters.
`StringSlice` retains a backing representation when present and bounds a range within that storage; without a representation, its accessors return an empty range.
`UnownedStringSlice` and `UnownedTerminatedStringSlice` borrow their characters.
The terminated borrowed form additionally requires a zero byte after its range, outside its reported length.
`OSString` owns its `new wchar_t[]` buffer when one is present and exposes static empty text otherwise.

A borrowed slice remains usable only while its particular backing storage is valid.
For example, obtaining a slice from a temporary `String` does not retain that temporary's allocation, and later mutation of a string may replace its allocation.
Direct access to `StringRepresentation` and the prepared append buffer exposes mutable storage; reference counting does not make those accesses immutable or synchronize character mutation.
Committing a nonempty prepared append requires the pointer returned by `prepareForAppend`, still-unique storage, and sufficient reserved capacity.

Explicit byte lengths can describe embedded zero bytes.
The existing comparisons do not all interpret them alike: slice equality compares the entire bounded range, whereas the `String`-to-`String` relational operators use C string comparison and stop at a zero byte.
`String::getHashCode` hashes the full represented range.
Clients choosing string keys must respect those existing equality and hashing limits; this decomposition does not introduce a new comparison rule.

### Conversion, Parsing, and Formatting

`CharUtil` classification and string upper/lower conversion use ASCII ranges rather than Unicode case mapping.
The encoding services convert between narrow text and the existing UTF-16, byte-reversed UTF-16, and UTF-32 representations.
Encoding inference uses byte-order marks and zero-byte heuristics, falling back to UTF-8.
The encoding services do not provide a common Unicode-validation guarantee: the UTF-8 decoder accepts values beyond Unicode's scalar range, and the code-point counting helper explicitly assumes valid, complete input.
The conversion implementations also retain their existing byte-order and alignment assumptions.

Text Operations provides both bounded parsing and C-runtime-based conversion.
Their failure contracts differ: `parseInt` and `parseInt64` reject overflow and unconsumed trailing characters, `parseIntAndAdvancePos` consumes a numeric prefix and uses zero for failure, and `parseDouble` delegates to `atof` and returns success without validating the complete input.
Variadic formatting follows its format string and requires correctly typed arguments; the argument-array formatter supports its implemented subset of conversions rather than every `printf` format.
Name/value lookup borrows the caller's table strings and descriptions; selecting the table's meanings remains with the caller.

Escaping offers C++-style, JSON-style, and space-based conventions; the current Slang style selects the C++ handler.
The handlers have distinct recognition and failure behavior, so naming a style does not promise complete validation against a language specification.
Some operations append output before reporting failure, and the convenience string-returning wrappers do not expose the handler result code.
Process argument escaping and stream-based text I/O remain with [Host Services](../host-services/host-services.md).

### Shared Spellings and Notation

Inserting a new nonempty spelling into a slice pool copies its characters into arena-owned, zero-terminated storage.
The reserved null entry has no characters.
The default style's reserved empty entry and empty spellings newly added to an empty-style pool use static empty text.
Copying a pool reconstructs added entries in arena storage, so a copied empty entry can instead use pool-owned storage.
Repeated spelling lookup returns the existing handle; the handle's interpretation belongs to that pool and its selected style.
Slice values copied from pool entries do not retain character storage, and references to the entries themselves additionally depend on the slice list's storage.
Clearing, assigning, or destroying a pool can invalidate its arena-backed characters and list-entry references; the static empty characters remain valid.
Swapping transfers the entries and any arena-owned storage to the other pool.
`StringSliceIndexMap` uses an empty-style pool to own keys and separately stores the integer values associated with them.
Source maps, JSON storage, and archive path lookup use these services without assigning their own data models to Text.

Notation Reading eagerly tokenizes input into its own token sequence and supplies lookahead, advancement, expected-token checks, and numeric or string reads.
The implementation tracks filename, line, column, and position using its own position type; it does not allocate Compiler Support source locations.
For example, the compiler's SPIR-V snippet parser uses this reader to consume words and punctuation, then interprets instruction names through the supplied grammar and reports compiler diagnostics itself.
That client use does not assign SPIR-V grammar, semantic checking, preprocessing, or Compiler Support's language lexer to Text.

## Existing Implementation Mapping

Unless otherwise stated, paths below are relative to `old-source/core/`.
A basename followed by `{h,cpp}` denotes both complete files, including inline operations and internal implementation support, except for the explicitly partial `StringUtil` assignment.

| Existing Implementation                                                      | Proposed Owner and Responsibility                                                                                                                                                                                                                                                              |
| ---------------------------------------------------------------------------- | ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| `slang-string.{h,cpp}`                                                       | Strings: all string and slice types, representation storage, ownership and mutation, builders, character and numeric append operations, conversions, free comparison and concatenation, ASCII integer formatting, numeric conversion helpers, `EndLine`, and the C++ stream insertion adapter. |
| `slang-char-util.{h,cpp}`                                                    | Strings: ASCII character classification and case conversion, digit conversion, the classification table, and its existing linkage support.                                                                                                                                                     |
| `slang-char-encode.{h,cpp}`                                                  | Strings: encoding vocabulary and objects, byte-reader decoding templates, code-point encoding, byte-order helpers and marks, encoding inference, and character-count utilities.                                                                                                                |
| `slang-string-util.{h,cpp}`, except the three blob-facing members below      | Text Operations: the non-blob `StringUtil` operations, their local formatting/parsing and edit-distance implementation, and `LineParser` with its iterator.                                                                                                                                    |
| `slang-name-value.{h,cpp}`                                                   | Text Operations: integer-backed table entries, optional aliases and descriptions, lookup, and name enumeration, including typed conversion wrappers.                                                                                                                                           |
| `slang-string-slice-pool.{h,cpp}` and `slang-string-slice-index-map.{h,cpp}` | String Interning: complete pool and spelling-to-integer-map declarations and implementations.                                                                                                                                                                                                  |
| `slang-string-escape-util.{h,cpp}`                                           | Escaping: the handler contract, concrete convention handlers, quoting and escape operations, shell-like quoted-section composition, and string convenience wrappers.                                                                                                                           |
| `slang-token-reader.{h,cpp}`                                                 | Notation Reading: all `Slang::Misc` token and position declarations, exception and failure adapter, reader operations, tokenization machinery, and the separate `Split` convenience operation.                                                                                                 |
| `slang-hex-dump-util.{h,cpp}`                                                | Hexadecimal Text: hexadecimal rendering, writer delivery, line parsing, marker search, and size/hash verification.                                                                                                                                                                             |

`StringUtil::getSlice`, `getString`, and `createStringBlob` are candidates for Blobs-owned text adaptations, rather than the Text Operations service above.
The [shared-class boundary](#blob-adaptations) remains unresolved; this semantic assignment does not authorize moving those members independently of their existing class contract.
The generic name/value table operations do not assign the compilation, archive, or file-system vocabulary in `slang-type-text-util` and `slang-type-convert-util` to Text; those mappings retain [Foundation's ancestor ownership questions](../foundation.md#existing-implementation-mapping).

Compiler Support has proposed its `slang-name-convention-util.{h,cpp}` and `slang-pretty-writer.{h,cpp}` for Text.
Inspection supports Text Operations as their candidate owner: naming-convention conversion splits and joins character sequences, while pretty writing constructs a string with indentation, C++-style quoted text, and nested comma tracking.
Neither implementation inspects compiler declarations or opens an output channel.
Their compiler and generator clients choose what text to produce.
This proposed extension requires Foundation and Compiler Support to agree and propagate the assignment; the files remain in `old-source/compiler-core/` and are not included in the accepted parent mapping.

The current build compiles the assigned core implementation files into static `core` and separately into enabled `slang-rt` under runtime export settings.
The [root's staged migration contracts](../../slang.md#staged-migration-contracts) preserve both consumers and the canonical `Slang` identities, layouts, signatures, and annotations of shared string types.
In particular, runtime entry points pass `String` by value, and the string storage, slice, wide-string, and builder classes carry `SLANG_RT_API`.
A same-entity reexport does not create another representation or permit a class definition to change identity.

## Unresolved Boundaries

### Failure Reporting and Native Ownership

String and pool operations assert their preconditions; shared string storage additionally uses the checked native reference-count operations.
Failure Reporting constructs and retains a `String` message, while `StringRepresentation` derives from Memory's `RefObject`.
The resulting Text to Failure Reporting to Text and Native Ownership to Failure Reporting to Text to Native Ownership cycles are real service dependencies.
Encoding buffers and intern tables also bring Collections into the relationships described by [Foundation](../foundation.md#failure-reporting-and-basic-data-services).
Narrower includes and the Strings child do not resolve those cycles.
Foundation must establish a behavior-preserving separation or coherent owner for the mutually dependent services before the affected boundaries can migrate; no replacement assertion or exception mechanism is selected here.

### Blob Adaptations

`StringUtil::getSlice` borrows a supplied blob's bytes and omits one final zero byte when present.
`getString` copies a nonempty resulting range into string storage and returns a string without a representation for an empty range.
The slice returned by `getSlice` does not retain the blob.
`createStringBlob` calls `StringBlob::create`.
For nonempty input, the factory copies the characters into a uniquely held representation; for empty input, the blob has no string representation and exposes static empty text.
The blob implementation therefore uses Text's representation, while the existing Text utility calls the concrete blob implementation.

Assigning the three adaptation operations to Blobs expresses their purpose but does not physically split `StringUtil`.
Those static members remain declared in the same class as Text's split, formatting, and parsing operations.
Foundation must reconcile ownership of that complete declaration and its member definitions while preserving the existing class identity and signatures.
Moving only definitions or includes does not establish an acyclic Text/Blobs graph, and this refinement does not select an API rewrite to obtain one.

### Hexadecimal Output and Writers

`HexDumpUtil::dumpWithMarkers` calls Output Writers' out-of-line `WriterHelper::print` and `put` operations.
`WriterHelper::print` in turn calls Text's `StringUtil::calcFormattedSize` and `calcFormatted`; string-backed writers also use Text storage.
The Text to Output Writers to Text cycle therefore remains after distinguishing the product writer interface from its helper implementation.
Hexadecimal parsing and output are members of the same `HexDumpUtil` class, so a semantic distinction between format and delivery does not establish a physical class split.
Foundation must settle that adaptation boundary without changing the existing signatures or behavior before the affected services can migrate.
