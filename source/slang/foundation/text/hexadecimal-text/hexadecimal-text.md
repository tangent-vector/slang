<!--
SPDX-FileCopyrightText: The Khronos Group, Inc.
SPDX-License-Identifier: CC-BY-4.0
-->

# Hexadecimal Text

Hexadecimal Text renders byte sequences as readable hexadecimal rows or C-style byte initializer fragments.
It also recovers bytes from hexadecimal rows, locates dump markers within surrounding text, and checks the byte count and hash recorded by marked output.
Clients choose which bytes to present and what recovered bytes mean; artifact interpretation, built-in module saving, file output, and test execution remain with those clients.

This is a proposed public ordinary leaf within [Text](../text.md), with no child modules.
Rendering, delivery, and recovery describe service areas of one concern rather than independently movable parts of the existing class.
The implementation remains in `old-source/core/`; the [migration holds](#migration-holds) apply before any code transfer.

## Concepts

[Foundation](../../foundation.md#concepts) defines borrowed views, string slices, and stable hashes.
[Output Writers](../../output-writers/output-writers.md#concepts) defines an output writer and its append-buffer protocol.

- A **hexadecimal row** begins with two hexadecimal digits per byte and can continue with a display column after a separating space.
  The renderer uses lowercase digits, pads a short final hexadecimal column to the requested row width, and ends each row with a newline.
  Its display column preserves byte values from `0x20` through `0x7f` and replaces other bytes with `.`.
- A **byte initializer fragment** is a sequence of C-style hexadecimal byte constants separated by commas and spaces, without a surrounding declaration or braces.
  The renderer writes each byte as `0xNN, `, with lowercase hexadecimal digits and a newline after each row.
  The hexadecimal-row parser does not read this notation.
- A **marked dump** surrounds hexadecimal rows with a start line beginning with `--START--` and a later line beginning with `--END--`.
  The marked renderer adds a byte count and a 32-bit stable hash to the start line.
  The parser checks the decoded bytes against those two values within the conversion limits described below.

## Dependencies

- [Strings](../strings/strings.md) supplies borrowed character slices, hexadecimal digit recognition, and the temporary strings and primitive integer conversions used to read marker fields.
- [Text Operations](../text-operations/text-operations.md) supplies line traversal and splitting of the start line into fields.
  Hexadecimal Text determines which characters represent bytes and how markers delimit a dump.
- [Sequence Storage](../../collections/sequence-storage/sequence-storage.md) supplies `List<uint8_t>` byte storage and the temporary list of header-field slices.
- [Hashing](../../hashing/hashing.md) supplies `StableHashCode32` and the fixed computation used by marked output and verification.
  Its [input-representation limits](../../hashing/hashing.md#compatibility-and-input-representation), including plain-`char` signedness for non-ASCII bytes, remain applicable.
- [Output Writers](../../output-writers/output-writers.md) supplies the concrete `WriterHelper` operations used by marked output.
  The reverse dependency on Text formatting and storage remains an [unresolved service cycle](#migration-holds).
- [Failure Reporting](../../failure-reporting/failure-reporting.md) supplies the ordinary renderer's output-buffer assertion.
  The collection, string, and writer providers also preserve their existing assertion contracts.
- The inherited product headers supply `ISlangWriter`, result codes, and result-forwarding macros; the host C++ environment supplies fixed-width integers, size types, and byte and pointer operations.
  Writing through the supplied interface requires no compiler implementation.

## Service Areas

### Rendering and Delivery

Consider the three bytes `0x41`, `0x00`, and `0x42` with a row width of three.
The ordinary renderer writes `410042 A.B` followed by a newline.
The initializer-fragment renderer writes `0x41, 0x00, 0x42, ` followed by a newline.
An empty byte range produces no rows; marked output still emits its start and end lines.
The separate scalar renderer writes exactly eight lowercase digits for a `uint32_t`, most significant digit first, without a newline.
That scalar operation formats the integer value rather than inspecting its native byte order.

The byte renderers obtain an append buffer from the supplied writer for each row and submit the characters actually written.
Marked output additionally submits the marker text and formatted header fields through `WriterHelper`.
Result-bearing operations stop at the first failing writer result, leaving any earlier output delivered.
The scalar renderer has a `void` result and discards the writer's result.
Hexadecimal Text does not open a destination, select console behavior or writer mode, or request a flush.

Input byte ranges and list buffers are borrowed for the call, and the writer pointer is not retained.
The methods read the input while obtaining and submitting writer storage; they do not copy the complete input into an independent buffer first.
The caller's input lifetime and the selected writer's append-buffer and storage rules therefore remain relevant when storage overlaps or a writer operation mutates backing storage.
This leaf introduces no additional overlap guarantee.

The implementations do not validate the row width or line-size arithmetic.
For nonempty input, a zero width prevents the row loops from advancing.
Existing compiler clients supply fixed widths: `ArtifactOutputUtil::write` uses 24 for a nontext artifact sent to a console, and `OptionsParser::writePendingBuiltinModuleSaves` uses 16 when writing a built-in module as initializer fragments through a string-backed writer.
The clients own those format selections; invalid-width handling remains existing implementation debt rather than a newly checked input contract.

### Recovering Bytes and Finding Markers

The ordinary parser clears the destination list, then reads hexadecimal pairs from the beginning of each line.
It accepts uppercase and lowercase digits.
A space, tab, carriage return, or newline at a pair boundary ends consumption of that line, allowing the parser to ignore a display column.
Leading whitespace skips the whole line rather than being trimmed.
The parser reports failure when a consumed pair contains a nonhexadecimal character; any preceding decoded bytes remain appended.
A final single character on a line is ignored because the parser only attempts complete pairs.
For example, `410` decodes to one byte, whereas `4 ` fails; the parser is not a strict validator of every character in a row.

Line traversal recognizes CR, LF, and adjacent unlike CR/LF pairs and also reads a final unterminated line.
Marker search selects the first line whose prefix is `--START--`, then the first later line whose prefix is `--END--`.
Text before and after that selected region is ignored by marked parsing.
The returned marker slices borrow the original text and exclude line terminators.
Search assigns the start output before looking for the end; a missing end does not undo that assignment.
Marker recognition tests prefixes rather than exact line equality.

Marked parsing splits the start line on ASCII space and requires exactly three resulting fields.
It converts the count through `stringToInt` and the hash through `stringToUInt`, then invokes the ordinary parser on the intervening text.
Those conversions accept numeric prefixes through the existing C-runtime operations and narrow to their existing integer return types; they do not report complete-field validation or overflow through a parse result.
The marked renderer formats the count with `%zu` and `hash.hash` with `%d`.
These existing formatting and conversion choices do not establish a checked full-range numeric round trip.
After decoding, marked parsing compares both the recovered byte count and the recomputed stable hash with the header values.

A marker-search failure or a wrong header-field count leaves the destination byte list untouched.
Once ordinary parsing begins, the list is cleared and populated directly: failed decoding can leave a prefix, and a final count or hash mismatch leaves the decoded bytes in the list.
Input slices borrow their characters, and parsing does not snapshot them before clearing or extending the output list.
These mutation effects do not provide rollback or an independent copy for overlapping input and output storage.
The test runner's `_createArtifactFromHexDump` uses a separate local byte list and transfers its storage into an artifact blob only after marked parsing succeeds.
Artifact reconstruction and execution remain test-runner services.

## Existing Implementation Mapping

The following assignment covers both complete files under `old-source/core/`, including internal support and every overload.
All operations remain members of the existing `Slang::HexDumpUtil` class.

| Existing Implementation   | Assigned Responsibility                                                                                                                                                                                            |
| ------------------------- | ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------ |
| `slang-hex-dump-util.h`   | Complete `HexDumpUtil` declaration: initializer-fragment output, list and pointer/count ordinary output, scalar output, list and pointer/count marked output, ordinary and marked parsing, and marker-line search. |
| `slang-hex-dump-util.cpp` | All corresponding definitions, the lowercase digit table and fixed marker strings, list-to-buffer forwarding, row construction and writer delivery, pair decoding, `_findLine`, and size/hash verification.        |

The product `ISlangWriter` definition remains in `include/slang.h`, and concrete writers retain Output Writers' ownership.
Character storage and digit recognition retain Strings' ownership; line traversal and field splitting retain Text Operations' ownership; stable-hash computation retains Hashing's ownership.
No client artifact, compiler option, file-writing operation, or test-runner operation is reassigned with these files.

The current CMake source glob selects the assigned implementation for static `core` and separately for enabled `slang-rt`, which recompiles the legacy core directory under runtime export settings.
This ordinary leaf creates no additional build artifact or installed product header.
Any later port must preserve both compilation consumers and the [root's staged migration contracts](../../../slang.md#staged-migration-contracts), including existing declaration names, signatures, behavior, and applicable identity and export constraints.

## Migration Holds

`HexDumpUtil::dumpWithMarkers` calls Output Writers' out-of-line `WriterHelper::print` and `put` operations.
`WriterHelper::print` calls Text Operations' formatted-size and formatting operations, and string-backed writers use Strings' storage.
The resulting Text to Output Writers to Text cycle remains the [unresolved adaptation boundary recorded by Text](../text.md#hexadecimal-output-and-writers).
Parsing and delivery also share the complete `HexDumpUtil` declaration, so describing separate service areas does not physically split that class.
Foundation must settle the boundary while preserving the existing class, signatures, and behavior before these services can migrate.
This refinement selects no replacement API, namespace, formatter, callback protocol, or artifact to remove the cycle.

The lower-level text, collection, native-ownership, and failure dependencies remain subject to [Foundation's existing service-cycle holds](../../foundation.md#failure-reporting-and-basic-data-services).
The permissive parsing, unchecked row sizing, and numeric header conversions described above remain behavior to preserve during structural work.
Hexadecimal delivery also retains Output Writers' [existing append-protocol limits](../../output-writers/output-writers.md#existing-append-protocol-limits).
These inherited limits do not authorize behavior repairs as part of this ownership refinement or a later structural port.
