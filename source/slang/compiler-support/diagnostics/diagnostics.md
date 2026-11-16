# Diagnostics

Diagnostics describes messages, applies reporting settings, and presents messages with locations, highlighted text, and related notes.
It provides shared reporting for the compiler, text readers, and repository tools without deciding which conditions those clients diagnose.
Clients supply the message arguments and locations, select settings and destinations, and decide how a reported error affects their operation.

This is a proposed public leaf of [Compiler Support](../compiler-support.md).
The implementation remains in `old-source/compiler-core/`; no implementation has moved into this directory.

## Concepts

[Compiler Support](../compiler-support.md#concepts) defines a diagnostic.
[Source Text](../source-text/source-text.md#concepts) defines source files, views, locations, and location ranges; [Names](../names/names.md#concepts) and [Tokens](../tokens/tokens.md#concepts) define the spellings accepted as message arguments.

- A **diagnostic description** (`DiagnosticInfo`) supplies a numeric identifier, a name, a default severity, a message format, and a warning group.
  The traditional format substitutes arguments numbered `$0` through `$9` and uses `$$` for a literal dollar sign.
  Descriptions can share a numeric identifier: the lexer has two descriptions for illegal characters with identifier `10000`.
  Settings keyed by identifier therefore affect both descriptions.
- A diagnostic's **severity** selects its displayed label and its treatment by the sink.
  In description-based reporting, `Disable` suppresses the message, severities from `Error` upward increment the sink's error count, and severities from `Fatal` upward request a compilation abort through Foundation's failure-reporting mechanism after output.
  A description's default severity is distinct from the effective severity after reporting settings have been applied.
  An error count is reporting state, separate from a caller's result code or a tool's exit status.
- A **warning group** (`WarningLevel`) gates warnings that have no explicit identifier override.
  Groups are independent: `Default` is always enabled, `Extra` is enabled initially, and `All` and `Pedantic` require enabling.
  Group names do not define an ordering of diagnostic severities.
- An **annotated diagnostic** (`GenericDiagnostic`) contains a formatted message, code, severity, primary span, secondary spans, and related notes that can have their own spans.
  A **diagnostic span** pairs a `SourceRange` with a label.
  The human-readable renderer uses the difference between the endpoints as a byte extent; equal endpoints allow a supplied highlighting callback to determine an extent.
  These presentation operations do not change Source Text's inclusive location-range membership rules or establish semantic relationships between the annotated program constructs.
- A **diagnostic sink** combines reporting settings, error counts, output buffering or a supplied writer, and optional forwarding to another sink.
  Its source manager interprets locations; the sink does not retain the manager, parent sink, or writer.
  An optional warning-state provider is retained through `RefPtr` and supplies location-dependent severity adjustments.
- A **diagnostic lookup** indexes borrowed descriptions by name and identifier, with aliases and spelling-convention conversion for name searches.
  Identifier lookup returns the first description added for that identifier.
  Descriptions, their strings, and stored alias spellings must remain valid while the lookup uses them; its arena can supply storage for added strings.

Message arguments passed through `DiagnosticArg` are borrowed for synchronous formatting.
Formatting an interned name uses its text, formatting a token uses its spelling, and formatting a token category uses its readable category description.
Those operations do not resolve declarations or decode literal values.
An annotated diagnostic owns its lists and holds retained message strings, but its locations still require the relevant source managers to remain alive without resetting them.

## Dependencies

- [Source Text](../source-text/source-text.md) supplies location interpretation, file text, line access, path information, and originating locations for token-paste presentation.
  It owns the files and views used during rendering.
- [Names](../names/names.md) and [Tokens](../tokens/tokens.md) supply spelling access, token locations, and readable token-category descriptions for argument formatting.
- Foundation's [Text](../../foundation/text/text.md) supplies strings, builders, slices, line scanning, Unicode decoding, numeric formatting, and spelling-convention conversion for lookup.
  The convention utility currently resides in `old-source/compiler-core/slang-name-convention-util.{h,cpp}`; Text's proposed ownership still requires ancestor coordination.
- [Blobs](../../foundation/blobs/blobs.md) supplies string-blob construction through the existing `StringUtil::createStringBlob` adaptation.
  The shared-class boundary for that adaptation remains recorded by Text and Blobs.
- [Memory](../../foundation/memory/memory.md), [Collections](../../foundation/collections/collections.md), and [Hashing](../../foundation/hashing/hashing.md) supply retained native objects, lookup arenas, diagnostic lists, indexes, and severity-override storage.
- [Output Writers](../../foundation/output-writers/output-writers.md) supplies output destinations and the append-buffer writer base used to adapt text output into a sink.
- [Failure Reporting](../../foundation/failure-reporting/failure-reporting.md) supplies assertions, invalid-format signals, the compilation-abort signal, and exception types used by the exception-reporting helpers.
- [Base Types](../../foundation/base-types/base-types.md) supplies scalar, count, and portability conventions.
- The product headers under `include/` supply writer and blob interfaces, result codes, and the severity, warning-group, presentation-format, and color constants.
  The host C++ environment supplies algorithms, ranges, numeric facilities, allocation, and the standard-string adapter used when signaling an abort.

Diagnostics does not invoke Lexing directly.
The optional `SourceLocationLexer` function pointer accepts a borrowed suffix of program text; rendering uses the returned slice's byte length immediately to extend a highlight.
Compiler clients commonly supply `Lexer::sourceLocationLexer`, which returns a slice into that input.
Supplying the function does not transfer its implementation or storage to the sink.

## Service Areas

Diagnostics has no child modules.
Its descriptions, reporting, and rendering share the same severity and location contracts, so this leaf keeps those services together.

### Descriptions and Lookup

The core catalog assembles the miscellaneous and lexer descriptions into a shared lookup.
The repeated-include catalog headers produce declarations, description objects, and the pointer lists registered with that lookup; they are C++ inputs rather than generated output files.
The miscellaneous messages include command-argument failures, a SPIR-V grammar failure, token-paste context, and internal-error context.
Owning those descriptions does not assign command parsing, grammar loading, or lexing to Diagnostics.

Other clients can supply their own descriptions and lookups.
[Compiler Diagnostics](../../compiler/compiler-diagnostics/compiler-diagnostics.md) constructs the compiler's lookup from generated compiler descriptions and core descriptions, skipping name conflicts and adding its compatibility alias.
Compiler owns its Lua catalogs, fiddle templates, generated diagnostic structures, and conversion of compiler values into annotated diagnostics.
The shared renderer receives the resulting strings and locations without inspecting AST or IR objects.

### Configurable Reporting and Routing

Description-based reporting first asks the optional warning-state provider to adjust a default severity at or below `Warning`.
The sink then applies an identifier override, or warning-group gating when no such override exists, and finally promotes remaining warnings when warnings-as-errors is set.
An override cannot lower a severity that has already reached `Error` or above.
Compiler owns option parsing and the preprocessor's warning timelines; the sink owns applying the supplied settings and provider contract.

Traditional reporting formats a description's arguments and location directly.
It instead constructs an annotated diagnostic when rich output or Visual Studio presentation is requested; typed compiler diagnostics already provide that representation.
Raw reporting accepts already formatted text and a severity, bypassing description-based filtering and formatting while still counting errors, forwarding output, and signaling fatal severities.
This path can carry downstream compiler output without interpreting that compiler's message syntax.

A sink writes to its borrowed writer when one is set and otherwise appends to its own output buffer.
The buffer can be returned as a retained string blob; a writer-backed sink does not collect the same output in that buffer.
Forwarded traditional messages retain the child sink's formatting, whereas a parent re-renders annotated diagnostics using its own settings and the originating sink's source manager.
The constructor taking another sink copies selected settings; forwarding is a separate operation selected by `setParentSink`.
Clients keep borrowed sinks, writers, managers, and callback code valid for the operations that use them.

`DiagnosticSinkWriter` adapts writes in the other direction, submitting text to a borrowed sink as raw `Note` output.
Foundation owns the general callback writer; Compiler's request layer retains that writer and installs the product diagnostic callback and borrowed user data.
Diagnostic callbacks receive formatted text through the writer rather than an owned annotated diagnostic.

Fatal reporting uses `SLANG_ABORT_COMPILATION`; it does not itself choose a product error result or tool exit code.
With exceptions enabled, Foundation throws `AbortCompilationException`; without them, its handler attempts a breakpoint and exits if execution continues.
The exception-reporting helpers translate caught exceptions into sink output and optional blobs; their catch-based recovery cannot be assumed in a build without exceptions.

### Rendering and Location Presentation

The traditional formatter provides a severity and code header, optional location and text lines, token-paste context, verbose actual-path output, and the existing language-server text convention.
The annotated renderer provides human-readable rich and Visual Studio layouts, plus a separate machine-readable TSV presentation.
The TSV entry point requires a supplied source manager, including for locationless messages.
The human-readable renderer organizes spans and notes into highlighted lines and uses actual file coordinates to retrieve text when displayed locations have been remapped.
Color, Unicode glyphs, and presentation format are rendering choices; automatic color checks the supplied writer's console status.
The layouts and highlighting calculations remain private implementation details of this leaf.

## Existing Implementation Mapping

Paths below are relative to `old-source/compiler-core/`; each `{h,cpp}` denotes both files.

- `slang-diagnostic-sink.h` belongs here except for the compiler-owned `IRInst` and `Modifier` forward declarations and their `printDiagnosticArg` declarations.
  The assigned declarations include descriptions and severities, warning groups, simple diagnostic records, borrowed argument adapters, sink settings and reporting, the warning-state provider interface, the sink writer, lookup storage, and exception-reporting helpers.
  Compiler already defines those two formatting extensions in `old-source/slang/slang-ir.cpp` and `slang-ast-modifier.cpp`; their declaration surface needs separation before migration.
- All of `slang-diagnostic-sink.cpp` belongs here, including basic argument and token-position formatting, traditional text presentation, configurable reporting and routing, lookup construction and searches, and exception-to-output adaptation.
- All of `slang-rich-diagnostics-render.{h,cpp}` belongs here: annotated diagnostic records, render options and entry points, private layout and glyph support, human-readable presentation, and TSV output.
- All of `slang-core-diagnostics.{h,cpp}`, `slang-misc-diagnostic-defs.h`, and `slang-lexer-diagnostic-defs.h` belongs here, including the core lookup assembly.

JSON's descriptions and catalog assembly in `slang-json-diagnostics.{h,cpp}` and `slang-json-diagnostic-defs.h` remain with [JSON](../json/json.md).
The compiler catalogs, generated declarations and implementations, compiler-value formatting, option-to-setting translation, and preprocessor warning-state implementation remain Compiler-owned.
Artifact diagnostic records and downstream-output parsing helpers in `slang-artifact-associated.*` and `slang-artifact-diagnostic-util.{h,cpp}` remain with [Artifacts](../artifacts/artifacts.md), separate from the sink's raw-text reporting.
Tool-specific catalogs under `tools/` remain with those clients.

The current `compiler-core` static target compiles the assigned implementation files and links the legacy Foundation `core` target.
Compiler variants and repository tools consume the shared declarations; the compiler's fiddle generation remains a separate build input of Compiler.
Diagnostics introduces no separate library or product export.
Any later migration must preserve existing consumers and behavior under the [root preservation contracts](../../slang.md#staged-migration-contracts); phase-one refinement does not establish migration readiness.
