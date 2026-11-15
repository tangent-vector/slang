<!--
SPDX-FileCopyrightText: The Khronos Group, Inc.
SPDX-License-Identifier: CC-BY-4.0
-->

# Output Writers

Output Writers delivers counted character ranges to callbacks, C files, string builders, or a destination that discards them.
It also provides formatted output, reusable writer implementation bases, and a context associating writers with standard output channels.
Clients choose the messages, output formats, and channel routing.

The implementation remains in `old-source/core/`.
This document refines [Foundation's assignment](../foundation.md#existing-implementation-mapping); the [Text cycle and ownership questions](#unresolved-boundaries) remain constraints on migration.
The product writer interface in `include/` remains an existing external contract.

## Concepts

[Foundation](../foundation.md#concepts) defines borrowed views, intrusive references, string slices, and byte streams.
[Object Interfaces](../object-interfaces/object-interfaces.md#concepts) distinguishes product interface references from native ownership.

- An **output writer** implements the product `ISlangWriter` contract for writing counted character ranges, obtaining a writable append buffer, flushing, reporting console status, and requesting a text or binary mode.
  The count measures bytes in the existing narrow-character implementations; the writer implementations do not validate a character encoding.
- An **append buffer** is writable storage obtained from a writer for a subsequent `endAppendBuffer` call that submits the written range.
  The product interface allows only one active append buffer and requires the returned pointer and a count within the reserved range when submitting it.
  The concrete providers implement that protocol differently, as described below.
- A **writer channel** identifies the diagnostic, standard-output, or standard-error slot in a `StdWriters` context, using the existing product `SlangWriterChannel` vocabulary.
  A channel is a selection of a writer, rather than another destination implementation.
- A **standard writer context** retains its installed writers and exposes an optional borrowed debug-message callback.
  `StdWriters` also provides a static singleton pointer for clients using implicit channel access.
- A **debug-message callback** receives a message with its existing `DebugMessageType` and `DebugMessageSource` classifications.
  This is the `IDebugCallback` contract stored by `StdWriters`, separate from the product `SlangDiagnosticCallback` adapted by `CallbackWriter`.

An output writer differs from Host Services' [byte-stream and stream-text abstractions](../host-services/host-services.md#named-files-and-stream-transfers).
`FileWriter` wraps a C `FILE*` directly; it does not wrap `Stream`, `TextWriter`, or `StreamWriter`.
Writing through a supplied writer does not assign the client's diagnostic formatting, compiler-output interpretation, or graphics-debug policy to Output Writers.

## Dependencies

- [Native Ownership](../memory/native-ownership/native-ownership.md) supplies `RefObject`, its reference operations, and `RefPtr` retention for writer objects and contexts.
  `BaseWriter` connects product references to that native counter using the product helper macros; it does not use Object Interfaces' `ComObject` or `ComBaseObject` implementations.
- [Collections](../collections/collections.md) supplies the resizable character buffer used by the append-buffer implementation and callback termination adapter.
- [Strings](../text/strings/strings.md) supplies borrowed character slices and the builder storage used by `StringWriter`.
  [Text Operations](../text/text-operations/text-operations.md) supplies variadic formatted-size calculation and formatting for `WriterHelper::print`.
  These are actual implementation calls, including the reverse dependency described below.
- [Failure Reporting](../failure-reporting/failure-reporting.md) supplies the append-buffer pointer and extent assertion; native-reference release and the collection and string implementations also invoke their own checks.
- [Scalar Types](../base-types/scalar-types/scalar-types.md) supplies count and index conventions, and [C++ Utilities](../base-types/cpp-utilities/cpp-utilities.md) supplies the printf-format annotation.
  [C Runtime Compatibility](../base-types/c-runtime-compatibility/c-runtime-compatibility.md) supplies the existing `fopen_s` adaptation on configurations requiring it.
- The product headers supply `ISlangWriter`, its UUID and method order, channels, modes, diagnostic callbacks, result codes, calling conventions, inline and unused-value macros, `ComPtr`, and the native-reference interface macros.
  These contracts do not require the compiler implementation or assign the public headers to this module.
- The host C and C++ environment supplies C file opening, counted writes, flushing and closing, terminal detection, variadic argument handling, byte copying, allocation, and the append-buffer mutex.
  Windows additionally supplies file-descriptor binary mode and UTF-8 console setup.

`slang-platform.h` supplies the existing non-MS C-runtime name aliases used for terminal detection.
Their physical placement requires [coordination with Host Services and C Runtime Compatibility](#platform-compatibility-ownership); the include does not establish a dependency on host paths, byte streams, process invocation, dynamic loading, or `PlatformUtil` queries.
The writer include in `slang-riff.h` has no corresponding writer use in the RIFF implementation and does not make Output Writers a dependency of RIFF Data.

## Service Areas

Output Writers is a proposed public leaf within Foundation, with no child modules.
The providers share one product protocol and implementation bases, and the channel context retains those providers without selecting the clients' output policy.
We keep the complete existing classes together, including the context's debug callback, rather than treating separate methods or platform branches as independently movable modules.

### Output Convenience and Implementation Bases

`WriterHelper` borrows an `ISlangWriter*` without retaining it.
Its output operations require a non-null writer kept alive by the caller.
Its `write` and `flush` operations forward directly; `put` submits either a zero-terminated C string's prefix or an explicitly bounded slice.
`print` uses Text's variadic formatter, reserves one additional byte for zero termination, and submits only the formatted payload.
An empty formatted result returns success without obtaining an append buffer.
The helper reports the writer's submission result and provides no separate formatting-error result.

`BaseWriter` implements discovery of `ISlangUnknown` and `ISlangWriter`, native-counter-backed product references, flag-based console status, a no-op flush, and an unsupported-mode result.
A successful interface query adds a reference; an unsupported UUID returns `SLANG_E_NO_INTERFACE` without clearing arbitrary output storage.
Ordinary product release uses `releaseReference`, whereas `WriterFlag::IsStatic` selects decrement without deletion.
The flag changes product `release`; native `RefPtr` release continues to use the native ownership operations.
For example, Compiler Support's `DiagnosticSinkWriter` uses the append-buffer base with `WriterFlag::IsStatic` and supplies its own write operation into a diagnostic sink.
That diagnostic interpretation remains with [Diagnostics](../../compiler-support/diagnostics/diagnostics.md).

`AppendBufferWriter` obtains a mutex before resizing its `List<char>` and returns that buffer.
Its end operation asserts the pointer and range, calls the concrete write operation, clears the logical buffer, unlocks, and returns the write result.
Direct write calls do not acquire that mutex.
The paired base operations are not a general thread-safety guarantee for every provider or operation.

### Callback, File, String, and Discard Destinations

`CallbackWriter` borrows its callback and user-data pointer.
For a nonempty write, it adds a zero terminator in its append storage or copies the supplied range there before invoking the callback synchronously.
Zero-byte writes do not invoke the callback, and writes return success because the callback has no result channel.
The callback receives a C string without a separate byte count, so an embedded zero byte can end the text visible to the callback before the submitted range ends.
The client keeps the callback and user data valid while they can be invoked.
The message pointer does not transfer storage ownership and can be invalidated by later writer operations.

`FileWriter` can wrap an existing C file or open a named file using caller-supplied C open options; its text and binary factories select `w` and `wb`.
An opened file is owned: the factory clears `IsUnowned`, returns `SLANG_E_CANNOT_OPEN` on opening failure, and replaces the output writer only after opening succeeds.
A directly wrapped file is closed on destruction unless `IsUnowned` is set.
Destruction flushes even an unowned file, and `AutoFlush` requests a flush after each write.
Write success means `fwrite` accepted the complete byte count; flush and close errors have no separate result channel.

The file constructor combines caller flags with detected terminal status, so callers can also assert console status through `IsConsole`.
On `_WIN32`, requesting binary mode calls `_setmode` and returns success without checking its result.
Other mode requests, including text mode, and the non-Windows branch return failure.
Those existing differences are not a portable guarantee that every requested mode was applied.

`StringWriter` borrows a `StringBuilder*` and uses Text's prepared append operations directly rather than the intermediate `List<char>`.
Submitting the prepared range extends the builder, and a direct nonempty write appends the counted bytes.
The caller must keep the builder alive and respect its storage and mutation contracts.
`NullWriter` discards direct writes and returns success; its inherited append path still allocates and submits a temporary buffer.
The file ownership and auto-flush flags do not give the string, callback, or discard providers corresponding ownership or flush behavior.

### Standard Channels and Default Setup

`StdWriters` retains each installed `ISlangWriter` through a `ComPtr` and returns borrowed pointers from its accessors.
Channel selection assumes a valid channel index, and an unset channel remains null.
`flushWriters` visits all non-null slots; installing the same writer in several slots can therefore flush it several times.

`createDefault` constructs auto-flushing, unowned wrappers for `stderr` and `stdout` and leaves the diagnostic channel unset.
On Windows-family configurations it also requests UTF-8 input and output console code pages without reporting those calls' results.
The context does not own the C standard files.
The static singleton pointer and the debug callback pointer are borrowed; setting either pointer does not retain its referent or synchronize access.
`initDefaultSingleton` returns a retained context, reusing an existing singleton or creating and installing defaults.
The caller keeps that context alive; the singleton pointer itself does not keep the context alive or clear itself on destruction.
Implicit channel access additionally requires an installed context and a non-null selected writer.

For example, the test runner supplies string-backed output and error slots to a loaded tool and restores its previous singleton after the invocation.
The render-test bridge separately borrows the context's debug callback for the active test.
Compiler requests have their own routing: `EndToEndCompileRequest::setWriter` selects static default providers and distinguishes a null diagnostic sink writer from its discard destination.
Neither the request's diagnostic policy nor the test bridge's callback lifetime mechanism belongs to the generic context.

The assigned `slang-std-writers.h` includes the borrowed debug-message callback slot alongside the retained writer channels.
The test runner installs an error-capturing callback, and render-test adapts RHI classifications to the existing `Slang` classifications before forwarding a message.
Foundation retains this complete context and its debug-message vocabulary in Output Writers.
The context stores channels and a callback pointer without calling compiler or rendering implementations; clients supply their messages, callback implementations, and lifetime management.
Migration preserves the complete declarations and their existing borrowed and retained lifetime rules.

## Existing Implementation Mapping

Paths below are relative to `old-source/core/`.
The four rows cover the complete assigned files, including inline operations, protected fields, aliases, local support, and platform branches.
No partial reassignment or code movement is selected.

| Existing Implementation | Output Writers Responsibility                                                                                                                                                                   |
| ----------------------- | ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| `slang-writer.h`        | Complete `WriterHelper`, `WriterFlag`, `WriterFlags`, `BaseWriter`, `AppendBufferWriter`, `CallbackWriter`, `FileWriter`, `StringWriter`, and `NullWriter` declarations and inline definitions. |
| `slang-writer.cpp`      | Formatting and forwarding, interface lookup, append storage and locking, callback termination, file creation and lifetime, terminal and mode handling, and builder adaptation.                  |
| `slang-std-writers.h`   | Complete `DebugMessageType`, `DebugMessageSource`, `IDebugCallback`, and `StdWriters`, including channel retention, borrowed callback and singleton access, and inline flushing.                |
| `slang-std-writers.cpp` | Singleton definition, default C-file providers, Windows console setup, and retained default-context initialization.                                                                             |

`ISlangWriter`, `SlangWriterChannel`, `SlangWriterMode`, and `SlangDiagnosticCallback` in `include/slang.h`, and the helpers in `include/slang-com-helper.h` and `include/slang-com-ptr.h`, remain product contracts.
`HexDumpUtil` and Text's formatters retain [Text's assignments](../text/text.md#existing-implementation-mapping).
The non-MS aliases at the end of `slang-platform.h` retain [Host Services' current mapping](../host-services/host-services.md#existing-implementation-mapping), subject to the coordination below.
Concrete diagnostic-sink providers, compiler request routing, generator output construction, and test/render callback bridges remain with their respective clients.

The current source glob compiles both assigned `.cpp` files into static `core` and separately into enabled `slang-rt` under runtime export settings.
Generators, compiler clients, and tools link the existing Foundation implementation; this assignment introduces no installed writer library.
The loaded `render-test-tool` also receives a `Slang::StdWriters*` through the exported `innerMain` entry point described by `TestToolUtil::InnerMainFunc`.
Its inline context access and callback dispatch make class layout and contract identity relevant across that binary boundary, even though these internal classes have no writer-specific product export annotations.
Statically linked copies of Foundation in different artifacts do not establish one shared singleton instance across those artifacts; tool entry points install the supplied context for their own implicit access.
The [root's staged migration contracts](../../slang.md#staged-migration-contracts) require preserving both compilation consumers and auditing canonical identities, layouts, names, signatures, method order, and annotations before any namespace or header transfer.

## Unresolved Boundaries

### Text Formatting and Hexadecimal Delivery

`HexDumpUtil::dumpWithMarkers` calls the out-of-line `WriterHelper::print` and `put` operations, and `WriterHelper::print` calls `StringUtil::calcFormattedSize` and `calcFormatted`.
`StringWriter` additionally uses Text's builder representation.
The resulting Text to Output Writers to Text cycle uses actual implementations, rather than only the product writer interface.
The [Text refinement](../text/text.md#hexadecimal-output-and-writers) also records that hexadecimal parsing and delivery share the complete `HexDumpUtil` class.
Foundation must settle the adaptation boundary while preserving those existing classes, signatures, and behavior; moving only a method definition or replacing an include does not settle the dependency.
No replacement writer API, formatter, or callback protocol is selected here.
The low-level ownership, text, collection, and failure [cycles recorded by Foundation](../foundation.md#failure-reporting-and-basic-data-services) also remain applicable.

### Platform Compatibility Ownership

The non-MS `_fileno` and `_isatty` aliases currently reside with the other C-runtime aliases at the end of `slang-platform.h`.
Host Services records their present assignment and the need to coordinate with Base Types' C Runtime Compatibility during header extraction.
Output Writers uses those aliases for direct C terminal detection and requires neither the platform query implementation nor the host stream implementation.
Foundation must settle the aliases' physical and semantic ownership before treating the proposed service graph as final.

### Existing Append-Protocol Limits

The product interface says an empty append need not be submitted, but `AppendBufferWriter::beginAppendBuffer` acquires a mutex that only its end operation releases.
`CallbackWriter::beginAppendBuffer` instead resizes without acquiring that mutex, while its inherited end operation still unlocks it.
`StringWriter` uses neither base operation and relies on the builder's append contract.
These existing implementation differences prevent a common cancellation or synchronization guarantee beyond what each provider actually implements.
They require explicit preservation and deferred issue notes during phase two, rather than a behavior repair folded into a structural port.
