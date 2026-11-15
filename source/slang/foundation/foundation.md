<!--
SPDX-FileCopyrightText: The Khronos Group, Inc.
SPDX-License-Identifier: CC-BY-4.0
-->

# Foundation

Foundation provides reusable data structures, memory and text services, and access to the host environment for the Slang implementation.
It also supplies binary storage, compression, persistent caching, communication framing, and performance measurement.

Foundation is the implementation concern currently supplied by the static `core` library.
Its children below are candidate ordinary modules within that artifact; they do not introduce additional libraries.
This decomposition is incomplete because several service dependencies are cyclic.
The [unresolved boundaries](#unresolved-boundaries) identify the cycles and the ownership decisions required before the affected services can be migrated.
[Compression](compression/compression.md), [Digests](digests/digests.md), [Hashing](hashing/hashing.md), and [Random Numbers](random-numbers/random-numbers.md) are implemented in this hierarchy and reexported by `foundation.h` in `slang::foundation`.
The other services still use their legacy implementations and headers; these ports do not settle their unresolved boundaries.

## Concepts

The [root document](../slang.md#concepts) defines implementation modules, build artifacts, visibility, and dependencies.
Foundation introduces the following implementation concepts:

- An **owned allocation** is storage whose lifetime a particular object or allocator controls.
  A **borrowed view** identifies existing elements or characters without extending the lifetime of their storage.
- An **intrusive reference** retains an object through a reference count stored in that object.
  Foundation provides intrusive smart pointers for C++ objects and implementations of the product API's COM-style reference-counted interfaces.
- A **blob** is a contiguous byte sequence exposed through the product `ISlangBlob` interface.
  Its implementation may own the bytes, retain another owner, or borrow storage without extending its lifetime.
- A **string slice** identifies a contiguous range of characters with explicit bounds.
  `UnownedStringSlice` borrows character storage; `StringSlice` retains its backing `StringRepresentation` through an intrusive reference.
  `String` manages its character storage through reference counting, and `OSString` owns a wide-character buffer.
  Character encodings describe how code units represent character values.
- A **callable wrapper** stores a function pointer, a member-function/object pair, or a client-supplied callable for later invocation.
  `Func` and `Action` retain their adapters.
  Their generic construction and assignment operations copy callable values when the callable type is deduced.
  An explicitly supplied lvalue-reference type selects an adapter that borrows its callable.
  Member-function adapters borrow their receiver.
- A **signal** in Foundation is a reported assertion failure, invalid operation, or other internal failure.
  It is separate from an operating-system signal and from an ordinary failure returned as a `SlangResult`.
- A **tool status code** identifies an outcome of a Slang tool invocation.
  Executable entry points and callers that invoke the corresponding tool in process or through the test server use the same vocabulary.
  A tool status code is separate from an entry point's `SlangResult` and from the operating system's representation of process termination.
- A **stream** is a cursor over bytes that supports some combination of reading, writing, seeking, and closing.
  A **file system** provides operations on named files and directories, through either the host operating system or another store.
- A **relocatable reference** identifies data by an offset relative to a containing buffer or to the reference itself, so the buffer can be relocated without rewriting ordinary pointers.
- An **archive** stores named files in one serialized byte sequence.
  RIFF chunks provide a tagged binary structure; a particular RIFF layout implements one of Foundation's archive formats.
- A **hash code** supports lookup of values in a collection.
  A **stable hash** has a deliberately fixed computation for reproducible keys; [Hashing](hashing/hashing.md#compatibility-and-input-representation) describes the existing input-representation limits.
  A **digest** summarizes a byte sequence using a named algorithm, such as the SHA1 keys used by the persistent cache.

These definitions describe implementation services, not additional supported product contracts.
The product interfaces and scalar types in `include/` retain their existing definitions.

## Dependencies

Foundation uses the following dependencies already available through [Slang Implementation](../slang.md#dependencies):

- Product headers supply scalar types, result codes, platform and export macros, blob and file-system interfaces, writers, shared-library interfaces, and the profiling interface.
  Implementing these interfaces does not require a dependency on the compiler implementation.
- The C++ runtime and host operating system supply allocation, atomics, synchronization, clocks, files, subprocesses, environment access, dynamic loading, and failure termination.
  The host-specific branches of these services remain implementation details of their owning modules.
- `unordered_dense` supplies the hashing machinery and hash-table implementation exposed by the existing hash and dictionary headers.
  The current `core` target makes this a public build dependency because clients instantiate those headers.
- Optional `mimalloc` supplies allocation through the existing allocator classes when enabled.
- `miniz` supplies DEFLATE compression and ZIP storage; LZ4 supplies LZ4 compression.
  These are private link dependencies of the current artifact.
- The build uses `Threads::Threads` and the platform dynamic-loader libraries for host services.
  Their exact link requirements follow the host configuration.

Foundation has no intended service dependency on its compiler, compiler-support, runtime, adapter, or tool siblings.
Those modules consume Foundation services.
The compiler-specific material presently compiled into `core` requires the explicit ownership decisions recorded below; its directory does not make compiler policy a Foundation responsibility.

## Candidate Decomposition

Every child in this list is public within Foundation: existing code outside `old-source/core/` uses the corresponding services.
Public visibility has the implementation-only meaning defined by the root document.
Platform branches and individual compression or archive implementations do not become separate child modules merely because their files are separate.
The children's own refinement determines their internal public and private organization.

| Child                                                       | Service                                                                                                |
| ----------------------------------------------------------- | ------------------------------------------------------------------------------------------------------ |
| [Base Types](base-types/base-types.md)                      | Scalar conventions, arithmetic, bit operations, type traits, and small C++ portability facilities.     |
| [Failure Reporting](failure-reporting/failure-reporting.md) | Assertions, internal failure signals, and their exception or termination behavior.                     |
| [Tool Status Codes](tool-status-codes/tool-status-codes.md) | Shared outcome vocabulary for Slang executable tools and their in-process and test-server invocations. |
| [Hashing](hashing/hashing.md)                               | Hash-code computation and combination, including stable hashes.                                        |
| [Memory](memory/memory.md)                                  | Allocation, arenas, allocation pools, and native C++ ownership.                                        |
| [Callables](callables/callables.md)                         | Stored function invocation and callable-object capture.                                                |
| [Collections](collections/collections.md)                   | Sequences, views, sets, dictionaries, and linked collections.                                          |
| [Text](text/text.md)                                        | Character storage and slices, encoding conversion, interning, formatting, and reusable text parsing.   |
| [Versions](versions/versions.md)                            | Version values, comparison, matching, and textual conversion.                                          |
| [Object Interfaces](object-interfaces/object-interfaces.md) | Reference-counted product objects and interface discovery.                                             |
| [Blobs](blobs/blobs.md)                                     | Byte buffers exposed through the product blob interface with implementation-specific storage lifetime. |
| [Command Options](command-options/command-options.md)       | Named option descriptions, lookup, and help-document formatting.                                       |
| [Host Services](host-services/host-services.md)             | Host files and paths, streams, subprocesses, command lines, environment access, and dynamic loading.   |
| [Output Writers](output-writers/output-writers.md)          | Output channels backed by callbacks, files, strings, or discarded output.                              |
| [File Systems](file-systems/file-systems.md)                | Host-backed, in-memory, cached, and relative views of named files.                                     |
| [Binary Layouts](binary-layouts/binary-layouts.md)          | Construction and access of relocatable binary buffers.                                                 |
| [Type Descriptions](type-descriptions/type-descriptions.md) | Native C++ data descriptions and operations driven by those descriptions.                              |
| [RIFF Data](riff-data/riff-data.md)                         | Reading, constructing, and writing tagged RIFF chunks.                                                 |
| [Digests](digests/digests.md)                               | Byte-digest computation and representation.                                                            |
| [Compression](compression/compression.md)                   | Byte-buffer compression and decompression.                                                             |
| [Archives](archives/archives.md)                            | Reading, writing, and recognizing file archives and exposing their file-system views.                  |
| [Persistent Cache](persistent-cache/persistent-cache.md)    | A disk cache mapping digests to blobs, with an optional entry-count limit.                             |
| [Packet Framing](packet-framing/packet-framing.md)          | Reading and writing length-delimited packets over supplied streams.                                    |
| [Profiling](profiling/profiling.md)                         | Recording function invocation counts and durations and presenting the results.                         |
| [Random Numbers](random-numbers/random-numbers.md)          | Seeded pseudorandom number sequences.                                                                  |

The principal service uses are as follows.
This list explains current relationships; the unresolved cycles prevent treating it as an accepted dependency graph.

- Tool Status Codes supplies the outcome vocabulary shared by Failure Reporting's no-exception termination branch and clients that execute tools.
  Result conversion, integer validation, decisions about comparing test output, and retry policy remain with clients rather than the vocabulary owner.
- Base types and hashing provide the scalar and key operations used by the other services.
  Memory uses hashing for smart-pointer keys; collections use memory for storage and hashing for lookup.
  Callables uses Memory to retain adapters containing the stored function or captured callable state.
- Text uses Memory for string ownership, Hashing for string keys, and Collections for encoding buffers and interned strings.
  Failure reporting stores text messages, while memory, collections, text, and object implementations invoke assertions or signals when their contracts fail.
  Versions uses Text to parse and format version identifiers, Hashing for version keys, and Collections to select among candidate versions.
- Object interfaces use native ownership to implement reference counting and interface discovery.
  Blobs use object interfaces, memory, collections, and text to expose byte storage.
  Text-to-blob conversions also use blob implementations.
- Host services use text, memory, collections, object interfaces, and blobs for paths, command-line arguments, streams, and library handles.
  Output writers use text and host services to format and deliver output; Text's hex-dump output also calls writer helpers, creating a further unresolved cycle.
  File systems adapt host operations and in-memory storage to the product file-system interfaces.
- Binary layouts use memory, collections, streams, and blobs to compose and transfer relocatable buffers.
  Type descriptions use memory, collections, and text for native layout metadata and operations on described values.
  RIFF data uses memory, linked collections, streams, and blobs to parse, construct, and write chunk hierarchies.
  Digests use blobs and text to expose computed byte summaries.
  Compression operates on buffers and blobs.
  Archives combine binary structure, compression, and file-system views.
- The persistent cache uses byte digests, blobs, host files, and synchronization to store and evict entries.
  Packet framing uses streams, text header parsing, and process timing; the protocol layered on those packets belongs to its caller.
- Command options use text and collections to describe options and format help.
  Profiling uses clocks, collections, text, and reference-counted product objects.
  The `RandomGenerator` family uses Memory and Failure Reporting; the separate `Random` class uses neither service.

## Existing Implementation Mapping

Paths below refer to `old-source/core/` unless another directory is stated.
A basename with `{h,cpp}` denotes both existing files; a name ending in `.h` identifies that header alone.
These assignments cover the directory, including the partial-file and deferred assignments following the list.
They propose service ownership and do not authorize moving a file wholesale.

- **Base Types:** `slang-type-traits.h`, `slang-range.h`, `slang-secure-crt.h`, `slang-byte-encode-util.{h,cpp}`, and the arithmetic, bit, and numeric-conversion facilities in `slang-math.h`.
  The separate seeded `Random` class formerly in the math header now resides in Random Numbers.
  The scalar aliases, character-code-unit types, move/swap/bit helpers, deferred cleanup utility, componentwise equality macros, compiler compatibility and warning macros, unused-value macros, and runtime export declarations in `slang-common.h` also belong here.
- **Failure Reporting:** `slang-signal.{h,cpp}` and `slang-exception.h`, together with the assertion and assumption macros in `slang-common.h`.
- **Tool Status Codes:** only the `ToolReturnCode` enum in `slang-test-tool-util.h`.
  It distinguishes successful execution, ordinary tool or compilation failure, an internal compiler error, an ignored invocation, and an invocation that could not run.
  The declaration retains its canonical `Slang` identity and existing enumerator names and values.
- **Hashing:** `hashing/hashing.h` in this directory owns the complete ordinary and stable header-only implementation.
  The two legacy headers have been removed, and their include clients use the new primary header.
  All definitions retain their canonical `Slang` identities; `slang::foundation::hashing` and `foundation.h` explicitly reexport the same eighteen names.
  The original math/product prerequisite includes remain available while their owners are unported.
- **Memory:** `slang-allocator.h`, `slang-smart-pointer.h`, `slang-memory-arena.{h,cpp}`, `slang-free-list.{h,cpp}`, `slang-virtual-object-pool.h`, and the generic `ScopedAllocation` class in `slang-blob.h`.
- **Callables:** `slang-func-ptr.h`, which stores and invokes functions, member-function/object pairs, and copied callable objects.
- **Collections:** `slang-array.h`, `slang-array-view.h`, `slang-list.h`, `slang-short-list.h`, `slang-chunked-list.h`, `slang-linked-list.h`, `slang-internally-linked-list.{h,cpp}`, `slang-dictionary.h`, `slang-short-dictionary.h`, and `slang-uint-set.{h,cpp}`.
- **Text:** `slang-string.{h,cpp}`, `slang-char-util.{h,cpp}`, `slang-char-encode.{h,cpp}`, `slang-string-util.{h,cpp}`, `slang-string-escape-util.{h,cpp}`, `slang-string-slice-pool.{h,cpp}`, `slang-string-slice-index-map.{h,cpp}`, `slang-name-value.{h,cpp}`, `slang-token-reader.{h,cpp}`, and `slang-hex-dump-util.{h,cpp}`.
  The miscellaneous token reader parses reusable textual notation; it is separate from the compiler's lexer and token model.
- **Versions:** `slang-semantic-version.{h,cpp}`, including version ordering, compatibility tests, and matching selection as well as parsing and formatting.
- **Object Interfaces:** `slang-com-object.h` and `slang-castable.{h,cpp}`, plus the `ICastable`/`IClonable` aliases and `clone` utility currently in `slang-common.h`.
- **Blobs:** `slang-blob.{h,cpp}`, except the `ScopedAllocation` class assigned to Memory.
  The blob-facing `StringUtil` operations in `slang-string-util.{h,cpp}` cross the text/blob boundary discussed below.
- **Command Options:** `slang-command-options.{h,cpp}` and `slang-command-options-writer.{h,cpp}`.
  These hold generic descriptions and help formatting; compiler-specific option tables remain with the compiler concern.
- **Host Services:** `slang-io.{h,cpp}`, `slang-stream.{h,cpp}`, `slang-text-io.{h,cpp}`, `slang-command-line.{h,cpp}`, `slang-process.h`, `slang-process-util.{h,cpp}`, `windows/slang-win-process.cpp`, `unix/slang-unix-process.cpp`, `slang-platform.{h,cpp}`, and `slang-shared-library.{h,cpp}`.
  The generic `MemoryReader` at the end of `slang-riff.h` also belongs here as in-memory byte I/O; it reads and skips bytes without interpreting RIFF structure.
  Despite its name, `slang-platform.h` principally declares host queries and dynamic-library operations; the basic platform-selection macros already come from the product headers.
- **Output Writers:** `slang-writer.{h,cpp}` and `slang-std-writers.{h,cpp}`.
- **File Systems:** `slang-file-system.{h,cpp}`, `slang-memory-file-system.{h,cpp}`, and `slang-implicit-directory-collector.{h,cpp}`.
- **Binary Layouts:** `slang-relative-ptr.{h,cpp}`, `slang-offset-container.{h,cpp}`, and `slang-blob-builder.{h,cpp}`.
- **Type Descriptions:** `slang-rtti-info.{h,cpp}` and `slang-rtti-util.{h,cpp}`.
  These descriptions and operations concern native C++ values used by JSON/native conversion and RPC; they are separate from shader reflection and the compiler's AST type system.
- **RIFF Data:** `slang-riff.{h,cpp}`, except the generic `MemoryReader` assigned to Host Services.
- **Digests:** `digests/digests.{h,cpp}` in this directory owns digest values, binary/hexadecimal conversion, SHA1 computation, and the value append adapters.
  The legacy crypto pair has been removed, and its include clients use the new primary header.
  The definitions retain their canonical `Slang` identities; `slang::foundation::digests` and `foundation.h` reexport the same four declarations.
  Digests temporarily uses the unchanged legacy text, blob, and collection types while those services remain unported.
- **Compression:** `compression/compression.{h,cpp}` in this directory now owns the complete style descriptor, encoding identifier, shared buffer interface, and both codec implementations.
  The five legacy Compression files have been removed, and clients use the new primary header.
  `foundation.h` explicitly reexports the five public names from `slang::foundation::compression`.
  Compression temporarily uses the unchanged legacy native ownership and blob types while their services remain unported.
- **Archives:** `slang-archive-file-system.{h,cpp}`, `slang-zip-file-system.{h,cpp}`, and `slang-riff-file-system.{h,cpp}`.
  The shared archive interface and format-dispatch factories remain one assigned service until Archives refines their internal organization.
- **Persistent Cache:** `slang-persistent-cache.{h,cpp}`.
- **Packet Framing:** `slang-http.{h,cpp}`.
  `HTTPPacketConnection` supplies HTTP-style headers and content framing over an existing stream, rather than opening network connections.
  For example, compiler-support's `slang-json-rpc-connection.cpp` uses the framing beneath JSON RPC.
- **Profiling:** `slang-performance-profiler.{h,cpp}`, including its adapter to the product `ISlangProfiler` interface.
- **Random Numbers:** `random-numbers/random-numbers.{h,cpp}` in this directory now owns the complete generator family and the separate seeded `Random` class formerly at the end of `slang-math.h`.
  The transferred legacy generator files have been removed, and `slang-math.h` retains only the arithmetic and numeric-conversion services assigned to Base Types.
  `foundation.h` explicitly reexports the four public generator names from `slang::foundation::random_numbers`.
  Random Numbers temporarily includes `old-source/core/slang-smart-pointer.h` and uses the unchanged `Slang::RefObject` and `Slang::RefPtr` ownership contract while Memory remains unported.

`slang-basic.h` is an aggregate header and the current precompiled-header input.
It assembles services assigned to several children and therefore belongs to Foundation's integration surface, rather than defining another service module.
`core.natvis` and `core_lldb.py` describe several children for debuggers and remain Foundation integration support until their type-specific portions can follow the corresponding implementations.
`CMakeLists.txt` builds the current artifact and retains the build-dependency and precompiled-header configuration.

The following files require ancestor ownership decisions rather than an invented Foundation service:

- `slang-type-text-util.{h,cpp}` and `slang-type-convert-util.{h,cpp}` describe compiler targets, source languages, and compiler settings.
  Compiler support is a candidate owner for the shared mappings used by both `old-source/compiler-core/` and `old-source/slang/`.
  The archive-name and file-system-mode entries within `TypeTextUtil` also need separation from that compiler policy; the generic `NameValueUtil` machinery is already assigned to Text.
- `slang-builtin-module-cache.{h,cpp}` reads and writes timestamp-prefixed serialized built-in language modules.
  Its callers in `old-source/slang/slang-api.cpp` and `tools/slang-core-module-cache/` make the compiler's bundled-library preparation/loading concern a candidate owner.
  It is distinct from Foundation's generic digest-to-blob persistent cache.
- `slang-render-api-util.{h,cpp}` holds rendering API names, flags, and availability policy used by `tools/gfx/`, `tools/render-test/`, and `tools/slang-test/`.
  The root must determine ownership alongside those existing tools, whose implementations are outside this trial's initial `old-source/` mapping.
- Apart from the `ToolReturnCode` enum assigned above, `slang-test-tool-util.{h,cpp}` supplies `ToolReturnCodeSpan`, the `TestToolUtil` result/integer converters and `isDone` predicate, entry-point contracts, executable layout, compiler-session prelude setup, command-line compilation modes, and test selection.
  Those tool-integration and test-policy services remain outside Tool Status Codes and require ancestor ownership decisions.
  Generic path operations may use Host Services without making the entire utility a host service.

## Unresolved Boundaries

The candidate children express service concerns, but the following observations prevent claiming an acyclic decomposition.
Splitting an include file or relocating a definition does not by itself remove a dependency on the service that definition provides.

### Failure Reporting and Basic Data Services

`slang-common.h` includes `slang-signal.h`, and the signal header includes the common header.
Separating scalar declarations from failure declarations can remove that header cycle.
The harder cycle is behavioral: `RefObject::releaseReference`, list accessors, and string operations use assertions, while `handleSignal` constructs and retains a `String` and throws string-bearing exceptions when exceptions are enabled.
`String` also uses native reference counting, hashing, and encoding buffers implemented with `List`.
`Array` and `Dictionary` include the exception header, but their implementations reach failure reporting through assertions and signals; those includes alone do not establish an additional service dependency.
These are real service dependencies even after replacing aggregate includes with narrower headers.

Before these children are accepted for migration, their refinement must identify a behavior-preserving separation or a coherent service boundary that owns the mutual dependency.
Grouping every scalar, failure, ownership, collection, and text operation into a generic data module would conceal the problem rather than explain a common responsibility.
No replacement failure mechanism or change to exception behavior is selected here.

The no-exception branch of `handleSignal` uses `ToolReturnCode::InternalCompilerError` from [Tool Status Codes](tool-status-codes/tool-status-codes.md).
The dependency on that vocabulary requires neither compiler-session setup nor the test framework.
Assigning the shared enum does not remove the independent failure/text/native-ownership cycle described above.

### Text Adaptations and Aggregate Headers

`StringUtil::getSlice` and `getString` read blobs through the product interface.
`StringUtil::createStringBlob` constructs the concrete `StringBlob` implementation, which in turn uses Text's string representation and Memory's native ownership.
Text refinement must determine where these adaptation operations belong without disguising the cycle as a file move.
Text also owns `HexDumpUtil::dumpWithMarkers` in `slang-hex-dump-util.cpp`, which calls Output Writers' out-of-line `WriterHelper::print` and `put` operations.
`WriterHelper::print` in `slang-writer.cpp` formats through Text's `StringUtil::calcFormattedSize` and `calcFormatted`, so text/output adaptation ownership is another unresolved service cycle.
The `String` wide-character conversions use `CharEncoding`; the `slang-text-io.h` include in `slang-string.cpp` does not establish a dependency on host files or stream-based text I/O.
Likewise, the output-writer include in `slang-riff.h` has no corresponding writer use in the RIFF implementation.
These aggregate or excess includes require examination during header extraction, rather than additional architectural edges.

### Host Operations and Archive Views

Host Services deliberately groups related operating-system operations for this first refinement.
`FileStream::_init` queries `File::exists` and `Path::getPathType`, while whole-file read/write operations construct file streams and text adapters.
`BufferedReadStream::readUntilContains` calls `Process::sleepCurrentThread`, and process utilities read from streams.
`Process::create` consumes a `CommandLine`, while command-line construction queries process escaping and executable suffixes.
These current peer dependencies must remain visible if Host Services later proposes narrower children.

File-system views depend on host operations, and archive file-system implementations depend on the ordinary file-system views.
RIFF binary structure belongs to RIFF Data, below the archive views; ZIP only needs RIFF's `FourCC` declaration rather than the RIFF tree implementation.
Archives refinement must retain one explicit owner for archive format selection and the shared interface so its internal implementation graph does not become cyclic.

### Build Consumers and Ancestor Decisions

`old-source/slang-rt/CMakeLists.txt` also compiles the core implementations using runtime export settings, rather than linking the `core` archive.
Every transferred implementation must therefore preserve both the static-core consumer and this separately compiled runtime consumer.
Moving files into candidate ordinary modules does not change that requirement or establish new installed libraries.
For Compression, Digests, and Random Numbers, this directory's CMake entries add each leaf implementation to both existing targets, conditionally including `slang-rt` when runtime is enabled.
Hashing lists its one header on both targets without adding a translation unit.
Root CMake gives the migrated Foundation directory a separate binary directory so the compiler's existing `source/slang` generated-file paths remain available.
The old targets retain their names and export settings; these unannotated Compression and generator declarations and their implementation headers are not installed product API.

The ancestor must settle the compiler-specific and tool-specific assignments listed above.
Foundation remains pending while those ownership questions and the low-level service cycles are unresolved.
Its investigation does not settle the separate bootstrap, compiler, and loadable-wrapper artifact questions recorded at the root.
