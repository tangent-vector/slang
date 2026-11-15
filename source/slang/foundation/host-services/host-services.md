<!--
SPDX-FileCopyrightText: The Khronos Group, Inc.
SPDX-License-Identifier: CC-BY-4.0
-->

# Host Services

Host Services provides access to named host files, byte and text I/O, subprocess invocation, loaded native libraries, and information about the executing host.
Clients decide which files, programs, or libraries they need and interpret their contents and results.
The implementation remains in `old-source/`; the children below refine [Foundation's assignment](../foundation.md#existing-implementation-mapping) without changing its existing services or establishing new libraries.
The [unresolved boundaries](#unresolved-boundaries) prevent treating these children as an accepted dependency graph or a code migration plan.

## Concepts

[Foundation](../foundation.md#concepts) defines streams, owned allocations, borrowed views, intrusive references, and string slices.
Host Services uses the following additional distinctions:

- A **host path** names a file or directory using the host's path conventions.
  Manipulating the spelling of a path is distinct from asking the host about the named object.
  The existing `Path` class provides both kinds of operation.
- An **executable location** records either a path to a program or a name to search for, together with an unspecified state.
  Equality compares that recorded choice and spelling rather than the identity of a resolved executable.
  A **command line** combines an executable location with an ordered sequence of unescaped argument strings.
- A **process termination result** distinguishes a program's return value from an operating-system signal that killed it.
  `Process` exposes these separately; neither is Foundation's [tool status code](../tool-status-codes/tool-status-codes.md#concepts) or the `SlangResult` describing whether a host operation succeeded.
- A **native library handle** is a platform loader reference obtained by loading a native library.
  Symbol addresses obtained through that reference do not retain the library.
  The reference-counted library wrappers control the loader reference's lifetime; selected libraries remain mapped even after that reference is released.
- A **file lock** coordinates access through an opened host file using shared or exclusive locking.
  It is separate from a C++ mutex and from the lifetime of the file's pathname.

The existing `URI` type stores a URI spelling and supplies protocol extraction and local-file path conversion.
Those operations do not implement a general URI parser or assign editor-protocol semantics to Host Services.
`PlatformKind` and `PlatformFamily` describe the implementation's existing platform taxonomy; an enumerator does not establish that every host service is implemented for that platform.
`PlatformUtil::getPlatformKind` reports the build-selected kind rather than detecting the platform at runtime.

## Dependencies

- [Scalar Types](../base-types/scalar-types/scalar-types.md) supplies byte, character-code-unit, count, index, and offset types.
  [C++ Utilities](../base-types/cpp-utilities/cpp-utilities.md) supplies annotations and unused-value helpers, [Numeric Operations](../base-types/numeric-operations/numeric-operations.md) supplies the minimum operation used during path composition, and [C Runtime Compatibility](../base-types/c-runtime-compatibility/c-runtime-compatibility.md) supplies bounded C file and byte operations.
- [Memory](../memory/memory.md) supplies intrusive ownership of stream and process objects and the `ScopedAllocation` destination used by whole-file reads.
  [Collections](../collections/collections.md) supplies owned byte buffers, argument sequences, and temporary path components.
- [Text](../text/text.md) supplies path and argument storage, bounded character views, numeric and line conversion, encoding detection and conversion, and the escaping handlers used when rendering command lines.
  Host Services determines when to apply those operations to a file, stream, or invocation; Text owns their character-level contracts.
- [Hashing](../hashing/hashing.md) supplies the hash-code result type exposed by `URI::getHashCode`.
  That adapter delegates character-content hashing to Text's string operation.
- [Object Interfaces](../object-interfaces/object-interfaces.md) supplies the reference-counted implementation base and interface-discovery conventions used by native-library wrappers.
  Product headers in `include/` supply their shared-library and loader interfaces, result codes, path categories, platform selection, and export declarations.
  The product support headers also supply `Guid`, UUID comparison, result propagation macros, and `ComPtr` retention.
- [Failure Reporting](../failure-reporting/failure-reporting.md) supplies assertions and assumptions used for buffer, access, seek, and handle preconditions.
  Ordinary host failures continue to use the existing result, boolean, empty-string, or null-symbol conventions of each operation.
- The host C and C++ environment supplies file and directory operations, C file streams, `std::filesystem` relative paths, thread creation for simultaneous stream transfer, process creation and waiting, environment access, timing, debug output, and native loading.
  The current artifact links `Threads::Threads` and `${CMAKE_DL_LIBS}` where the host configuration requires them.

Several includes collect declarations owned elsewhere without adding a service dependency.
In particular, `slang-blob.h` supplies Memory's `ScopedAllocation` to `slang-io.h`; the assigned host implementation does not create or consume a blob interface.
The `MemoryReader` currently declared in `slang-riff.h` consumes a borrowed pointer and byte count without interpreting RIFF structure.
Neither the `slang-basic.h` aggregate nor `slang-shared-library.h`'s dictionary include establishes use of every service in those headers.

## Decomposition

All three children are public ordinary modules within Foundation.
Compiler, Compiler Support, Runtime, adapters, and existing repository tools use the services directly.
Public visibility has the implementation-only meaning defined by the [root document](../../slang.md#concepts).

| Child                                                    | Service                                                                                                                                         |
| -------------------------------------------------------- | ----------------------------------------------------------------------------------------------------------------------------------------------- |
| [Host I/O](host-io/host-io.md)                           | Addressing and accessing host files, transferring bytes and text through files and memory, and invoking and communicating with child processes. |
| [Shared Libraries](shared-libraries/shared-libraries.md) | Native library naming, loading, symbol lookup, retained loader references and scopes, and library-file information.                             |
| [Host Environment](host-environment/host-environment.md) | Platform classification, environment-variable reads, executable-directory lookup, host error text, and host debug output.                       |

The platform implementations and their local handle and pipe classes remain implementation details of the relevant child.
They are not public Windows and Unix modules.
Host I/O groups file access, streams, and invocation because their implementations call each other in both directions.
The common task is transferring data through named files, memory buffers, and child pipes; separating the providers from the convenience operations would currently introduce cycles between their owners.
Host I/O also owns the complete `Process` class, including its timing, identity, and standard-stream operations; assigning those members to separate services would not by itself split their shared declaration.
Shared Libraries and Host Environment use Host I/O's path operations, and none of the assigned Host I/O implementations calls either of those children.

## Service Contracts and Boundaries

### Named Files and Stream Transfers

Host I/O provides both lexical path operations and host queries, but they have different contracts.
`Path::simplify` removes path components without resolving filesystem links or consulting the working directory.
`Path::getCanonical` uses `_wfullpath` on Windows and `realpath` on the active non-Windows branch, so the two branches do not provide the same existence or link-resolution guarantees.
Directory enumeration likewise keeps its existing host-specific matching rules.
The pathname operations and `URI` conversions do not acquire a product file-system view.
The `OSFileSystem` implementations and their identity, caching, and interface policy remain with [File Systems](../file-systems/file-systems.md).

Host I/O keeps `FileStream` together with the convenience operations that read or write an entire named file.
For example, `File::readAllText` opens a `FileStream` and uses `StreamReader` to decode its bytes and normalize line endings.
Conversely, `FileStream::_init` checks `File::exists` and `Path::getPathType` before opening its C file handle.
Those implementation calls explain why the file and stream services have one owner rather than separate cyclic children.

Stream capabilities and lifetimes depend on the provider.
`MemoryStreamBase` borrows its byte storage, `OwnedMemoryStream` stores its own bytes, and `BufferedReadStream` retains its underlying stream and buffers unread bytes.
Writing through `OwnedMemoryStream` inserts bytes at the cursor rather than overwriting an existing range.
`MemoryReader` is a separate bounded borrowed reader with exact-size reads and skips; it is not a `Stream` subclass.
A successful zero-byte stream read need not mean end-of-stream, particularly for a child pipe that has no bytes available yet.
The pipe-draining operations poll again after a temporarily empty read, whereas `Stream::readExactly` fails a short read without waiting for more bytes.

`StreamReader` and `StreamWriter` adapt byte streams using Text's encoding operations.
They retain supplied streams; explicit `close` calls close the underlying stream, while `releaseStream` clears the retained reference without calling `close`.
`StreamReader::init` propagates failure from its initial `readBuffer` call, which in turn propagates failure from the underlying `Stream::read`.
`StreamReader::readBufferByte` ignores failure from later refills, and `TextReader::read` and `TextReader::peek` expose only a character.
`StreamReader::readToEnd` uses that character-reading path and returns `SLANG_OK`, so later refill failures do not propagate through `File::readAllText`.
`StreamWriter::writeSlice` returns its underlying write result, but the `TextWriter` insertion operators discard write results.
`StreamReader` and `StreamWriter` do not provide a common guarantee that arbitrary input is valid Unicode.
The product `ISlangWriter` implementations, writer-channel selection, formatted writer helpers, and default console setup remain with [Output Writers](../output-writers/output-writers.md).
`TextWriter` and `StreamWriter` are the separate stream-text abstraction, despite their similar names.

`LockFile` owns its opened file handle and uses `LockFileEx` or `flock`; `LockFileGuard` borrows that object.
The guard's constructor calls `LockFile::lock`, and its destructor calls `LockFile::unlock`; neither checks the returned result.
Constructing the guard therefore does not establish that acquisition succeeded.
The persistent cache is a client of that existing mechanism and retains ownership of its cache consistency and eviction policy.

### Invocations and Process Observation

Host I/O keeps command construction with process creation because the two services currently call each other.
`ExecutableLocation::set` uses path spelling and the host executable suffix, while command-line rendering obtains the escaping handler from `Process`.
The Windows provider renders a command string for `CreateProcessW`; the Unix provider passes the stored argument strings directly to `posix_spawn` or `posix_spawnp`.
Those operations invoke an executable rather than interpreting the arguments as a shell program.
Downstream compiler discovery, version selection, compiler switches, and diagnostic interpretation remain with Compiler's [Downstream Tools](../../compiler/downstream-tools/downstream-tools.md).
Option descriptions and help generation remain with [Command Options](../command-options/command-options.md), and located argument parsing remains with Compiler Support.

A `Process` retains its parent-side pipe streams.
The standard-stream wrappers returned by `Process::getStdStream` borrow the process's existing native standard handles or descriptors, whereas child-pipe wrappers own their native resources.
Their explicit `close` operations distinguish those cases; migration must also preserve the providers' existing error-path behavior rather than infer that every handle-clearing operation checks ownership.
Creation flags can leave the parent's input or error stream absent, and stream-draining clients must respect those cases.
`UnreadableStdin` requests a read failure in the child rather than end-of-file; its declaration records the different evidence supporting the Windows and POSIX implementations.
For ordinary exit, `UnixProcess::_updateTerminationState` narrows the exit status through `int8_t`; for signal death, it stores the signal separately from the return value.
`WinProcess::_hasTerminated` records the Windows process exit code when `GetExitCodeProcess` succeeds.
After byte draining succeeds, the `ProcessUtil::readUntilTermination` overload taking `ExecuteResult&` stores the value returned by `Process::getReturnValue` and line-normalized standard output and standard error in that record.
`ExecuteResult` has no termination-signal field, and that overload leaves its `debugLayer` field unchanged.
The byte-draining overload propagates read failures during live-process polling but ignores read results during the trailing drain after termination.
`ProcessUtil::execute` propagates the errors returned by `Process::create` and `readUntilTermination`; its success does not require a zero child return value.

Process output draining uses byte streams, and `BufferedReadStream::readUntilContains` calls `Process::sleepCurrentThread` when no more bytes arrived.
That mutual dependency is another reason to keep stream and process services within Host I/O.
`StreamUtil::readAndWrite` performs the write and the two reads on three C++ threads, joins them, and returns the `readStream` reader's result.
The write worker closes `writeStream`; the helper discards the write and `errStream` reader results.
Child refinement must preserve those existing error contracts rather than infer stronger propagation from the helper's name.

### Native Libraries and Host Information

Shared Libraries distinguishes unadorned library names from platform filenames and paths.
`SharedLibrary` performs the native load and lookup operations; `DefaultSharedLibraryLoader` adapts them to the product loader interface and has singleton lifetime.
`DefaultSharedLibrary` owns a loader reference, whereas `ScopeSharedLibrary` additionally retains a caller-supplied product object and unloads its reference before releasing that object.
For example, Compiler Support's `DefaultArtifactHandler::_loadSharedLibrary` supplies a retained file representation so that the backing file outlives the loaded reference.
That mechanism does not assign artifact construction or temporary-file policy to Host Services.
`SharedLibraryUtils::getSharedLibraryFileName` locates a library file from a symbol address; `SharedLibraryUtils::getFileTimestamp` and `SharedLibraryUtils::getSharedLibraryTimestamp` return host-local modification stamps.
Zero denotes an empty path, an unavailable timestamp, or modification time at the epoch; a nonzero stamp supports equality comparisons on that host rather than portable time arithmetic.

Native loading preserves its platform branches, including Windows loader search attempts and error mapping, POSIX loader flags, and the existing Cygwin, Apple, Linux, and fallback naming rules.
Its resident-library selection includes knowledge of particular tool and graphics libraries, as discussed below.
The generic product loader interface can have implementations other than native libraries, so using the interface does not assign those implementations to Shared Libraries.

Host Environment owns the complete `PlatformUtil` class and its platform vocabulary.
Despite `getInstancePath`'s declaration describing a library or executable instance, its Windows and Linux/Cygwin implementations return the invoked executable's parent directory; other non-Windows branches report that the operation is unimplemented.
`appendResult` supplies Windows system error text and is unimplemented on the other branch.
`outputDebugMessage` uses Windows debug output and reports unavailability elsewhere.
`backtrace` prints to `stdout`, including its unavailable-message fallback, despite its declaration mentioning `stderr`.
These are existing host debug facilities; compiler diagnostic selection and presentation remain with their owning services.

### Built-in Invocation and Residency Contracts

The current Unix `Process::create` replaces inherited `LC_ALL` with `LC_ALL=C`; its Windows counterpart inherits the environment without that override.
The current `SharedLibrary::isUnclosable` matches filename prefixes for `slang-llvm`, `libslang-llvm`, `libdxcompiler`, `libdxvk_d3d11`, and `libdxvk_dxgi`.
Native loading enforces that choice with Windows pinning or POSIX `RTLD_NODELETE` so that unloading the owned loader reference leaves the selected library mapped.
Foundation owns the complete process and native-library services under Host Services, including these existing environment and residency conventions.
The Unix `Process::create` implementation constructs its environment from inherited string entries, and `SharedLibrary::isUnclosable` compares filename prefixes; neither operation calls Compiler or graphics services to make those choices.
Clients still select programs and libraries and interpret their results.
The existing `Process` and `SharedLibrary` declarations and their platform-specific behavior remain the preservation boundary.

## Existing Implementation Mapping

Paths below are relative to `old-source/core/`.
A basename followed by `{h,cpp}` denotes both complete files, including inline operations and local support, except for the explicitly divided files.
These are semantic assignments; the shared-file and dependency holds do not authorize moving a complete legacy file into one child.

| Existing Implementation                                                           | Proposed Owner and Responsibility                                                                                                                                                                                                                                                               |
| --------------------------------------------------------------------------------- | ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| `slang-io.{h,cpp}`                                                                | Host I/O: complete `File`, `Path` and nested vocabulary and visitor, `URI`, `LockFile` and nested lock vocabulary, and `LockFileGuard`; all implementations, executable-path helpers, and host branches.                                                                                        |
| `slang-stream.{h,cpp}`                                                            | Host I/O: complete `StdStreamType`, `SeekOrigin`, `Stream`, `FileMode`, `FileAccess`, `FileShare`, `MemoryStreamBase`, `OwnedMemoryStream`, `FileStream`, `BufferedReadStream`, `StreamBufferStyle`, and `StreamUtil`, including standard C stream buffering and simultaneous transfer support. |
| `slang-text-io.{h,cpp}`                                                           | Host I/O: complete `TextReader`, `StreamReader`, `TextWriter`, and `StreamWriter`, including encoding and line-ending adaptations, insertion overloads, and local buffering.                                                                                                                    |
| `MemoryReader` at the end of `slang-riff.h`, outside `namespace RIFF`             | Host I/O: its complete declaration, fields, constructors, bounded reads and skips, and remaining-byte queries. The `RIFF` namespace, chunk types, builders, and macros stay with [RIFF Data](../riff-data/riff-data.md).                                                                        |
| `slang-command-line.{h,cpp}`                                                      | Host I/O: complete `ExecutableLocation` and `CommandLine`, including exact-definition equality, stored unescaped arguments, path-prefix composition, and host-style rendering.                                                                                                                  |
| `slang-process.h`, `slang-process-util.{h,cpp}`                                   | Host I/O: complete `Process`, flags, stream and termination state, all static host operations, `ExecuteResult`, `ProcessUtil`, and draining/text-conversion helpers.                                                                                                                            |
| `windows/slang-win-process.cpp`                                                   | Host I/O: complete Windows provider, including `WinHandle`, `WinPipeStream`, `WinProcess`, child redirection and creation, standard handles, waiting, termination, timing, suffix, escaping selection, and process identity.                                                                    |
| `unix/slang-unix-process.cpp`                                                     | Host I/O: complete Unix provider, including `UnixProcess`, `UnixPipeStream`, spawn setup and cleanup, inherited-environment adaptation, standard descriptors, signal and return-value observation, timing, suffix, escaping selection, and the WebAssembly spawn branch.                        |
| `SharedLibrary` in `slang-platform.h` and its definitions in `slang-platform.cpp` | Shared Libraries: complete handle and symbol vocabulary, native naming, loading, resident-library predicate, unloading, lookup, and associated host branches and compile-time result-code checks.                                                                                               |
| `slang-shared-library.{h,cpp}`                                                    | Shared Libraries: complete `DefaultSharedLibraryLoader`, `DefaultSharedLibrary`, `ScopeSharedLibrary`, `SharedLibraryUtils`, interface and object queries, scope lifetime, symbol/file lookup, and timestamps.                                                                                  |
| Remaining declarations and definitions in `slang-platform.{h,cpp}`                | Host Environment: complete `PlatformKind`, `PlatformFlags`, `PlatformFlag`, `PlatformFamily`, `PlatformUtil`, family table, host queries and debug output, and the non-MS C-runtime name aliases at the end of the header.                                                                      |

The current `core` target obtains these implementations through `slang_glob_sources`; `cmake/Glob.cmake` selects the Windows and Unix implementation directories using the configured host conditions.
Enabled `slang-rt` separately compiles the same core implementation directory under runtime export settings, rather than linking the `core` archive.
Runtime's `_slang_rt_load_dll` calls the default loader and retains the resulting product interfaces in its own library map.
The [root's staged migration contracts](../../slang.md#staged-migration-contracts) preserve both consumers, existing product interfaces, and required canonical C++ identities, names, signatures, layouts, and annotations.
The proposed children create no additional installed artifacts or product exports.

## Unresolved Boundaries

### Internal I/O Coupling

Host I/O gives one owner to the mutual file/stream, stream/process, and command-line/process dependencies identified by Foundation.
It does not claim that narrower providers could move independently after changing includes or relocating `FileStream` definitions.
Any later subdivision must account for those actual calls and the complete `Process` declaration before introducing narrower public children.
No callback mechanism, replacement stream contract, timing-member split, or changed file-opening policy is selected here.
Host I/O still depends on Foundation's text, collection, ownership, and failure services, whose [cross-service cycles](../foundation.md#failure-reporting-and-basic-data-services) remain ancestor holds.

### Shared Declarations and Existing Contract Limits

`slang-platform.{h,cpp}` physically combines Shared Libraries with Host Environment, and `slang-riff.h` physically combines `MemoryReader` with RIFF Data.
Those separations follow complete class boundaries; they do not require splitting either class's members across owners.
The non-MS C-runtime aliases in `slang-platform.h` also require coordination with Base Types' C Runtime Compatibility when the header is extracted, without silently broadening Host Environment into all portability support.
The declaration/implementation discrepancies and limited error and lifetime guarantees recorded above remain existing contracts to preserve or explicitly flag during phase two.
Phase one does not repair those implementations or turn their current limitations into stronger guarantees.
