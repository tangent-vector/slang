<!--
SPDX-FileCopyrightText: The Khronos Group, Inc.
SPDX-License-Identifier: CC-BY-4.0
-->

# File Systems

File Systems provides access to named files and directories through host-backed and in-memory stores, cached access to a supplied store, and access relative to a supplied directory.
It also collects immediate directory contents from stored paths, including directories implied by descendants.
Clients choose the store and interpret its contents; the module supplies the existing file-system implementations and their path, identity, and lifetime behavior.

The implementation remains in `old-source/core/`.
This document refines [Foundation's assignment](../foundation.md#existing-implementation-mapping).
The product interfaces in `include/` remain existing external contracts, and the [inherited service cycles](#migration-constraints) remain unresolved.

## Concepts

[Foundation](../foundation.md#concepts) defines file systems, blobs, owned allocations, and borrowed views.
[Object Interfaces](../object-interfaces/object-interfaces.md#concepts) defines product interface references, interface discovery, and borrowed interface lookup.
[Text](../text/text.md#owned-and-borrowed-characters) describes string and slice storage lifetimes.

- A **file-system view** interprets named files and directories through the existing product file-system interfaces.
  A view may perform host operations, hold entries itself, or transform requests to a retained backing file system.
- A **unique identity** is the zero-terminated string returned by the product `getFileUniqueIdentity` operation to compare entries reached through different paths.
  The [product contract](../../../../include/slang.h) requires equal identities for files to imply identical contents; recognizing every pair of paths to the same file is a separate concern affecting `#pragma once`.
  The implementations below use different identity computations.
- An **implicit directory** is a directory whose existence follows from a stored descendant path, even without a separate directory entry.
  Directory collection distinguishes an absent directory, an existing directory without collected children, and a directory with children.

`PathKind`, `OSPathKind`, `SlangPathType`, and `FileSystemContentsCallBack` retain their definitions in the [product header](../../../../include/slang.h).
In particular, a canonical path is provider-specific; the interface does not require every provider to return a host path or to support canonicalization.
`OSPathKind` distinguishes no host mapping, directly usable host paths, and host paths obtained through `PathKind::OperatingSystem`.
Path combination, simplification, canonicalization, display spelling, and unique identity remain separate operations.

## Dependencies

- [Host I/O](../host-services/host-io/host-io.md) supplies lexical path combination and simplification, host path queries, directory enumeration and mutation, whole-file reads, and the file stream used for host writes.
  Even the in-memory and relative implementations use its lexical path operations; they do not need host file access for those operations.
- [Text's Strings](../text/strings/strings.md) supplies owned paths, builders, borrowed path components, comparisons, ASCII lowercasing, and integer append operations.
  [Text Operations](../text/text-operations/text-operations.md) supplies character replacement and the current `StringUtil` blob/string adaptations.
  [String Interning](../text/string-interning/string-interning.md) supplies the directory collector's distinct child names and associated path types.
- [Collections](../collections/collections.md) supplies path-to-entry maps, identity-to-record maps, and their lookup and iteration operations.
  The file-system implementations determine what their string keys mean; the containers do not supply another identity policy.
- [Hashing](../hashing/hashing.md) supplies the stable 64-bit content hash used by the cache's hash-based identity modes.
  Those modes also include the lowercased filename in the identity string.
- [Blobs](../blobs/blobs.md) supplies copied or adopted file bytes and the string blobs returned for paths and identities.
  [Heap Storage](../memory/heap-storage/heap-storage.md) supplies the `ScopedAllocation` whose payload the host reader transfers to a raw blob.
- [Object Interfaces](../object-interfaces/object-interfaces.md) supplies the standalone atomic reference-counting base and its query, retain, and release macros for cache, relative, and memory objects.
  Each concrete class determines its supported interfaces and borrowed implementation lookup.
- [Scalar Types](../base-types/scalar-types/scalar-types.md) and [C++ Utilities](../base-types/cpp-utilities/cpp-utilities.md) supply count/index types, compiler annotations, and unused-value declarations.
  [Failure Reporting](../failure-reporting/failure-reporting.md) supplies assertions for retained-interface assumptions, cache-record consistency, entry kinds, and collected child kinds.
- The product headers supply the three file-system interface levels, blob interfaces, UUIDs and comparison, result codes, calling conventions, and `ComPtr` retention and output-reference operations.
  The host C++ environment supplies allocation and deletion of objects and cache records.

These dependencies follow operations in the assigned declarations and implementations.
The `slang-basic.h` aggregate does not establish additional dependencies on every service it includes.
Archives and compiler clients use File Systems; the assigned implementations do not call their format readers, compilation policy, or repro serialization.

## Service Areas

File Systems is a proposed leaf with no child modules.
The providers and wrappers implement the same product protocol, while the collector supplies their reusable operation for listing entries from paths.
Keeping their shared capability and path support together permits complete-class ownership without a replacement interface or a member-by-member class split.
Its services are public within Foundation because compiler sessions, compiler-support artifacts, archive implementations, and existing tools use them directly.
Protected entry operations and cache internals retain their existing access; public module visibility does not broaden C++ member visibility or create product exports.

### Interface Capabilities and Object Lifetimes

`FileSystemStyle` distinguishes load-only, extended, and mutable interfaces.
The shared implementation support negotiates the most capable supplied interface, checks style-gated operations, and combines paths lexically when a backing extended interface is unavailable.
`OSFileSystem` has three static instances whose discovery exposes the selected style, although the C++ class implements the mutable interface.
`CacheFileSystem` exposes the extended interface and its existing class UUID through borrowed lookup.
`RelativeFileSystem` exposes the interface level discovered on its backing object; `MemoryFileSystem` and `NULLFileSystem` expose all three levels.

Cache, relative, and memory objects use `ComBaseObject` product reference counting.
Cache and relative objects retain their backing interfaces, and memory entries and cache records retain their content blobs.
The product contract makes callers responsible for releasing returned blob references.
Cache and memory loads add references to stored blobs when returning them.
Both `OSFileSystem` and `NULLFileSystem` implement `addRef` and `release` as no-ops returning one.
`NULLFileSystem` remains publicly constructible and derivable; Compiler's `ReplayNullFileSystem` overrides reference counting for separately allocated playback objects.
Preserving that inherited virtual behavior matters independently of preserving the unavailable-I/O results.

### Host-Backed and Unavailable Access

`OSFileSystem` adapts Host I/O to the product interfaces.
It loads binary bytes into a raw blob, writes through a file stream, queries and mutates named paths, and converts directory visitor results into product callbacks.
Its unique identity is its canonical path.
The underlying canonicalization uses `_wfullpath` on Windows and `realpath` on the active non-Windows branch; existence and link-resolution behavior therefore remain host-specific.
Display and operating-system path requests try canonicalization and then simplification.

Delimiter and error handling are operation-specific.
On the non-Windows branch, loading, canonicalization, and path-type queries replace backslashes before host access; enumeration and removal use the supplied path, and saving opens the original path despite constructing a replaced spelling.
`OSFileSystem::enumeratePathContents` ignores `Path::find`'s result and returns `SLANG_OK` after the style check.
`OSFileSystem::createDirectory` returns the boolean result of `Path::createDirectory` as a `SlangResult`, so both boolean outcomes are nonnegative.
These are observed implementation limits, rather than stronger error guarantees inferred from the product operation names.

`NULLFileSystem` reports `SLANG_E_NOT_AVAILABLE` from its result-returning file and path operations, leaves their output arguments untouched, and performs no cache clearing.
Its host-path kind is `None`; its interface and lifetime operations still function as described above.

### Cached Access and Identity

`CacheFileSystem` adapts a supplied load-only or extended provider to the extended interface and caches load, path-type, and canonical-path information.
An input-path map can contain null failed resolutions or point to a shared `PathInfo`; the unique-identity map owns the records deleted by the cache.
Multiple path spellings may therefore share one record and retained content blob.
The public map accessors and `PathInfo` fields are used by Compiler's repro code to save and restore that state, including a cache populated without a backing provider.

The selected `UniqueIdentityMode` controls whether identity comes from the backing extended interface, the input path, a simplified path, or a content hash combined with the lowercased filename.
`SimplifyPathAndHash` first resolves a different simplified spelling through the path cache, then computes a hash identity when no record is available.
In the hash modes, a failed file load can fall back to the backing extended identity operation, allowing that provider to identify directories.
Default identity selection uses the extended provider when present and `SimplifyPathAndHash` otherwise; requesting the extended identity mode without that interface also selects the latter mode.
The separate `PathStyle` controls delegated versus lexical path processing.
The hash computation is finite, and the path modes rely on their selected spelling; the architecture does not strengthen those identities or change their collision behavior.

Cached results use the existing compact result vocabulary: success, not found, cannot open, and generic failure, in addition to uninitialized state.
Other failure codes are compressed to generic failure.
Resolution failures can instead become null path-map entries before a load or path-type result is cached.
Without an extended provider, path-type queries use load results and identify successful loads as files; enumeration only scans known cached paths and requires a simplification-capable identity mode.
With an extended provider, enumeration delegates to it.
`clearCache` deletes owned records, clears both maps, and forwards cache clearing to the retained extended provider.
`setInnerFileSystem` changes the retained interfaces and selected modes without clearing those maps.

The current canonical-path helper records the provider's result but returns `SLANG_OK` after an attempted provider query when a path record exists, even if no canonical-path blob was produced.
Failed path resolution returns not found, and an uninitialized canonical result without an extended provider returns not implemented.
Migration must preserve that distinction, the result-code compression, and caller access to restored cache state rather than silently improving the error protocol or invalidation policy.

### Relative Access

`RelativeFileSystem` retains the most capable discovered backing interface and a directory path used to translate entry access.
By default, it simplifies an entry path with `AbsoluteOnlyAndNoRoot`; the alternative `stripPath` mode keeps only the filename.
It then combines that spelling with the supplied directory using the backing extended operation when available, or lexical combination otherwise.
Loading can therefore use a load-only backing provider, while extended and mutable operations remain gated by their respective discovered interfaces.
The simplified-path query is delegated on the original input, canonical-path queries return the view-local simplified spelling, and operating-system queries translate through the supplied directory.
A display-path request chooses simplified or operating-system spelling according to the backing host-path kind.
The wrapper changes a direct host-path kind to `OperatingSystem` when its supplied directory is nonempty.
These lexical transformations do not establish containment through host symbolic links or authorize a changed path-security policy.

### In-Memory Entries and Directory Collection

`MemoryFileSystem` stores file and directory entries by simplified, rootless path and supplies a separate root entry found through `.`.
Creating an entry requires its parent directory to exist; file replacement requires a file entry, and removal rejects a nonempty directory and never removes the root.
Saving raw bytes copies them into a blob, while saving a blob retains the supplied immutable blob.
Canonical and display paths use the lexical spelling without requiring an entry to exist; unique identity uses that canonical spelling.
The provider reports no host-path mapping and leaves cache clearing as a no-op.

The complete protected `Entry` stores a path kind, canonical path, logical uncompressed byte count, and retained contents.
Archives' `RiffFileSystem` derives from `MemoryFileSystem` and uses those entries and protected lookup, creation, and clearing operations to hold compressed contents alongside their uncompressed sizes.
Compression, decompression, archive parsing, and serialized entry layout belong to Archives; the generic entry representation and complete memory-file-system class remain together here.

`ImplicitDirectoryCollector` collects distinct immediate children from supplied canonical paths using `/` to separate descendants.
The caller supplies the path spelling and any known directory-existence fact; the collector also infers existence from matching entries or children.
Its root recognition accepts an empty path, `.`, or a single delimiter, and its stored non-root prefix ends in `/`.
It emits child names through the product callback and reports not found only when no directory existence was established.
Memory enumeration and nonempty-directory checks use the collector, and Archives' ZIP implementation uses its existence/content states to stop scans once a requested fact is known.
The collector's name pool supplies zero-terminated callback names during enumeration; a callback's copied pointer does not retain that storage.

## Existing Implementation Mapping

Paths below are relative to `old-source/core/`.
Each row assigns the complete named declarations and implementations, including inline operations, nested types, fields, interface hooks, and local support.
The host-specific file/path primitives, archive formats, and compiler clients are declared and implemented elsewhere.

| Existing Implementation                      | File Systems Responsibility                                                                                                                                                                                                    |
| -------------------------------------------- | ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------ |
| `slang-file-system.h`                        | Complete `FileSystemStyle`, `OSFileSystem`, `CacheFileSystem` with its nested modes, `CompressedResult` and `PathInfo`, `RelativeFileSystem`, and `NULLFileSystem`; their complete current public/protected/private contracts. |
| `slang-file-system.cpp`                      | All five shared capability/path helpers, the delimiter helper, all provider/wrapper definitions and static host instances, the compact-result table and conversion, cache algorithms, and relative translation.                |
| `slang-memory-file-system.{h,cpp}`           | Complete `MemoryFileSystem`, protected `Entry`, entry/root storage, lookup and path operations, byte/blob saves, mutation, enumeration, interface support, and local definitions.                                              |
| `slang-implicit-directory-collector.{h,cpp}` | Complete `ImplicitDirectoryCollector`, its ordered `State` vocabulary, prefix/remainder access, child collection and deduplication, existence tracking, root recognition, and callback enumeration.                            |

The product file-system interfaces and path vocabulary in `include/slang.h`, the product reference wrappers in `include/`, and Host I/O's `File`, `Path`, and stream implementations remain with their existing contracts and owners.
The archive interface, format detection/factories, ZIP and RIFF providers, compression choices, and archive size limits remain with [Archives](../archives/archives.md).
Compiler retains session default/wrapping choices, repro serialization and restoration, playback stand-ins, include-search policy, and command-line file-system mode selection.
Compiler Support retains artifact-container and file-representation policy; existing unit tests and graphics tools remain clients.

## Migration Constraints

The three implementation pairs currently enter static `core` through its source glob.
Enabled `slang-rt` separately compiles the same core directory under runtime export settings rather than linking that archive.
A later port must update both source selections and preserve the [root's staged migration contracts](../../slang.md#staged-migration-contracts), including existing names, signatures, visibility, layouts, member order, and any required canonical C++ identities.
Product UUIDs, interface inheritance, virtual dispatch and calling conventions remain fixed.
The proposed leaf creates no new artifact or product export.

Complete-class ownership is significant at the client boundaries.
Repro code can construct and fill `CacheFileSystem::PathInfo` records and mutate the exposed maps, while `RiffFileSystem` and `ReplayNullFileSystem` derive from their respective classes.
Relocating only selected methods or assigning protected fields to an archive or compiler concern would leave the shared class declaration unresolved.
Header extraction must preserve those uses and the existing aggregate/include reachability until clients are deliberately updated; a direct include edge alone is insufficient evidence of a new service dependency.

File Systems requires the text, collection, blob, object-reference, allocation, failure, and Host I/O services listed above.
Their [Foundation cycles](../foundation.md#unresolved-boundaries), including the current Text/Blobs adaptations, remain prerequisites to an acyclic migration; this leaf does not resolve them.
No additional ancestor ownership change is required for the six assigned files.
The host-specific path rules, cache identity modes and collision properties, exposed mutable cache state, limited error propagation, and failure-side mutation remain existing behavior for a later port to preserve or mark with the pitch's deferred TODOs.
For example, `MemoryFileSystem::saveFile` can create an empty entry before blob creation fails; phase one does not introduce rollback or repair that state.
