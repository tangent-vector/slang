<!--
SPDX-FileCopyrightText: The Khronos Group, Inc.
SPDX-License-Identifier: CC-BY-4.0
-->

# Archives

Archives reads and writes collections of named files as ZIP or Slang-specific RIFF archives and exposes their contents through mutable file-system views.
Clients can recognize an archive in a supplied byte buffer, load its files, change its entries, and serialize the resulting collection into a blob.
Choosing the files and interpreting their contents remain client responsibilities.

The implementation remains in `old-source/core/`.
This document refines [Foundation's assignment](../foundation.md#existing-implementation-mapping) as a proposed leaf within the existing Foundation artifact.
It introduces no library or completed code move, and the [migration constraints](#migration-constraints) remain in force.

## Concepts

[Foundation](../foundation.md#concepts) defines archives, blobs, owned allocations, and borrowed views.
[File Systems](../file-systems/file-systems.md#concepts) defines file-system views, unique identities, and implicit directories.
[Object Interfaces](../object-interfaces/object-interfaces.md#concepts) defines product interface references and borrowed interface lookup.

- An **archive entry** is a named file or directory represented by an archive file-system view.
  A file entry has file bytes; a directory entry has a path and kind without file bytes.
  ZIP can also expose implicit directories without a directory entry in the serialized archive.
- An **archive format** specifies how the entries are serialized.
  `SlangArchiveType` selects ZIP, uncompressed RIFF, DEFLATE RIFF, or LZ4 RIFF when creating a view.
  In a RIFF archive, the encoding identifier is part of the archive header; changing the compression preference is a separate operation from selecting that encoding.

Both supplied implementations combine the product file-system interfaces with `IArchiveFileSystem` for whole-archive loading, serialization, and compression preferences.
The format factories return those product interfaces; clients obtain the archive operations through interface lookup.
The interface's `blobOwnsContent` argument distinguishes an independent output blob from an output that may borrow the file system's storage.
It does not select whether the file system takes ownership of the input buffer when loading an archive.

## Dependencies

- [File Systems](../file-systems/file-systems.md) supplies `MemoryFileSystem`, including the entry table, path operations, directory mutation, and protected file-entry operations inherited by the RIFF implementation.
  Its `ImplicitDirectoryCollector` supplies ZIP's path-type queries, directory enumeration, and empty-directory checks.
- [Host I/O](../host-services/host-io/host-io.md) supplies lexical `Path` simplification, combination, and parent-directory extraction for ZIP paths.
  It also owns the bounded `MemoryReader` used to read a RIFF entry's fields and payload, currently declared in `slang-riff.h`.
  Those uses do not open host files or require a stream.
- [RIFF Data](../riff-data/riff-data.md) supplies recognition and traversal of RIFF chunks and construction of the serialized chunk hierarchy.
  Both formats use its `FourCC` representation and `SLANG_FOUR_CC` macro for signatures; ZIP does not use the RIFF tree or builder.
- [Compression](../compression/compression.md) supplies the shared compression preference and encoding vocabulary, and the buffer codecs selected by RIFF archive metadata or construction.
  ZIP interprets the preference itself and uses miniz directly rather than calling a buffer codec.
- [Blobs](../blobs/blobs.md) supplies copied and adopted file bytes through `RawBlob` and ZIP's borrowed serialized output through `UnownedRawBlob`.
  [Memory's Heap Storage](../memory/heap-storage/heap-storage.md) supplies `StandardAllocator` callbacks for miniz and the `ScopedAllocation` storage used for ZIP archive buffers and decoded files.
  `ScopedAllocation` is still declared alongside the blob classes; that physical location does not make its allocation service a blob operation.
  ZIP path output additionally calls `StringUtil::createStringBlob`, whose proposed [Blobs ownership](../blobs/blobs.md#unresolved-boundaries) remains subject to the shared `StringUtil` class decision.
- [Text's Strings](../text/strings/strings.md) supplies owned entry paths, builders, borrowed character ranges, comparisons, and slash trimming.
  [String Interning](../text/string-interning/string-interning.md) supplies ZIP's owned path-to-index map.
- [Collections](../collections/collections.md) supplies ZIP's set of removed entry indices and the entry-map operations used directly by the RIFF implementation.
  These containers store the archive implementation's path and deletion decisions rather than defining those policies.
- [Object Interfaces](../object-interfaces/object-interfaces.md) supplies `ComBaseObject`, the `SLANG_COM_BASE_IUNKNOWN_ALL` implementation macros, and the borrowed `as<T>` interface lookup used by format dispatch.
  [Scalar Types](../base-types/scalar-types/scalar-types.md) supplies the count and integer types, and [Failure Reporting](../failure-reporting/failure-reporting.md) supplies assertions about ZIP modes, appended indices, and allocator compatibility.
- The product headers in `include/` supply `SlangArchiveType`, file-system and blob interfaces, path categories, UUIDs, calling conventions, and result codes.
  `slang-com-ptr.h` supplies retained `ComPtr` values and output-reference operations; `slang-com-helper.h` supplies `Guid` comparison and `SLANG_RETURN_ON_FAIL`.
  `slang.h` supplies the interface-declaration and unused-value macros used here.
- The external miniz library supplies ZIP parsing, entry extraction, heap writing, compression levels, and copying compressed entries between archives.
  The host C++ environment supplies object allocation, byte copying, fixed-width integer types, and the allocation-overflow limit used by ZIP's callbacks.

These dependencies follow declarations and operations in the assigned files.
The `slang-basic.h` aggregate does not establish a dependency on every included service.
Archives calls the RIFF builder and accesses supplied buffers through the RIFF and `MemoryReader` operations; the assigned implementations do not call Binary Layouts or stream services directly.

## Service Areas

Archives has no child modules.
The shared interface, format selection, entry-size bound, and two format implementations serve one task: presenting an editable collection of named files together with its serialized archive.
Keeping those operations together gives the shared contract and the dispatch to concrete implementations one owner without introducing a parent-to-format-to-parent dependency.
The existing format files remain implementation organization within the leaf.

Archives is public within Foundation because Compiler and Compiler Support use its factories and archive interface, and existing unit tests also construct the concrete formats.
That module visibility does not broaden C++ member access or add product exports.
The file-system interface definitions remain product contracts, and ordinary in-memory views remain with File Systems.

### Shared Archive Operations and Format Selection

`createArchiveFileSystem` selects an empty ZIP view or a RIFF view configured with no codec, raw DEFLATE, or LZ4.
Undefined and unrecognized archive types return `SLANG_FAIL`.
`loadArchiveFileSystem` first tries ZIP recognition, then RIFF recognition, creates a matching view, and calls its `loadArchive` operation before publishing the extended file-system interface.
The resulting object also supports the mutable interface even though the factory's output type is extended access.

Recognition selects a reader rather than proving that every file can be decoded.
ZIP recognition checks the first four bytes against three accepted ZIP signatures.
RIFF recognition uses `RIFF::RootChunk::getFromBlob` and the Slang container type, without recursively validating every entry.
Loading and later file access retain their existing additional checks and failure behavior.

The shared header owns the configurable per-file uncompressed-size bound and its older ZIP-specific macro alias.
The default bound is 256 MiB; the predicate also requires room for a terminating byte in a `size_t` allocation.
ZIP file extraction and compressed RIFF file loading apply that predicate before allocating decoded storage and report `SLANG_E_OUT_OF_MEMORY` when the bound is exceeded.
Uncompressed RIFF file access returns its existing blob without that decoding allocation.
The bound is neither a limit on archive input size nor an aggregate decoded-byte or entry-count budget; callers remain responsible for those limits.

### ZIP Entry Access and Mutation

`ZipFileSystem` exposes creation and recognition; its `ZipFileSystemImpl` implements the complete mutable and archive interfaces privately in the implementation file.
The implementation keeps a path-to-miniz-index map and marks deleted or replaced indices in a set.
It copies surviving compressed entries when a mode transition requires rebuilding the archive, rather than decoding and recompressing every retained file.
Its uninitialized, reader, and readable-writer modes also determine which object owns the archive allocation.

ZIP path lookup uses `Path::SimplifyStyle::AbsoluteOnlyAndNoRoot`.
Canonical and display queries prefer an explicit entry's stored spelling and otherwise return the simplified spelling; unique identity delegates to that canonical-path query.
ZIP exposes no operating-system path, and clearing its cache does nothing.
Saving a file can create implicit directories, while deleting the descendant that implied a directory can make that directory disappear.
Removing an explicit nonempty directory fails.
These rules differ from RIFF's inherited requirement for existing parent directories.

Loading a ZIP copies the supplied archive buffer.
File extraction returns a new owned, terminated raw blob.
Serialization with `blobOwnsContent` set copies the serialized bytes; serialization without it returns an `UnownedRawBlob` that does not retain the file system or its allocation.
The shared interface permits any later file-system method call or loss of the file system's lifetime to invalidate that borrowed content, so retaining the blob interface alone does not keep its bytes alive.

All miniz buffers use `StandardAllocator` through matching allocate, reallocate, and free callbacks.
The in-place reader-to-writer transition requires a growable buffer compatible with miniz's reallocation callback and transfers ownership away from `ScopedAllocation` after conversion succeeds.
Finalizing a heap writer transfers its allocation back to `ScopedAllocation`, guarded by the existing allocator-callback assertions.
The implementation also installs a miniz memory-reader callback on its heap writer to support reading and selective copying during mutation.
Those mode and callback details are internal constraints of the ZIP implementation, not another generic compression service.

### Slang RIFF Entries and Encoding

`RiffFileSystem` extends the complete `MemoryFileSystem` class and adds archive conversion and optional file compression.
Ordinary path lookup, identity, enumeration, directory creation, and removal use the inherited implementation.
The constructor retains the supplied compression interface; the built-in codec accessors return statically stored implementations whose reference operations do not control their lifetime.
When a codec is selected, an entry's stored content blob holds compressed bytes and its separate uncompressed-size field records the byte count exposed by file loading.
Saving compresses the supplied bytes, while loading decompresses into an owned terminated blob.
Without a codec, `saveFileBlob` can retain the supplied immutable blob and file loading returns a retained reference to the stored content.
The retained blob's own storage-lifetime contract still applies.

`RiffFileSystemBinary` owns the archive-specific container, header, and entry layout.
The `Scon` container contains a `Shea` header identifying the encoding and `Sfil` entries recording compressed size, uncompressed size, path byte count, and path kind, followed by a terminated path and file payload.
These tags and layouts are Slang's archive format rather than a general RIFF convention.
Loading chooses the codec from the header, copies file payloads and path strings into the in-memory entry table, and omits the serialized root entry.
The input buffer need only remain available during that call.

Serialization constructs a RIFF hierarchy from the current entry table and writes it to an owned blob regardless of `blobOwnsContent`.
The builder temporarily borrows entry payloads while forming that output; the final serialized blob has independent storage.
The archive layout uses its existing 32-bit fields and native structure representation.
This refinement does not introduce a new endian conversion, size-width policy, or archive version.

### Client Policy and Serialization Boundaries

For example, `Session::saveBuiltinModule` serializes a language module into bytes, writes those bytes as a named `.slang-module` file in an archive view, and requests independent archive output.
`Session::loadBuiltinModule` recognizes and loads that archive before interpreting the stored module file.
Compiler owns the language-module serialization, naming, and preparation policy; Archives only stores and retrieves the named bytes.

Likewise, `ArtifactContainerUtil::writeContainer` in Compiler Support selects ZIP for a `.zip` output name, arranges the artifact hierarchy, and writes the borrowed archive output to its destination before releasing the view.
Its corresponding read operation selects ZIP from the artifact kind and then interprets the archive's files as artifacts.
Archive selection from filenames or artifact kinds and the artifact directory conventions stay with those clients.
No assigned archive implementation calls compiler or artifact services.

Neither format's API promises a reproducible byte sequence for a given collection of files.
ZIP's miniz entry-writing operation records local timestamps, and RIFF serialization follows the entry table's existing iteration order.
Compression preferences affect subsequent saves: ZIP maps them to miniz levels, whereas the current RIFF buffer codecs ignore the supplied preference.

## Existing Implementation Mapping

All six files assigned by Foundation remain wholly within Archives:

| Existing files under `old-source/core/` | Assigned responsibility                                                                                                                                                              |
| --------------------------------------- | ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------ |
| `slang-archive-file-system.{h,cpp}`     | `IArchiveFileSystem`, the shared size macros, bound constant and predicate, and both creation/loading factories with format and RIFF codec dispatch.                                 |
| `slang-zip-file-system.{h,cpp}`         | `ZipFileSystem`, complete `ZipFileSystemImpl` with its mode state, entry access and mutation, all allocator and reader-callback support, recognition, and compression-level mapping. |
| `slang-riff-file-system.{h,cpp}`        | `RiffFileSystemBinary` and its nested layouts and tags, and the complete `RiffFileSystem` class with its inherited-storage adaptations, codec selection, loading, and serialization. |

This assignment includes all declared members, inline operations, file-local helpers, constants, and macros in those files.
No member of either concrete file-system class is assigned elsewhere.
`MemoryFileSystem` and `ImplicitDirectoryCollector` remain with File Systems, `MemoryReader` with Host I/O, the RIFF chunk API with RIFF Data, and the codec implementations with Compression.
Using those services does not transfer their declarations into Archives.

## Migration Constraints

The archive interface UUID, existing declaration names and signatures, result behavior, format tags and layouts, compression identifiers, and input/output storage lifetimes remain unchanged by this ownership description.
A later port must follow the root's [canonical identity and export rules](../../slang.md#staged-migration-contracts) without inventing another definition of a product interface or cross-binary C++ type.
Existing direct callers of `ZipFileSystem`, `RiffFileSystem`, and `RiffFileSystemBinary`, including the unit tests, must retain access.

Both static `core` and enabled `slang-rt` currently compile the assigned implementations, with miniz and LZ4 linked under each artifact's existing settings.
The one Foundation owner does not authorize dropping runtime's separate compilation or linking it to `core` instead.
The root's unresolved [artifact and source-reuse questions](../../slang.md#unresolved-architectural-dependencies) and Foundation's [low-level service cycles](../foundation.md#unresolved-boundaries) remain holds on migration.
RIFF Data and Host I/O must also retain their explicit shared-header assignment for `MemoryReader`; including `slang-riff.h` does not settle that header separation.

Existing error paths do not provide transactional loading or complete propagation of mode-transition failures.
For example, RIFF loading clears its entry table before later header and entry checks can fail, and ZIP saving and serialization discard some `_requireMode` results.
RIFF's header access, path-field checks, and 32-bit size conversions also require a separate correctness audit before anyone claims complete malformed-input validation.
Those inherited issues, the ZIP reader-callback technique, and missing implementation narrative remain deferred concerns under the pitch's TODO policy during a later port; phase one makes no behavioral repair.
No additional ancestor reassignment is required for these six files, and this leaf does not resolve the ancestor holds by declaring itself refined.
