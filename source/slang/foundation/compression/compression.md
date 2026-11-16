<!--
SPDX-FileCopyrightText: The Khronos Group, Inc.
SPDX-License-Identifier: CC-BY-4.0
-->

# Compression

Compression compresses byte buffers into owned blobs and decompresses them into caller-provided storage.
It supplies a common contract and the existing DEFLATE and LZ4 implementations.
Callers own the surrounding archive structure, file names, and serialized-program interpretation.

This is a public leaf module within [Foundation](../foundation.md#candidate-decomposition).
Its public declarations are in `compression.h`, and `compression.cpp` implements both codecs.
The public names belong to `slang::foundation::compression` and are reexported by Foundation.

## Concepts

- A **compression system** is a service that compresses and decompresses buffers using one identified encoding.
  `ICompressionSystem` is the shared implementation interface; `CompressionSystemType` identifies the encoding, with `None` representing uncompressed storage in archive metadata.
- A **compression style** is a request for a compression level or a preset balance between speed and compressed size.
  `CompressionStyle` carries that request.
  Both buffer-compression implementations currently ignore it; the ZIP implementation in [Archives](../archives/archives.md) interprets the same descriptor for its own compression operations.
- A **compressed buffer** is the encoded byte sequence returned by compression.
  Callers select the corresponding compression system and supply the original byte count when decompressing it.
  The DEFLATE implementation uses raw DEFLATE data, and the LZ4 implementation uses the LZ4 block format.

The [Foundation concepts](../foundation.md#concepts) define blobs and owned allocations.
The [root concepts](../../slang.md#concepts) define implementation visibility and dependencies.

## Dependencies

- [Blobs](../blobs/blobs.md) supplies the owned `RawBlob` storage returned by compression.
- [Memory](../memory/memory.md) supplies the `RefObject` base of the concrete compression systems and the `ScopedAllocation` buffer transferred into the LZ4 result blob.
- The product headers inherited through [Foundation](../foundation.md#dependencies) supply `ISlangUnknown`, `ISlangBlob`, interface identifiers, COM pointer helpers, result codes, and calling-convention declarations.
  Those existing external contracts remain outside this implementation module.
- `miniz` supplies the raw DEFLATE buffer operations and manages their temporary compression allocation.
  The wrapper copies that allocation into a Slang-owned blob and releases the temporary with `mz_free`.
- LZ4 supplies compression bounds, block compression, and decompression into supplied storage.

The public header includes only the product interface header; the concrete ownership and codec dependencies remain in `compression.cpp`.
In particular, the wrappers do not perform file I/O or interpret archive structure.
The low-level service cycles recorded by [Foundation](../foundation.md#unresolved-boundaries) remain unresolved by this leaf's decomposition.

## Service Areas

Compression has no child modules.
Its public implementation services are:

- **Compression selection and preferences:** The shared system interface, encoding identifiers, and style descriptor let callers select a buffer implementation or configure an archive writer.
- **Buffer conversion:** Compression reads the supplied input during the call and returns an owned blob through the product interface.
  Decompression writes into caller-provided storage; its contract requires the original byte count as the output size.
  Operations report ordinary failures through `SlangResult`.
  The output buffer may have been modified when decompression fails.
  Readable input and writable output do not establish a general overlap guarantee.
  Raw DEFLATE can pass overlapping spans to `memcpy`; LZ4's supported in-place layouts remain codec-specific.
- **Available systems:** `DeflateCompressionSystem` and `LZ4CompressionSystem` provide access to function-local static implementations.
  Their COM `addRef` and `release` operations return a constant value and do not control singleton lifetime.
  The concrete implementation classes and their interface-discovery routines remain private implementation details within this leaf.

The two small wrappers share the same buffer contract and ownership model, so the leaf contains both codecs.

## Clients and Compatibility

[Archives](../archives/archives.md) selects a system when creating a compressed RIFF file system and stores compressed file contents through that system.
The RIFF archive header records `CompressionSystemType`, and each file entry records the original size used to allocate and decompress its contents.
Archive loading, file-system views, size budgets, and ZIP storage remain Archives responsibilities.
ZIP uses miniz directly and shares only the style descriptor with these wrappers.

The standard-module embedding generator in `tools/slang-generate-monolithic-standard-module-library/` compresses serialized modules with LZ4.
The optional `slang-standard-modules` library exposes the compressed bytes and sizes, and `tryLoadStandardModuleFromMonolithicLibrary` in `old-source/slang/slang-session.cpp` decompresses them before deserialization.
That cross-library contract exchanges byte pointers and sizes rather than `ICompressionSystem` objects.
The compression unit test and file-system unit tests also use these services.

The migrated declarations preserve the shared interface's UUID, method order, signatures, and calling convention, as well as singleton behavior, the encoding identifiers stored in RIFF archives, and the buffer formats used by both archive and embedded-module readers.
The wrappers' style and error behavior is unchanged.
`ICompressionSystem` and the singleton access classes are implementation declarations, not installed product interfaces; `ISlangUnknown` and `ISlangBlob` retain their product API definitions.
The unannotated Compression declarations now use the leaf namespace.
The legacy native ownership and blob types keep their existing identities.
Each separately compiled implementation has its own function-local static instances; the accessors do not promise one instance across libraries or executables.

## Existing Implementation Mapping

The implementation is organized within this leaf as follows:

- `compression.h` declares the complete style descriptor, encoding identifier, shared compression interface, and both singleton accessors.
  It includes the product interface header without exposing concrete blob, allocator, or codec declarations.
- `compression.cpp` defines the two concrete implementations in separate raw DEFLATE and LZ4 block sections, including interface discovery, buffer conversion, and allocation handoff.
  It still uses `Slang::RefObject`, `Slang::RawBlob`, and `Slang::ScopedAllocation` through their legacy headers while those owning services remain unported.
- `CMakeLists.txt` adds the same implementation file to the existing static `core` target and the optional separately compiled `slang-rt` target.
  Both targets retain their private miniz and LZ4 links and their existing export settings.

The five legacy Compression files under `old-source/core/` have been removed, and their clients include the new primary header.
The archive implementations, embedding generator, compiler loader, and unit tests remain with their respective owners.
This source reuse follows [Foundation's existing build arrangement](../foundation.md#build-consumers-and-ancestor-decisions) without introducing a library for this leaf.
