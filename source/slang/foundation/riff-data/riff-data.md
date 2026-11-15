<!--
SPDX-FileCopyrightText: The Khronos Group, Inc.
SPDX-License-Identifier: CC-BY-4.0
-->

# RIFF Data

RIFF Data reads tagged chunk hierarchies from existing bytes and constructs hierarchies for writing to a stream or blob.
It supplies the chunk tags, structural traversal, and construction operations; clients define the meaning and validation of each chunk's payload.
For example, compiler serialization stores module properties in chunks, while Archives stores file entries in chunks using a different set of tags and payload records.

The implementation remains in `old-source/core/slang-riff.{h,cpp}`.
This document refines [Foundation's assignment](../foundation.md#existing-implementation-mapping) as a proposed leaf, rather than a completed extraction or a new library.
The complete generic `MemoryReader` class shares the existing header but belongs to [Host I/O](../host-services/host-io/host-io.md).

## Concepts

[Foundation](../foundation.md#concepts) defines borrowed views, owned allocations, blobs, and streams.
[Memory](../memory/memory.md#concepts) defines arenas, and [Collections](../collections/collections.md#concepts) defines intrusive lists.
The RIFF representation uses the following concepts:

- A **four-character code**, represented by `FourCC`, is a 32-bit tag whose four bytes occur in character order in memory.
  Its numeric value depends on host endianness; `SLANG_FOUR_CC` constructs the corresponding value from four supplied bytes.
  Existing clients also use that operation for nontextual signatures, such as ZIP recognition.
- A **chunk** comprises a header containing a tag and a 32-bit byte count, followed by the bytes described by that count.
  The count excludes the base chunk header.
  `Chunk::getTotalSize` includes that header.
- A **data chunk** uses its header tag as its type and treats all following bytes as payload.
  A **list chunk** uses the `LIST` tag, stores a separate type immediately after the base header, and contains child chunks.
  A **root chunk** is a list with the `RIFF` tag.
  A list's byte count includes its type, padding before children, and child contents.
- A **serialized chunk view** is a `Chunk` or derived pointer into an existing byte buffer.
  These views borrow that buffer; obtaining a root from an `ISlangBlob` does not retain the blob or its bytes.
  `BoundsCheckedChunkPtr` additionally carries the remaining byte extent used for sibling traversal.
- A **chunk builder** is a mutable node in a `RIFF::Builder` hierarchy, separate from a serialized chunk view.
  The builder's arena owns the nodes and copied payload storage.
  A data builder's **shards** are contiguous ranges whose concatenation forms that data chunk's logical payload.
  A shard can identify arena-owned copied bytes or caller-owned borrowed bytes.

`Chunk::kChunkAlignment` is eight in the current implementation.
The reader and writer round child offsets relative to their containing list to that boundary, including the first child after the larger list header.
An empty list contains only its header, without padding for an absent first child.
This spacing does not establish alignment for an arbitrary payload type or validate the absolute alignment of a supplied buffer.

## Dependencies

- [Scalar Types](../base-types/scalar-types/scalar-types.md) supplies byte, size, and fixed-width integer conventions.
  [C++ Utilities](../base-types/cpp-utilities/cpp-utilities.md) supplies `UNREACHABLE_RETURN`, which accompanies selected signal paths without reporting a failure itself.
- [Arenas](../memory/arenas/arenas.md) supplies `MemoryArena`, arena placement construction, and the current-allocation operations used to coalesce successive copied payloads.
  RIFF Data owns construction order and shard composition; the arena owns storage reclamation.
- [Linked Lists](../collections/linked-lists/linked-lists.md) supplies `InternallyLinkedList` for child and shard order.
  [Sequence Storage](../collections/sequence-storage/sequence-storage.md) supplies the byte list used to transfer completed output.
  Neither intrusive list retains or deletes its nodes.
- [Host I/O](../host-services/host-io/host-io.md) supplies `Stream::write`, `OwnedMemoryStream`, `FileAccess`, and the stream's byte-list transfer operation.
  RIFF Data writes to a supplied stream; it does not open paths or read a hierarchy directly from a stream.
- [Blobs](../blobs/blobs.md) supplies `ListBlob::moveCreate`, which takes the completed output list without copying its payload.
- [Failure Reporting](../failure-reporting/failure-reporting.md) supplies construction and payload-size assertions and signals for malformed traversed structure or impossible builder kinds.
  These are distinct from the null and result-returning paths described below.
- The product headers supply `ISlangBlob` access, `Result`/`SlangResult`, result propagation, `ComPtr::detach`, endianness selection, and token concatenation for scoped-builder macros.
  The existing product reference is transferred to the caller when output creation detaches its `ComPtr`.
- The host C and C++ environment supplies native record representation, pointer arithmetic, byte copying, and placement construction.

The aggregate `slang-basic.h` include does not establish use of every Foundation service.
In particular, the RIFF implementation does not call the output-writer services despite including `slang-writer.h`.
Compiler payload serialization and archive compression remain client work; RIFF Data does not depend on those clients or on Binary Layouts' composition service.

## Principal Services

RIFF Data is a public leaf within Foundation, with no child modules.
Its four service areas operate on the same tagged tree and byte layout:

- **Tag and header interpretation** provides `FourCC`, data/list/root classification, type selection, and payload access.
- **Borrowed hierarchy traversal** provides root recognition, bounded sibling traversal, child iteration, immediate typed lookup, and recursive list lookup.
- **Hierarchy construction** provides arena-owned nodes, ordered copied or borrowed payload shards, and a cursor with scoped begin/end helpers.
- **Hierarchy emission** computes sizes for the complete tree and writes headers, payloads, and alignment padding to a supplied stream or a newly owned blob.

The borrowed and construction types remain together because both encode the same header, kind, type, and alignment rules.
The current clients do not require independent ownership modules for their individual classes or cursor operations.
`RIFF::ChunkBuilder` builds tagged tree nodes and concatenated payloads; the separate `Slang::ChunkBuilder` assigned to [Binary Layouts' Blob Composition](../binary-layouts/blob-composition/blob-composition.md) builds aligned output sequences with deferred relative references and prefixes.
Their similar names do not identify a common class or an architectural dependency.

## Existing Contracts and Client Boundaries

### Recognition and Borrowed Traversal

`RootChunk::getFromBlob` checks for non-null storage, enough bytes for a root header, the `RIFF` tag, and a reported total size no larger than the supplied extent.
It accepts extra bytes after the reported root and does not recursively validate children or interpret payloads.
The blob overload reads the product interface's pointer and size, then returns the same borrowed view as the pointer-and-size overload.
Callers must keep the actual backing storage alive for every use of any returned chunk, child list, or payload pointer.

`BoundsCheckedChunkPtr` checks the available base header, the reported chunk size against its extent, and the larger minimum header for a list.
Its single-pointer constructor assumes that the caller has already made the header safe to access and limits navigation to that chunk's reported size.
`ListChunk::getFirstChild` derives its available bytes from the parent's reported total, and sibling traversal subtracts each aligned step from its remaining extent.
An invalid traversed size can invoke `SLANG_UNEXPECTED`; traversal is not a result-returning validation interface.
These checks do not establish complete arithmetic, payload, alignment, or hostile-input safety.

`findDataChunk` and `findListChunk` select the first matching immediate child.
`findListChunkRec` also considers the current list and then searches descendant lists in child order.
The `as` overloads distinguish chunk kinds rather than client-defined types.
`ChunkList<T>::cast` changes the iterator's static type without checking that every child belongs to the client's derived type.
Clients such as `ModuleChunk::getFileDependencies` supply that stronger format knowledge.
`DataChunk::writePayloadInto` asserts that the requested byte count fits and copies that prefix; its templated operations copy native object bytes without decoding them.

### Construction, Ownership, and Output

`Builder` permits one root list, and `ListChunkBuilder` appends new child nodes to that root or another list.
`BuildCursor` distinguishes adding a root to a builder from adding a child to a current list or appending payload to a current data chunk.
Its scoped helpers pair begin/end operations; incorrect kinds, a second root, and unmatched ends are assertion preconditions rather than ordinary reported failures.
Returned chunk-builder and shard pointers borrow arena storage controlled by the enclosing builder.

`DataChunkBuilder::addData` copies its input during the call, and a zero-byte copy is a no-op.
When the final copied shard ends at the arena's current cursor and the current block has enough space, the operation extends that shard instead of allocating another node.
`addUnownedData` instead adds a shard referring to the supplied bytes without copying or retaining an owner.
The caller must keep those bytes valid for the enclosing builder's lifetime, as required by the existing declaration contract.
Construction and arena allocation do not provide a uniform recoverable allocation-failure result.

`Builder::writeTo` recalculates each node's cached total immediately before emission, so mutation between separate write calls is allowed without maintaining cached sizes during construction.
The writer emits native headers, zero padding before children, and each data chunk's shards in order.
It borrows the stream, writes at its current position, and neither seeks, flushes, nor closes the stream.
The hierarchy's size and alignment calculations describe that emitted sequence rather than any preceding stream bytes.
Size accumulation must remain representable, and the writer asserts that each header byte count fits its 32-bit field.

The current result propagation is narrower than the public `Result` return type might suggest.
`ChunkBuilder::_writeTo` propagates failures from header, payload, and recursive child writes, but discards padding-write results.
`Builder::writeTo` returns `SLANG_FAIL` without a root, otherwise discards `_writeTo`'s result and returns `SLANG_OK`.
Output may therefore be partial even when `writeTo` reports success.
`writeToBlob` propagates `writeTo`'s reported result, swaps a fresh writable memory stream's bytes into a temporary list, and moves that list into `ListBlob`.
It then detaches the resulting product reference to its caller.
The output blob owns its completed list independently of the builder and any borrowed input shards.

### Client Formats

Compiler's [Module Storage](../../compiler/module-storage/module-storage.md) defines the module, property, IR/AST, and debug tags and payloads in `slang-serialize-container`, `slang-serialize-types`, and their serializers.
For example, `writeSerializedModuleAST` first produces a separate Fossil blob through Binary Layouts' `BlobBuilder`, then copies that completed blob into one RIFF data chunk with `BuildCursor::addDataChunk`.
The temporary blob can be released after that copy; the RIFF builder owns the copied payload.
Module loading and inspection in `slang-module-library.cpp`, `slang-global-session.cpp`, and `slang-session.cpp` recognize roots and traverse those compiler-defined chunks.
Their schema checks, version policy, and retained backing blobs remain compiler responsibilities.

Compiler's [Reproduction](../../compiler/reproduction/reproduction.md) uses another pair of root/data tags.
`ReproUtil::saveState` copies its compatibility header and borrows an `OffsetContainer`'s bytes until it writes the hierarchy before the container is destroyed.
The loading path checks reproduction compatibility and validates its copied state after unwrapping the chunk.
Those offset records and their validation remain outside RIFF Data.

[Archives](../archives/archives.md) defines the RIFF file-system header and entry records.
`RiffFileSystem::storeArchive` copies entry metadata and paths but borrows the retained file blobs' bytes while constructing and writing its result.
`loadArchive` interprets entry sizes, paths, and compression selection after RIFF traversal.
ZIP recognition uses only `FourCC` and `SLANG_FOUR_CC` for its initial signature comparison.
The VM bytecode header likewise uses `SLANG_FOUR_CC` for its own section constants without using RIFF's tree layout.
Neither use transfers those formats into RIFF Data.

## Existing Implementation Mapping

Paths below are relative to `old-source/core/`.
The assignments cover complete declarations and implementation bodies, including templates, private support, and scoped helpers.

| Existing Implementation               | RIFF Data Responsibility                                                                                                                                                                                           |
| ------------------------------------- | ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------ |
| `slang-riff.h`, before `MemoryReader` | Complete `Slang::FourCC`, `SLANG_FOUR_CC`, and the `Slang::RIFF` chunk views, bounds pointer, typed child sequence, kind casts, builder classes, shard records, cursor, scoped helpers, and scoped-builder macros. |
| `slang-riff.cpp`                      | Complete alignment support, borrowed traversal and lookup, payload copying, root recognition, tree construction, shard coalescing, size computation, stream emission, and output-blob transfer.                    |

The complete `Slang::MemoryReader` at the end of the header stays assigned to Host I/O: both constructors, fields, bounded typed/raw reads, skips, and remaining-byte queries form one generic borrowed-reader contract.
Its three current clients are RIFF archive loading, reproduction loading, and `SerialRiffUtil::readArrayChunk`; the RIFF implementation itself does not call it.
This complete-class split agrees with Foundation and Host Services and requires no ancestor reassignment.
The header's shared includes and guard still require coordination when the declarations are extracted.

## Preservation Constraints

`FourCC` stores character-order bytes, but chunk size fields and templated payloads use native C++ representation without a general byte-order or schema conversion.
The existing struct layout, width, and alignment assumptions therefore remain part of client compatibility.
Eight-byte chunk spacing does not turn arbitrary native records into portable serialized values.
Several inherited header comments still refer to two-byte alignment or a nonexistent `kAlignment`; those discrepancies do not change the actual eight-byte reader/writer convention.
Phase two must preserve the existing behavior and flag deferred concerns rather than silently repair the format, traversal validation, or write-result propagation.

The complete declarations currently have their canonical identities in `Slang` and `Slang::RIFF`.
The [root's staged migration contracts](../../slang.md#staged-migration-contracts) govern preservation of names, signatures, layout, annotations, and any required C++ binary identity; a new module header must not create duplicate chunk or tag types.
The current `core` target obtains this implementation through source globbing, and enabled `slang-rt` separately compiles the same core directory under runtime export settings.
An eventual port must preserve both consumers and remove the corresponding legacy compilation without introducing another installed artifact.

This leaf does not resolve Foundation's [failure, ownership, collection, and text cycles](../foundation.md#failure-reporting-and-basic-data-services) or the root's artifact construction and source-reuse questions.
Its dependencies and client boundaries describe the existing service without claiming that those ancestor migration holds are settled.
