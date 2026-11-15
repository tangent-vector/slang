<!--
SPDX-FileCopyrightText: The Khronos Group, Inc.
SPDX-License-Identifier: CC-BY-4.0
-->

# Binary Layouts

Binary Layouts constructs byte buffers whose internal references remain meaningful when the complete buffer moves.
It provides references resolved from the reference's own address, references resolved against a supplied buffer base, and composition of separately accumulated data into one aligned output sequence.
Clients own the meaning and validation of the records stored in those buffers.

The implementations remain in `old-source/core/`.
The proposed children refine [Foundation's assignment](../foundation.md#existing-implementation-mapping); they introduce no new library or product format.
The [preservation constraints](#preservation-constraints) distinguish the existing representations from stronger portability, validation, or failure guarantees.

## Concepts

[Foundation](../foundation.md#concepts) defines relocatable references, blobs, streams, owned allocations, and borrowed views.
[Memory](../memory/memory.md#concepts) defines arenas and allocation families, and [Collections](../collections/collections.md#concepts) defines intrusive lists.
Binary Layouts distinguishes two reference representations:

- A **self-relative pointer** stores a signed byte displacement from the address of its own field to the target.
  `RelativePtr<T, Traits>` supplies the existing 32-bit and 64-bit forms.
  A stored zero means null, so a target at the field's own address cannot be distinguished from null.
- A **buffer-relative pointer** stores an unsigned byte displacement from a separately supplied buffer base to the target.
  `Offset32Ptr<T>` stores only that 32-bit displacement; it does not remember the base or identify which buffer owns the target.
  `Offset32Ref<T>` represents the corresponding non-null reference, and `Offset32Array<T>` stores a buffer-relative pointer and a 32-bit element count.
  Zero is the null pointer displacement; an `OffsetContainer` reserves its first eight bytes so its allocations begin at a nonzero displacement.

For deferred composition, a **chunk** is an appendable logical sequence with a minimum content alignment and an optional prefix immediately before its content.
A **shard** is an implementation record for copied bytes, a pending self-relative pointer, or a field containing the total output size.
Chunks may accumulate data before their final offsets are known; shards describe what to emit after those offsets are computed.
The builder's temporary native pointers connect its construction objects and are separate from references encoded into the output.

## Dependencies

- [Scalar Types](../base-types/scalar-types/scalar-types.md) supplies byte, size, index, and fixed-width integer conventions.
- [Heap Storage](../memory/heap-storage/heap-storage.md) supplies the matching reallocation and release operations for `OffsetContainer`'s single buffer.
  [Arenas](../memory/arenas/arenas.md) supplies stable construction storage for chunks, shards, and copied shard payloads.
- [Collections](../collections/collections.md) supplies intrusive ordering of chunks and shards and the byte list used when transferring completed output.
  The builder, rather than the intrusive list, owns construction storage through its arena.
- [Text's Strings](../text/strings/strings.md) supplies the borrowed character ranges consumed and returned by `OffsetString` operations.
  Binary Layouts owns that stored string's length encoding and terminator; Text owns the slice abstraction.
- [Host I/O](../host-services/host-io/host-io.md) supplies byte streams and the owned memory stream used to collect output.
  Binary Layouts uses `Stream::write`, `OwnedMemoryStream`, and its byte-list transfer, rather than path or host-file operations from the broad legacy I/O include.
- [Blobs](../blobs/blobs.md) supplies the list-backed implementation that takes the completed byte list.
- The product headers supply `ISlangBlob`, `ComPtr::detach` for transferring the completed blob's product reference, and the compiler annotations `SLANG_FORCE_INLINE` and `SLANG_MAYBE_UNUSED`.
- [Failure Reporting](../failure-reporting/failure-reporting.md) supplies assertions for representable displacements, alignment and construction preconditions, release assertions for selected offset access checks, and signals for unsupported shard kinds or widths.
- The host C and C++ environment supplies native address arithmetic, fixed-width integer representations, byte copying and clearing, placement construction, and the maximum operation used to increase alignment.

The aggregate `slang-basic.h` and `slang-io.h` includes are not evidence of additional service use.
Compiler serialization and reproduction consume these services; their formats and validation do not become dependencies of Binary Layouts.

## Decomposition

The following proposed children are public ordinary modules within Foundation.
Compiler serialization uses self-relative pointers and deferred composition, and compiler reproduction uses buffer-relative storage.
Public visibility has the implementation-only meaning defined by the [root document](../../slang.md#concepts).

| Child                                                       | Service                                                                                                                     |
| ----------------------------------------------------------- | --------------------------------------------------------------------------------------------------------------------------- |
| [Relative Pointers](relative-pointers/relative-pointers.md) | Address-independent pointer fields resolved relative to each field's own address.                                           |
| [Offset Containers](offset-containers/offset-containers.md) | Construction and access of contiguous storage addressed through an explicit buffer base.                                    |
| [Blob Composition](blob-composition/blob-composition.md)    | Deferred arrangement and emission of aligned chunks with copied data, relative references, prefixes, and total-size fields. |

Blob Composition emits the self-relative displacement convention used by Relative Pointers, but its implementation computes integer fields without constructing a `RelativePtr` object.
Offset Containers uses a different base and unsigned displacement convention; the two pointer families are not interchangeable.
`BlobBuilder`, `ChunkBuilder`, and `ShardBuilder` remain together because their arena, linked ordering, cached offsets, and emission operations implement one construction service.
`ShardBuilder` remains implementation support rather than a public child service, despite its declaration being present in the legacy header.

## Existing Contracts and Client Boundaries

### Self-Relative Fields and Deferred Output

`RelativePtr::set` computes the target address minus the field address and asserts that the selected signed offset type can represent the result.
`get` adds the stored displacement to the field address without a buffer extent or target-alignment check.
Copy and move construction and assignment resolve the old pointer and recompute a displacement for the destination field.
Copying only the encoded field bytes instead preserves the displacement and generally changes the resolved target.
Relocating the field and target together by the same amount preserves a stored displacement, provided the containing format's other fields also support relocation.
Neither reference family retains its target or its backing buffer.

`BlobBuilder::writeTo` first computes each included chunk's content offset, then emits prefixes, content, and zero padding in list order.
For each chunk, layout adds the prefix size before rounding up to the content alignment, so the prefix can occupy bytes that would otherwise be padding.
The cached offset identifies the content, not the prefix.
`ShardBuilder::_writeTo` uses the pointer field's own emitted byte offset when subtracting it from the target chunk's content offset.
Null content pointers are emitted as zero bytes; prefix-relative pointers require a non-null target.
Relative-pointer and total-size fields support four-byte and eight-byte output representations.
Total-size fields do not automatically align their containing chunk, while relative-pointer fields pad and raise content alignment to their field width.
`addPrefixData` also raises content alignment to the prefix byte count, which must satisfy the existing power-of-two alignment precondition.
Raw data operations copy their input during the call without adjusting its representation, so clients supply bytes that can be copied unchanged and contain no address-dependent fields requiring a deferred reference.

For example, the `Fossil::SerialWriter` constructor taking a `BlobBuilder&` adds a chunk and passes it to `_initialize`.
`_initialize` uses that chunk as the header and inserts a root-value chunk immediately afterward.
It copies the header magic and flags, requests a deferred total-size field, and requests a self-relative pointer to the root-value chunk.
The writer later supplies raw values, string/count prefixes, and layout references through the same construction service.
`writeSerializedModuleIR` finishes and destroys that writer before calling `BlobBuilder::writeToBlob`, because the writer's destructor flushes deferred serialization work.
`Fossil::Header::rootValue` is read through `FossilizedPtr`, derived from `RelativePtr32`, so the emitted displacement and the reader's field-relative interpretation must agree.
Fossil's header, layouts, object traversal, and validation remain Compiler's [Module Storage](../../compiler/module-storage/module-storage.md) concern.
The separate `RIFF::ChunkBuilder` and RIFF tree operations remain with [RIFF Data](../riff-data/riff-data.md); the similar builder names do not identify the same class or construction service.

### Buffer-Relative Storage and Validation

`OffsetBase` resolves a displacement against its current `m_data` and `m_dataSize`; `MemoryOffsetBase::set` borrows a supplied range without retaining or validating its contents.
Pointer access returns null for zero, absent backing storage, or a fixed-size object range outside that extent.
Reference access additionally uses a release assertion before forming a C++ reference from the resolved address.
Array indexing has a release assertion for a nonnegative index whose conversion to `uint32_t` is below the stored count.
These operations do not validate target alignment, a complete array range, variable-length string payloads, or the graph reachable through record fields.
`getOffset` checks a non-null address against the current buffer with an ordinary assertion and narrows the displacement to 32 bits; the raw offset constructors and arithmetic are not a checked deserialization interface.
`getFirst` supplies the conventional address at the reserved eight-byte offset when the used size reaches eight bytes; it does not establish that any complete root object exists there.

`OffsetContainer` grows one `StandardAllocator` buffer and rounds allocation offsets to a requested nonzero power of two.
It rejects alignment or size arithmetic outside the 32-bit byte domain and returns null on a failed reallocation before changing the recorded buffer state.
The initial reserved-byte allocation is different: failure triggers a release assertion because offset zero must remain unavailable to objects.
`newObject` value-constructs one object, and `newArray` value-constructs its elements after checking the count and multiplication.
Those helpers return a null pointer or empty array on their reported allocation failures; an empty array is also the result for a zero count.
Container destruction releases the raw buffer without running destructors for objects constructed inside it.
Clients therefore supply types whose representations remain valid after buffer relocation and whose lifetime requirements match bulk storage release.
Allocation can invalidate every ordinary pointer or reference previously obtained from the container, including borrowed string characters.
Clients keep offsets across allocation and resolve them again afterward.
An input character range passed to `newString` must remain valid during allocation and the subsequent copy; a borrowed range into the same growing container does not generally satisfy that requirement.

For example, `StoreContext::addFile` allocates a `ReproUtil::FileState`, then separately allocates its identity string before resolving the file offset again to assign `uniqueIdentity`.
`ReproUtil::saveState` stores the container bytes in a RIFF data chunk after its own version and type-hash header.
The loading path unwraps that chunk and runs `isReproStateValid` on the copied payload before returning it in a `ListBlob`.
That compiler validator checks record and array ranges and alignment, complete encoded string lengths and terminators, and reproduction-specific relationships.
For example, `spLoadReproAsFileSystem` keeps the returned blob alive while configuring a borrowed `MemoryOffsetBase` over its bytes and calling `ReproUtil::loadFileSystem`.
Reproduction state, compatibility decisions, validation, and reconstruction remain Compiler's [Reproduction](../../compiler/reproduction/reproduction.md) concern.

`OffsetString` stores a byte length, the characters, and a zero terminator excluded from that length.
Within its 32-bit length domain, lengths through 251 use one byte; larger lengths use a leading byte describing one to four following little-endian length bytes.
`newString(const char*)` maps null input to a null offset, whereas a non-null empty string is allocated with its own length and terminator.
The decoder and character accessors take no extent, so their caller must first establish that the length encoding, payload, and terminator fit.
This length encoding does not validate a character encoding or make the surrounding native records endian-independent.

## Existing Implementation Mapping

Paths below are relative to `old-source/core/`.
The assignments cover the complete declarations and implementations in all six files, including inline operations and support definitions.
They propose ownership without moving code or splitting any class's members among owners.

| Existing Implementation      | Proposed Owner and Responsibility                                                                                                                                                                                                                                                                                  |
| ---------------------------- | ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------ |
| `slang-relative-ptr.h`       | Relative Pointers: complete `detail::RelativePtr32Traits`, `detail::RelativePtr64Traits`, `RelativePtr`, and the `RelativePtr32` and `RelativePtr64` alias templates.                                                                                                                                              |
| `slang-relative-ptr.cpp`     | Relative Pointers: existing include-only translation unit; there is no out-of-line pointer implementation to relocate separately.                                                                                                                                                                                  |
| `slang-offset-container.h`   | Offset Containers: `kNull32Offset`, `kStartOffset`, `kMax32Offset`, and complete `Offset32Ptr`, `Offset32Ref`, `Offset32Array`, `OffsetString`, `OffsetBase`, `MemoryOffsetBase`, and `OffsetContainer`, including templates, fields, comparisons, arithmetic, conversions, allocation helpers, and access checks. |
| `slang-offset-container.cpp` | Offset Containers: complete string-length encoding/decoding and character views, container startup and destruction, aligned allocation and growth, zeroing, alignment padding requests, and string allocation.                                                                                                     |
| `slang-blob-builder.h`       | Blob Composition: complete `BlobBuilder`, `ChunkBuilder`, and `ShardBuilder`, with shard kinds, linked-node inheritance, templates, fields, and the `isPowerOfTwo` and `roundUpToAlignment` support functions.                                                                                                     |
| `slang-blob-builder.cpp`     | Blob Composition: complete two-pass layout/emission, output-blob transfer, chunk creation and ordering, alignment/padding, copied-data coalescing, prefix and deferred-field construction, and movement of contents between chunks.                                                                                |

The alignment support functions remain part of the existing composition interface in this assignment.
`Fossil::SerialWriter::_getOrCreateChunkForLayout` also calls `roundUpToAlignment` to compute an element stride; that client use must remain available during migration.
`UnownedStringSlice`, `StandardAllocator`, `MemoryArena`, `InternallyLinkedList`, `Stream`, `OwnedMemoryStream`, `ListBlob`, and the product interfaces retain their respective owners.
No portion of a Compiler serializer, reproduction schema, or validator is assigned to these children.

## Preservation Constraints

The existing native record layout and integer representation are part of the byte formats used by clients.
Blob Composition copies raw payload bytes and emits native `Int32`, `Int64`, `UInt32`, and `UInt64` fields without endian conversion.
`OffsetString`'s explicit little-endian extended length does not change the native representation of other offset-container fields.
Fixed-width pointer fields alone therefore do not establish a portable schema across arbitrary hosts or C++ layouts.
Repro's type-size hash and Fossil's layouts and validation belong to their clients and are not replaced by a common serialization API.

The current operations also have distinct limits that migration must preserve:

- `OffsetContainer` aligns displacements within an ordinary reallocatable allocation; it does not establish support for arbitrary over-aligned native objects.
  Alignment gaps are not initialized by ordinary allocation; `allocateAndZero` clears only its requested payload, and `fixAlignment` discards the allocation result.
- Blob Composition requires valid power-of-two alignments, representable size arithmetic and relative displacements, and target chunks that receive offsets in the same completed output.
  `createUnparentedChunk` still allocates in the creating blob's arena; the chunk becomes part of the output only through insertion or transfer of its contents into an included chunk.
  The implementation does not comprehensively check chunk ownership, repeated insertion, inclusion of a pointer target, or relative-offset narrowing.
  Unsupported non-null pointer widths signal during emission; the null-content-pointer path instead writes the requested zero bytes after its existing size/alignment assertions.
- `addContentsOf` transfers the donor's shards and prefix and resets its content size and alignment.
  A donor prefix requires an otherwise empty destination, and the operation does not redirect existing references to the donor chunk toward the destination.
  Construction objects and payloads remain in their allocating blob's arena; neither linked-list movement nor retaining a raw chunk pointer extends that lifetime.
- `BlobBuilder::writeTo` borrows the supplied stream, writes at its current position, and neither seeks nor closes it.
  Emitted offsets and total-size fields describe the builder's sequence, independently of any bytes preceding that sequence in the stream.
  The writer discards `Stream::write` results, and its byte-count assertion compares computed layout counts rather than the stream's accepted byte count.
  `writeToBlob` uses a fresh writable memory stream, transfers its byte list into `ListBlob`, and detaches the product reference to the caller; it does not introduce a result-returning failure protocol.

`OffsetBase&` occurs in existing `SLANG_API` reproduction helper signatures used by companion modules.
The [root's staged migration contracts](../../slang.md#staged-migration-contracts) require preservation of canonical C++ identities where they cross such binary interfaces.
All existing names, signatures, class layouts and member order, alias-template identities, inline choices, and behavior remain the preservation boundary; namespace organization must not create replacement types.
The present `core` source glob compiles the three translation units, and enabled `slang-rt` separately compiles the same legacy core directory with runtime export settings.
Both construction consumers must remain accounted for before any code transfer.

No shared-class split or new service cycle is required by the complete-class assignments above.
The [Foundation failure/text/native-ownership cycles](../foundation.md#failure-reporting-and-basic-data-services) still affect their dependencies, and the [root artifact/source-reuse questions](../../slang.md#unresolved-architectural-dependencies) remain open.
The proposed children need their own refinement and review before migration; phase one selects no replacement offset-pointer interface, stronger decoder, changed allocation policy, or new emission error protocol.
