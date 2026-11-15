<!--
SPDX-FileCopyrightText: The Khronos Group, Inc.
SPDX-License-Identifier: CC-BY-4.0
-->

# Memory

Memory provides byte storage, bulk allocation and reuse, native C++ reference ownership, and allocation of contiguous index ranges in caller-managed storage.
Clients choose the lifetime of their data; Memory supplies the mechanisms for retaining objects or reclaiming storage at that lifetime's end.

The implementation remains in `old-source/core/`.
This document proposes child boundaries within [Foundation's assignment](../foundation.md#existing-implementation-mapping); the [failure-reporting cycle](#unresolved-boundaries) remains unresolved and prevents treating the decomposition as ready for migration.

## Concepts

The [root document](../../slang.md#concepts) defines modules, dependencies, and visibility.
[Foundation](../foundation.md#concepts) defines owned allocations, borrowed views, and intrusive references.
Those concepts also apply when the client receives an ordinary pointer: the pointer alone does not identify who owns its storage or extend its lifetime.

- An **allocation family** pairs operations that obtain storage with the operations permitted to resize or release that storage.
  `StandardAllocator`, `AlignedAllocator`, and C++ `new`/`delete` are distinct families; a buffer cannot be transferred between them merely because each operation uses a `void*`.
- An **arena** supplies byte allocations whose storage can be reclaimed together or rewound to an earlier allocation cursor.
  The arena owns backing storage, rather than tracking the construction and destruction of objects placed in that storage.
- A **fixed-size pool** supplies slots of one configured size, with individual return and reuse of slots as well as bulk reclamation.
  Returning a slot does not run a client object's destructor.
- An **index allocation** reserves a contiguous run of integer positions in a caller-specified range.
  The caller supplies any corresponding objects, bytes, or device descriptors; reserving positions does not allocate that backing storage.

## Dependencies

- [Scalar Types](../base-types/scalar-types/scalar-types.md) supplies the integer conventions used for counts and indices.
  [C++ Utilities](../base-types/cpp-utilities/cpp-utilities.md) supplies type relations, swap, compiler annotations, and runtime visibility declarations.
- [Hashing](../hashing/hashing.md) supplies address hashing for native smart-pointer keys.
  Equality and hashing use the pointed-to address, including agreement with an ordinary pointer used for heterogeneous collection lookup.
- [Failure Reporting](../failure-reporting/failure-reporting.md) supplies reference-count, allocation-size, alignment, cursor, and capacity checks.
  These checks occur in native ownership, arenas, fixed-size pools, and `ScopedAllocation`; the allocator primitives do not themselves report a Foundation signal.
- The product headers supply the platform and C++ compiler selection macros used by allocation, casting, and annotation branches.
  Those headers remain external contracts rather than implementation modules owned by Memory.
- The host C and C++ environment supplies allocation and release, byte copying and clearing, type traits, placement construction, atomic reference counts, and the default RTTI-based native casts.
  Aligned allocation selects the existing Windows, Cygwin, or POSIX operation for the host.
- Optional `mimalloc`, available through [Foundation](../foundation.md#dependencies), replaces the allocation operations selected by `SLANG_ENABLE_MIMALLOC` in the allocator header.
  Its compile definitions and link dependency must remain consistent with the clients that instantiate that header.

The `ScopedAllocation` class currently resides in a header that also includes text, collections, blobs, and COM support.
Its operations use none of those services, so extracting that class does not create a Memory dependency on them.
Likewise, array allocation invokes constructors and destructors supplied by its element type; the client's choice of element type does not make Memory depend on every service used by that type.

## Decomposition

The proposed children separate storage allocation, storage reclamation, object retention, and reservation of positions in storage owned elsewhere.
They are ordinary modules within the existing Foundation artifact.

| Child                                                     | Visibility | Service                                                                                                        |
| --------------------------------------------------------- | ---------- | -------------------------------------------------------------------------------------------------------------- |
| [Heap Storage](heap-storage/heap-storage.md)              | Public     | Allocator-matched byte storage, scoped buffer ownership, and typed array allocation and destruction.           |
| [Native Ownership](native-ownership/native-ownership.md)  | Public     | Intrusive native reference ownership, pointer casts and keys, and explicitly managed non-retaining references. |
| [Arenas](arenas/arenas.md)                                | Public     | Byte allocation with bulk reclamation, cursor rewind, and reuse of backing storage.                            |
| [Fixed-Size Pools](_fixed-size-pools/fixed-size-pools.md) | Private    | Allocation and reuse of equal-sized raw storage slots.                                                         |
| [Index Allocation](index-allocation/index-allocation.md)  | Public     | Reservation and return of contiguous integer ranges without owning the storage indexed by them.                |

Heap Storage serves collection buffers, blobs, and whole-file reads.
Native Ownership serves strings, object interfaces, and compiler objects.
Arenas serves compiler builders, text storage, and binary construction.
Index Allocation serves the compiler's container pool and graphics descriptor heaps.
These independent clients justify public service access.

Fixed-Size Pools is private because its existing production use is `MemoryArena`'s allocation of block metadata.
The direct `FreeList` unit test exercises that implementation; no production client outside Memory currently uses the service.
Arenas uses both Heap Storage for payloads and Fixed-Size Pools for metadata; Fixed-Size Pools obtains its backing blocks from Heap Storage.
Native Ownership releases last-referenced objects through C++ `delete`, and Index Allocation uses C++ `new` and `delete` for its bookkeeping.
Their implementations do not call `StandardAllocator`.
Each child still requires its own refinement; these boundaries do not select its internal structure or change the existing C++ layouts.

## Lifetime and Client Contracts

### Allocator-Matched Storage

`StandardAllocator` uses the configured C allocator or `mimalloc`; `AlignedAllocator` has its own matching release operation, including `_aligned_free` on Windows.
`ScopedAllocation::attach` accepts only storage compatible with `StandardAllocator`, and `detach` transfers responsibility for releasing that storage to the caller.
For example, `RawBlob::moveCreate` swaps a `ScopedAllocation` into the blob without copying its bytes, whereas Compression copies a miniz-owned buffer before releasing the original through miniz.
The blob and compression policies remain with their respective modules.

Typed array allocation differs from arena allocation: `AllocateMethod` constructs and destroys nontrivial elements across the count supplied by the caller.
`List` supplies its buffer capacity, including elements beyond its logical count.
Reclamation must preserve that distinction.
The existing allocation APIs have different failure behavior; this decomposition does not introduce a shared allocation-failure policy or promise that every wrapper can recover from a null allocation.

### Native Object Lifetimes

`RefObject` starts with no references, and copying the base initializes a new zero reference count rather than sharing the old count.
`RefPtr` retains the pointed-to object; ordinary pointer access through `get`, `Ptr`, or conversion does not add a reference.
`attach` and `detach` transfer an existing reference without incrementing it.
`releaseReference` deletes the object on the last-reference transition, whereas `decreaseReference` only decrements the count.
Output Writers uses the latter operation for static writers that must survive their reference count reaching zero.

The atomic count does not synchronize the object's contents or make an unretained pointer safe to use after another owner releases the object.
Clients retain a separate `WeakSink` object without retaining its target: the target's owner must explicitly detach the sink, and clients must check the returned pointer.
For example, the OpenGL device detaches its sink during destruction, and buffer destruction checks the sink before calling the device.
`TransformablePtr` instead stores a raw pointer together with an optional strong reference; demotion removes the strong reference without arranging automatic nulling of the raw pointer.
Reflection uses that operation when constructing an offset variable layout, leaving the surrounding layout ownership responsible for the referenced type layout's lifetime.

### Reclaiming Storage Without Destroying Objects

Arena and pool reclamation concerns raw storage.
Typed arena helpers do not invoke constructors; the arena placement-`new` overloads allow C++ construction, but arena rewind, reset, and destruction do not invoke the resulting objects' destructors.
For example, `ASTBuilder` records nodes with nontrivial destructors and calls those destructors before its arena member releases storage.
That compiler-specific destruction policy remains with Compiler.

Arena allocations remain at their addresses until the client reclaims the corresponding storage or destroys its owner.
Cursor rewind invalidates the allocations being discarded, even if the arena retains their backing blocks for later reuse.
The IR builder uses that distinction to discard a temporary lookup instruction when an equivalent instruction already exists.
The empty-string arena helper is an exception to arena-owned storage: it returns the static empty string.
`MemoryArena::addExternalBlock` records a `StandardAllocator` allocation for later deallocation; it does not borrow arbitrary client memory.
If allocating block metadata fails, the existing operation returns without recording or releasing the buffer, and its `void` interface does not report that outcome.
A structural port must preserve that limitation rather than promising that every call transfers ownership successfully.

Index Allocation only tracks which positions are available.
Its caller owns construction, destruction, storage lifetime, and the validity of ranges supplied to `free`.
`VirtualObjectPool::alloc` returns `-1` when no free run fits; its free-list nodes themselves use C++ allocation.
The compiler's `ObjectPool` keeps its objects in a separate `List`, and D3D12 clients keep their descriptors in a device heap.

## Existing Implementation Mapping

The following paths are relative to `old-source/core/`.
The assignment covers each file's complete declarations and definitions except for the explicitly partial blob header.

| Existing Implementation                             | Proposed Owner and Responsibility                                                                                                                                        |
| --------------------------------------------------- | ------------------------------------------------------------------------------------------------------------------------------------------------------------------------ |
| `slang-allocator.h`                                 | Heap Storage: `StandardAllocator`, `AlignedAllocator`, and `AllocateMethod`, including platform and allocator selection and typed element lifetime operations.           |
| `slang-smart-pointer.h`                             | Native Ownership: `RefObject`, nullable reference helpers, `dynamicCast` and `as`, `RefPtr`, `WeakSink`, and `TransformablePtr`, including pointer equality and hashing. |
| `slang-memory-arena.h` and `slang-memory-arena.cpp` | Arenas: the complete `MemoryArena`, inline and typed helpers, and global arena placement allocation and matching placement-delete overloads.                             |
| `slang-free-list.h` and `slang-free-list.cpp`       | Fixed-Size Pools: the complete `FreeList`, its element/block bookkeeping, and its memory-initialization macros and operations.                                           |
| Only `ScopedAllocation` in `slang-blob.h`           | Heap Storage: scoped buffer allocation, resizing, copying, ownership transfer, size/capacity bookkeeping, and optional byte termination.                                 |
| `slang-virtual-object-pool.h`                       | Index Allocation: the complete `VirtualObjectPool`, including free-range bookkeeping and node lifetime.                                                                  |

The other declarations in `slang-blob.h` remain with [Blobs](../blobs/blobs.md).
The COM implementations in `slang-com-object.h` remain with [Object Interfaces](../object-interfaces/object-interfaces.md), even when they bridge product reference counting to `RefObject`.
The product smart pointers and interface helpers in `include/` remain external contracts; native `RefPtr` ownership does not assign them to Memory.
Compiler-specific pools, slice allocators, destructor registries, AST/IR construction, and reflection ownership remain clients of Memory rather than additional responsibilities of this module.

The current build compiles the arena and free-list implementations into static `core` and separately into enabled `slang-rt` under runtime export settings.
The [root's staged migration contracts](../../slang.md#staged-migration-contracts) apply to both consumers.
In particular, `RefObject` and `RefPtr` carry `SLANG_RT_API`, and Text's similarly annotated `StringRepresentation` derives from `RefObject`.
Migration must preserve their canonical `Slang` type identities, layouts, signatures, and visibility, as well as allocator pairing across existing ownership transfers.
Reexporting those same declarations is distinct from moving their definitions to a new namespace.

## Unresolved Boundaries

Native Ownership depends on Failure Reporting for the reference-count checks in `RefObject::releaseReference` and `isUniquelyReferenced`.
Failure Reporting formats and retains messages through Text's `String`, whose `StringRepresentation` derives from `RefObject` and is retained through `RefPtr`.
The resulting Native Ownership to Failure Reporting to Text to Native Ownership cycle remains even after separating allocation, pool, and arena headers.
An ordinary assertion's release-build optimizer-assumption behavior does not remove its debug-build service dependency.

[Foundation's unresolved boundary](../foundation.md#failure-reporting-and-basic-data-services) requires a coherent ownership decision or behavior-preserving separation before the affected services can migrate.
This refinement neither replaces the failure mechanism nor changes reference-count checks, message ownership, exception behavior, or client destruction policy.
Heap Storage, Arenas, and Fixed-Size Pools have their own failure-reporting uses.
All child modules still require refinement of their dependencies and inherited allocation contracts.
No complete acyclic dependency graph or Memory code port is established by this document.
