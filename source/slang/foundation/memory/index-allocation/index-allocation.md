<!--
SPDX-FileCopyrightText: The Khronos Group, Inc.
SPDX-License-Identifier: CC-BY-4.0
-->

# Index Allocation

Index Allocation reserves and returns contiguous integer ranges without owning the storage indexed by them.
A client can reserve three descriptor positions together, for example, while keeping the actual descriptors in a device heap.
The service tracks available positions and reuses returned ranges; the client chooses what the positions represent and when they can be reused.

This is a public ordinary leaf of [Memory](../memory.md#decomposition), within the existing Foundation artifact.
Its implementation remains in `old-source/core/slang-virtual-object-pool.h`.
The assignment includes the complete `VirtualObjectPool` and its bookkeeping nodes, rather than the compiler containers or graphics objects that use its indices.

## Concepts

Memory defines an [index allocation](../memory.md#concepts), and the [root document](../../../slang.md#concepts) defines implementation modules, dependencies, and visibility.
Public visibility here allows clients outside Memory to use the service; it does not establish a product API in `include/`.

An **available range** is a contiguous run of positions that the pool can reserve.
We describe a run by the half-open interval `[offset, offset + size)`, where offsets and sizes count positions, not bytes.
A **free-list node** records one available range and links to its predecessor and successor.
The pool owns these native C++ nodes; it does not own any client storage represented by the positions.

Starting from a positive initialized extent, positive allocations and valid returns preserve nonoverlapping available ranges ordered by offset, provided that integer arithmetic is representable and the links remain intact.
Returning a range joins it to either neighboring range when their endpoints meet.
That representation supports reserving a contiguous run without moving any client objects or compacting existing reservations.
The implementation relies on callers to maintain the range conditions; the [limitations below](#unchecked-inputs-and-failure-behavior) distinguish them from validation performed by the pool.

## Dependencies

The host C++ environment supplies built-in `int`, pointer operations, and ordinary `new` and `delete` for free-list nodes.
The complete header has no includes and uses no project scalar aliases, allocator wrappers, collections, assertions, or product declarations.
In particular, bookkeeping allocation does not call Memory's `StandardAllocator`, and the leaf does not itself depend on Failure Reporting.

The compiler and graphics wrappers use collections, failure reporting, reference ownership, and graphics APIs for their own work.
Those uses do not create reverse dependencies from Index Allocation to its clients or to the services selected by those clients.
The leaf's narrow dependency does not resolve [Memory's inherited failure-reporting cycle](../memory.md#unresolved-boundaries) or the [root's artifact questions](../../../slang.md#unresolved-architectural-dependencies).

## Principal Service Areas

The module remains one leaf because range reservation, return, coalescing, and node lifetime all operate on the same linked representation.
There are no proposed child modules or additional build artifacts.

| Service Area           | Existing Operations                                                     | Responsibility                                                                                               |
| ---------------------- | ----------------------------------------------------------------------- | ------------------------------------------------------------------------------------------------------------ |
| Pool and node lifetime | Default initialization, `initPool`, `destroy`, and `~VirtualObjectPool` | Establish available positions and release the bookkeeping nodes.                                             |
| Contiguous reservation | `alloc`                                                                 | Find the first available range large enough and consume positions from its beginning.                        |
| Return and coalescing  | `free`                                                                  | Insert the supplied range and join exactly adjacent neighbors.                                               |
| Exposed representation | `FreeListNode` and `freeListHead`                                       | Retain the existing public node fields and head pointer, including their mutation and lifetime implications. |

Consider this sequence with eight positions:

```cpp
VirtualObjectPool pool;
pool.initPool(8);
int first = pool.alloc(3);  // Returns 0, reserving [0, 3).
int second = pool.alloc(2); // Returns 3, reserving [3, 5).
pool.free(first, 3);
pool.free(second, 2);
```

After both allocations, the remaining available range is `[5, 8)`.
Returning the first range puts `[0, 3)` before that remainder.
Returning the second range joins its predecessor and then its successor, leaving one range `[0, 8)`.
The caller's objects or descriptors remain in their original storage throughout the sequence.

### Pool and Node Lifetime

A default-initialized `VirtualObjectPool` has a null `freeListHead`.
`initPool(numElements)` allocates one node, sets its offset to zero and its length to exactly `numElements`, and sets both links to null.
The pool stores no independent total capacity or initialized flag.
An empty free list can therefore mean that initialization has not occurred, that all positions have been reserved, or that `destroy` has run.

`initPool` directly replaces the head pointer without deleting an existing list.
The previous nodes are no longer reachable through this pool, but a retained head pointer or copied pool can still refer to them.
If nothing retains and eventually reclaims the old chain, its nodes leak.
The name does not provide a reset-and-release guarantee.
A structural port must retain this behavior unless a separate change authorizes different initialization semantics.

`destroy` follows the forward links, deletes each node, and nulls the head.
Calling it again on an empty list performs no deletions, and the destructor calls the same operation.
The pool does not check that every reservation has been returned before destruction.
Destroying the bookkeeping nodes does not destroy client objects or release their backing storage.
The absence of a reservation ledger means that destruction cannot enumerate outstanding reservations.

The class declares a destructor but no copy constructor, copy assignment, or move operations.
Its implicit copies copy the head pointer, sharing the same node chain rather than cloning or transferring it.
Mutating or independently destroying such copies can leave dangling pointers or delete nodes twice, and copying over a live destination can lose its previous chain.
The destructor suppresses implicit move generation; an rvalue can still be copied.
This refinement records the existing limitation rather than introducing deleted operations, an ownership wrapper, or a new copy policy.

`FreeListNode::Offset`, `Length`, `prev`, `next`, and `freeListHead` are public mutable fields.
Compiler `ObjectPool<T>`, D3D12 general descriptor heaps, and D3D12 queue-index allocation use the methods rather than editing the links.
The representation nevertheless remains exposed, and the implementation does not validate arbitrary external changes to it.
Reserving a whole available range deletes its node; returning a range can delete nodes during coalescing, and `destroy` deletes the remaining chain.
Node addresses therefore do not provide stable identities for reservations.
The implementation provides no internal synchronization for shared access.

### Contiguous Reservation

`alloc(size)` scans from the head for the first node whose length is at least `size`.
For the ordered positive ranges described above, that is the lowest-offset available range that fits, rather than a best-fit selection.
The operation returns the old offset, advances the node's offset by `size`, and subtracts `size` from its length.
If the remainder has zero length, it reconnects the neighboring nodes, updates the head when necessary, and deletes the exhausted node.

A null head or a scan that finds no fit returns `-1` without changing the list.
This can occur even when the sum of available positions is sufficient, because the request needs one contiguous run.
`alloc` neither creates bookkeeping nodes nor grows backing storage, moves live data, or applies byte alignment.
The search is linear in the number of free-list nodes.

### Return and Coalescing

`free(offset, size)` records exactly the supplied range.
If the list is empty, the operation creates a standalone head node, even if `initPool` has never been called.
Otherwise it scans while each visited node's offset is less than `offset + size`, then inserts a newly allocated node between the last visited node and its successor.
For a valid nonoverlapping return, that search places the returned range in offset order.

The operation first merges with the predecessor when the predecessor ends exactly at the returned offset.
It then merges with the successor when the combined range ends exactly at the successor's offset.
These are adjacency checks, not overlap checks.
Link updates preserve the head and both link directions for valid input.
Even a return that immediately coalesces creates a node before the merges delete the redundant nodes.
The search is linear in the number of free-list nodes, and only the immediate neighbors are considered for merging.

Returning positions does not clear or destroy client data.
The caller supplies the offset and length and decides whether all users of that range have finished.
`free` returns no status and does not verify that the range was previously reserved, belongs to an initialized extent, or has not already been returned.

### Unchecked Inputs and Failure Behavior

All counts, offsets, and lengths in the pool are signed `int` values.
The useful range interpretation depends on positive sizes, valid nonoverlapping returns, and representable arithmetic.
The implementation does not enforce those conditions, and this document does not add input rejection to a future structural port.
There is no saved capacity against which to check a returned endpoint.

Zero and negative sizes are not rejected.
For example, `alloc(0)` against a positive-length head returns its offset without consuming positions, while a selected negative allocation decreases the offset and increases the remaining length.
`initPool(0)` still creates a zero-length node; a positive allocation then returns `-1`.
`free` likewise stores zero or negative lengths without validation, and overlapping or repeated returns can break the available-range interpretation.
The integer expressions, including `offset + size`, have no overflow checks; the implementation establishes no portable behavior for signed overflow.

The `-1` result reports failure to find a fitting range, not failure to allocate bookkeeping memory.
`initPool` and `free` use ordinary C++ `new` without translating its failure into that sentinel, a `SlangResult`, or a Foundation signal.
Client wrappers choose their own exhaustion reporting and do not establish an allocation-failure recovery contract for this leaf.
These limitations remain inherited behavior; the refinement does not fix or conceal them.

## Existing Implementation Mapping

| Existing Implementation                                | Assignment                                                                                                                                                                       |
| ------------------------------------------------------ | -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| Complete `old-source/core/slang-virtual-object-pool.h` | Index Allocation owns `VirtualObjectPool`, its nested `FreeListNode`, every field, all inline operations, and bookkeeping-node creation, relinking, coalescing, and destruction. |

The header contains no unrelated declarations requiring a partial-file assignment.
The complete mapping includes the exposed representation and implicit-copy limitations, rather than only the allocation and return entry points.
The following implementations remain clients and are excluded from the assignment:

| Existing Client                                                                     | Client-Owned Responsibility                                                                                                                                                    |
| ----------------------------------------------------------------------------------- | ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------ |
| `old-source/slang/slang-container-pool.h`                                           | `ObjectPool<T>` owns a separate `List<T>`, element-address/index conversion, and exhaustion reporting; `ContainerPool` owns clearing and container-capacity retirement policy. |
| `tools/gfx/d3d12/d3d12-descriptor-heap.h` and `.cpp`                                | D3D12 heap creation, descriptor-handle arithmetic, subheap growth and index translation, reference ownership, and graphics failure reporting.                                  |
| `tools/gfx/d3d12/d3d12-device.h`, `d3d12-device.cpp`, and `d3d12-command-queue.cpp` | The 32-position queue-index pool, D3D queue creation and destruction, waiting, and returning queue indices at the end of the queue's lifetime.                                 |

`ObjectPool<T>` calls `initPool(maxElementCount)` and separately sets its list's element count.
`getObject` reserves one position and returns the address of that existing list element; exhaustion calls `SLANG_UNEXPECTED`.
`freeObject` returns one position without destroying or reconstructing the element.
The surrounding `ContainerPool` chooses how to clear or release retained container storage before returning the position.

`D3D12GeneralDescriptorHeap` initializes the actual device heap before initializing its index allocator.
Its integer allocation and return operations forward range requests, and its descriptor overloads convert between indices and CPU descriptor handles.
`D3D12GeneralExpandingDescriptorHeap` decides when to create subheaps and converts between local and combined indices.
The linear heap classes in the same header use allocation cursors rather than `VirtualObjectPool`; their presence in that file does not assign them to Index Allocation.

`DeviceImpl` uses a separate `VirtualObjectPool` for 32 command-queue indices.
`createCommandQueueImpl` reserves one index and returns `SLANG_FAIL` on exhaustion; `CommandQueueImpl` returns the index during destruction after its graphics cleanup.
The chosen queue limit and lifetime policy are graphics responsibilities, just as object lifetime is the compiler client's responsibility.

## Build and Preservation Boundaries

The implementation is header-only and has no product export annotation or separate translation unit.
`old-source/core/CMakeLists.txt` registers it through `slang_add_target` and the recursive header selection in `cmake/Glob.cmake`.
Enabled `slang-rt` also registers legacy core sources through `EXTRA_SOURCE_DIRS`, under its separate runtime export settings.
Header registration does not establish that a runtime translation unit instantiates this pool or that an out-of-line pool symbol is provided by either library.

The compiler includes the pool through `slang-container-pool.h`, which `slang-ir.h` includes.
The optional `gfx` target includes it through its D3D12 implementation, subject to the existing backend source-selection rules.
Both clients obtain the legacy include path through the existing `core` target configuration.
A later move must preserve those include consumers and both core/runtime source-registration roles without inventing a library for this ordinary module.

The [root's staged preservation contracts](../../../slang.md#staged-migration-contracts) and [Foundation's unresolved boundaries](../../foundation.md#unresolved-boundaries) continue to apply.
This leaf assigns ownership and describes current behavior; it does not select a namespace or ABI change, alter public fields or signatures, impose new input bans, or establish a code port.
Its absence of direct Foundation service dependencies does not lift the inherited ancestor holds.
