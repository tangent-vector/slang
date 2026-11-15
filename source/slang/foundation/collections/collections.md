<!--
SPDX-FileCopyrightText: The Khronos Group, Inc.
SPDX-License-Identifier: CC-BY-4.0
-->

# Collections

Collections provides storage and traversal for sequences, lookup by key, and membership and set operations.
Clients choose whether they need owned element storage, a borrowed view of existing elements, or ordering through links embedded in client objects.
Collections manages those arrangements without owning the compiler, text, or graphics meaning of their elements.

The implementations remain in `old-source/core/`.
The children below refine [Foundation's assignment](../foundation.md#existing-implementation-mapping), rather than describe completed file moves.
The existing failure-reporting cycle and a compiler-specific member of `UIntSet` remain [unresolved boundaries](#unresolved-boundaries).

## Concepts

[Foundation](../foundation.md#concepts) defines owned allocations and borrowed views.
Collections distinguishes the lifetime of element storage from the lifetime of anything an element refers to: storing a pointer does not retain its pointee, whereas storing a retaining smart pointer applies that type's ownership rules.
An element can itself contain references, so copying its containing collection does not necessarily give the copy independent ownership of the referenced data.

- A **sequence** orders elements by position and permits traversal in that order.
  Contiguous storage permits a pointer-and-count view; segmented storage can preserve element addresses during append without providing one contiguous range.
- A collection's **count** describes its logical contents; its **capacity** describes the storage available for those contents without further allocation.
  In the existing `Array`, `List`, and `ShortList`, reducing the count need not end the lifetimes of elements outside the new logical contents.
  `UIntSet::getCount` is a different existing convention: it reports the number of represented bit positions, while `countElements` counts members.
- A **dictionary** associates each distinct key with a value.
  A **set** records membership without a separate client-visible value.
  Equality and, for hash lookup, compatible hash codes determine whether keys are the same; the stored values' own equality is a separate question.
- An **intrusive list** orders existing objects through a link stored in each object.
  `InternallyLinkedList` supplies that linkage and traversal without allocating, retaining, or deleting the objects.
  `LinkedList` instead allocates separate nodes containing values and deletes those nodes when clearing the list.

## Dependencies

- [Scalar Types](../base-types/scalar-types/scalar-types.md) supplies count, index, and integer conventions.
  [C++ Utilities](../base-types/cpp-utilities/cpp-utilities.md) supplies the `_Move` cast used to transfer element and container state.
  [Numeric Operations](../base-types/numeric-operations/numeric-operations.md) supplies logarithm, minimum, and maximum helpers used by collection growth and range operations.
- [Memory's Heap Storage](../memory/heap-storage/heap-storage.md) supplies the default allocator and typed buffer allocation and destruction used by growable sequences.
  Allocator parameters retain their existing allocation and release requirements; these containers do not impose one allocation family on all collections.
- [Hashing](../hashing/hashing.md) supplies hash-code types, ordinary hash dispatch for keys and pair components, and hash combination for the pair hash.
  The integer-set hash uses the hash-code type and computes its XOR locally.
  Clients supply compatible key equality and any type-specific hashing or heterogeneous lookup behavior.
- [Failure Reporting](../failure-reporting/failure-reporting.md) supplies assertion and signal machinery used by collection operations to check index, capacity, and key preconditions and to report internal consistency failures.
  The service dependency includes debug assertions even where release builds use optimizer assumptions.
- The product header `include/slang.h` supplies the `SLANG_FORCE_INLINE` annotation used on collection accessors and helpers.
- The host C++ environment supplies embedded arrays, node allocation through `new`/`delete`, standard algorithms, type traits, and pairs.
  The integer-set implementation uses byte operations and the existing compiler-specific bit-scan intrinsics.
- `unordered_dense`, available through [Foundation](../foundation.md#dependencies), supplies the ordinary dictionary's hash table and storage.
  Clients instantiate that backend through the existing public build dependency.

Element construction, assignment, destruction, comparison, hashing, and callbacks can invoke operations supplied by a client's types.
Those operations do not make Collections depend on every client module.
The templates require the particular expressions used by the selected operation; they do not establish one common copyability, default-construction, or const-invocation requirement for every collection.
The concrete `UIntSetVal` adaptation described below differs from those generic operations because it names a compiler type directly.

## Decomposition

The proposed children separate access to existing elements, ownership of sequence storage, ordering through links, and key-based membership.
All five are public ordinary modules within Foundation, rather than independently built libraries.

| Child                                                                         | Service                                                                                                                   |
| ----------------------------------------------------------------------------- | ------------------------------------------------------------------------------------------------------------------------- |
| [Array Views](array-views/array-views.md)                                     | Borrowed access, slicing, traversal, comparison, and search over contiguous elements.                                     |
| [Sequence Storage](sequence-storage/sequence-storage.md)                      | Fixed-capacity, growable, and segmented storage for ordered elements, including search and rearrangement where supported. |
| [Linked Lists](linked-lists/linked-lists.md)                                  | Traversal and editing of sequences represented by separately allocated nodes or links embedded in client objects.         |
| [Associative Collections](associative-collections/associative-collections.md) | Key-value lookup and set membership, including ordered iteration and small add-only dictionaries.                         |
| [Integer Sets](integer-sets/integer-sets.md)                                  | Membership, enumeration, and set algebra for unsigned integers represented by bits.                                       |

Array Views can describe storage supplied by Sequence Storage or by a client.
Sequence Storage also uses Array Views for range input and output.
Associative Collections uses Linked Lists to preserve the ordered dictionary's iteration sequence and Integer Sets to record occupied and deleted hash slots.
Integer Sets uses Sequence Storage for its bitmap words; it does not use Associative Collections.
These relationships explain the proposed ownership of shared machinery without making a child's implementation depend on the assembled Collections interface.

The clients justify public access to each concern.
Compiler and host utilities use array views and lists, the compiler uses short dictionaries for operation-local caches, and the graphics tools use chunked storage when native descriptors hold pointers to earlier entries.
Binary Layouts and RIFF Data use intrusive lists to connect objects whose storage they manage separately.
The children still require their own refinement before any implementation transfer.

## Storage, Lifetime, and Ordering

Sequence storage does not imply one layout or one removal policy.
`Array` embeds a fixed number of slots, `List` owns one growable buffer, and `ShortList` keeps its embedded prefix when allocating a separate overflow buffer.
`ChunkedList` appends into chunks so that adding another chunk does not relocate earlier elements; that property is not a guarantee about moving, clearing, or destroying the container.
For example, the D3D12 ray-tracing pipeline builder stores library and export descriptors in chunked lists and passes their addresses to native descriptor structures while continuing to append entries.
The graphics module owns the native request and its lifetime; Collections supplies the storage behavior.

Removing an item from a sequence's logical contents need not release resources held by its slot.
`List::removeLast` and `clear` only change the count, so a removed `RefPtr` can continue retaining an object until its slot is assigned again or the backing buffer is destroyed.
`Array` and the inline part of `ShortList` likewise have element lifetimes tied to their containing object, rather than to the logical count.
Linked-node deletion and hash-table erasure have different element destruction behavior.
Each child's refinement must preserve those differences, including copying, moving, and buffer ownership transfer.

An array view never extends the backing storage's lifetime.
Its validity also depends on mutations that relocate storage, remove elements from the described range, or change their positions.
There is no common iterator-invalidation rule across the containers: `List` exposes pointers, `ShortList` iterators store a container and index, and linked iterators follow nodes.
The existing const interfaces also differ: `ConstArrayView` exposes const element access, but `List::getArrayView() const` returns a mutable `ArrayView`.
`ShortList::getArrayView` returns a separate result object that borrows when the requested range is contiguous and owns a temporary copy when the range spans its two buffers.
The temporary's destruction releases that copy; assigning one of its copied slots does not assign the corresponding list slot.

Owning nodes does not imply ownership of objects referenced by their values.
`LinkedList` copies values into new nodes, while copying an `InternallyLinkedList` copies its head and tail pointers without cloning elements.
The intrusive list's `append` links the existing chain and leaves the argument's head and tail intact.
Clients therefore control the node lifetimes and coordinate changes to shared linkage; one embedded link cannot represent independent simultaneous list memberships.

Ordinary dictionary and hash-set iteration has no promised key or insertion order.
`OrderedDictionary` and `OrderedHashSet` instead traverse the linked sequence of entries.
Assigning an existing ordered-dictionary key deletes its node and appends a replacement, moving that entry to the end; rehashing rebuilds nodes and can invalidate their addresses too.
`ShortDictionary` offers add-only lookup rather than that update contract, retaining its inline entries when it copies them into the overflow dictionary.
`UIntSet` enumerates set bits in increasing numeric order, and its storage grows with the represented integer positions rather than just the number of members.

## Existing Implementation Mapping

The following paths are relative to `old-source/core/`.
Except for the compiler adaptation in `slang-uint-set.h`, these assignments cover the files' complete declarations and definitions, including their construction, iteration, search, and internal support operations.

| Existing Implementation                                                             | Proposed Owner and Responsibility                                                                                                                                                               |
| ----------------------------------------------------------------------------------- | ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| `slang-array-view.h`                                                                | Array Views: `ConstArrayView`, `ArrayView`, and the single-element, pointer/count, and native-array construction helpers.                                                                       |
| `slang-array.h`                                                                     | Sequence Storage: `Array`, its zero-capacity specialization, `FirstType`, array construction/insertion helpers, and generic `addToList` operations.                                             |
| `slang-list.h`                                                                      | Sequence Storage: `List`, its allocator and buffer-transfer operations, sorting/searching machinery, and `calcMin`/`calcMax` reductions.                                                        |
| `slang-short-list.h` and `slang-chunked-list.h`                                     | Sequence Storage: inline-prefix/overflow storage, temporary contiguous copies, and chunked append storage, with their iterators and lifetime operations.                                        |
| `slang-linked-list.h`                                                               | Linked Lists: `LinkedNode` and `LinkedList`, including node allocation/deletion, linkage editing, copying, and list conversion.                                                                 |
| `slang-internally-linked-list.h` and `slang-internally-linked-list.cpp`             | Linked Lists: embedded linkage, pointer traversal and concatenation, and `InternallyLinkedListNode`; the implementation unit currently only includes its header.                                |
| `slang-dictionary.h`                                                                | Associative Collections: `KeyValuePair`, `KVPair`, pair access adapters, `Dictionary`, `OrderedDictionary`, the hash-set family and its shared base, and their bookkeeping types and constants. |
| `slang-short-dictionary.h`                                                          | Associative Collections: the add-only small dictionary and its inline-to-hash-table promotion.                                                                                                  |
| `slang-uint-set.h` and `slang-uint-set.cpp`, except the `UIntSetVal` coupling below | Integer Sets: bitmap storage, iteration and set operations, hashing, and the logarithm and bit-scan helpers used by that implementation.                                                        |

The pair adapters support both `std::pair` and `KeyValuePair`; they do not assign the standard pair type or its element semantics to Collections.
Type-specific keys and comparators, compiler work queues and caches, text interning, binary object ownership, and serialization formats remain with their clients.
The broad `slang-basic.h` include in the intrusive-list header and exception-header includes in the array and dictionary headers do not establish additional semantic dependencies by themselves.

The current build compiles `slang-uint-set.cpp` and the intrusive-list implementation unit into static `core` and separately into enabled `slang-rt` under runtime export settings.
Future transfers preserve both consumers and the [root's staged binary-interface contracts](../../slang.md#staged-migration-contracts), including uses of collection types within other services' layouts and signatures.
This document changes neither C++ namespace identity nor the selection of allocator or hash-table implementation.

## Unresolved Boundaries

Collections participates in [Foundation's failure-reporting cycle](../foundation.md#failure-reporting-and-basic-data-services).
For example, a checked list access uses Failure Reporting, whose signal handler constructs a Text string; Text's encoding buffers use `List` in turn.
Narrower includes do not remove those behavioral dependencies, and no replacement assertion or allocation policy is selected here.
The ancestor's ownership decision or behavior-preserving separation remains a prerequisite for migration of the affected services.

Integer Sets has an additional existing dependency that does not fit Foundation's compiler-independent responsibility.
`slang-uint-set.h` forward-declares Compiler's `UIntSetVal`, grants it friendship, and declares `UIntSet::unionWith(const UIntSetVal&)`.
The member's definition is in `old-source/slang/slang-ast-val.cpp`, where it reads the compiler value's bitmask operands into the bitmap; `UIntSetVal::toUIntSet` also accesses the bitmap through that friendship.
`CapabilityTargetSet::unionWith(const CapabilityTargetSetVal&)` uses both conversion and union, and `CapabilityStageSet::tryJoin(const CapabilityTargetSetVal&)` calls the specialized union when both atom sets exist.
Those are concrete compiler adaptations, rather than element-type operations selected by an otherwise generic container.
The generic Foundation implementation does not call that member or link its definition; the coupling is in the shared class contract and Compiler's implementation of the member.
Foundation and Compiler must reconcile ownership of that member contract and privileged access before Integer Sets can be accepted for migration.
The member remains part of the same `UIntSet` class definition as the generic bitmap operations; describing separate responsibilities does not make that class physically separable.
The generic bitmap services are assigned here, but moving only their implementation or the forward declaration would leave the cross-module dependency intact.
The existing member signature, class identity, friend access, and compiler call paths remain preservation constraints until an ancestor decision establishes a permitted separation.
