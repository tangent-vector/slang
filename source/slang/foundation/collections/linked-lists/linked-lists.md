# Linked Lists

Linked Lists provides traversal and editing of sequences through separately allocated value nodes or links embedded in objects whose lifetime the client controls.
Both forms change sequence order through links; the intrusive form needs no separately allocated node for each object.
The element values and the meaning of the ordered objects remain the clients' responsibility.

The implementations remain in `old-source/core/`.
This document refines [Collections' assignment](../collections.md#existing-implementation-mapping) into one public ordinary module within Foundation.
The proposed boundary does not describe a completed implementation transfer or a new library.

## Concepts

The [root document](../../../slang.md#concepts) defines modules, visibility, and dependencies.
[Foundation](../../foundation.md#concepts) defines owned allocations and borrowed views, and [Collections](../collections.md#concepts) defines sequences, intrusive lists, and the distinction between element storage and anything an element refers to.

`LinkedList<T>` represents an ordered sequence with separately allocated `LinkedNode<T>` objects.
Each node contains a `T` value, links to its previous and next nodes, and a pointer to the list used by node-level editing operations.
`InternallyLinkedList<T>` instead holds the first and last pointers for a chain of client objects.
Each client object publicly inherits `InternallyLinkedList<T>::Node`, which supplies one successor link; `InternallyLinkedListNode<T>` names that same base type.

Both forms expose borrowed node access.
An owning-list iterator dereferences to a value reference, whereas an intrusive iterator dereferences to a pointer to the client object.
Neither iterator retains the node or extends the lifetime of objects referenced by a stored value or client object.

## Dependencies

- [C++ Utilities](../../base-types/cpp-utilities/cpp-utilities.md) supplies `_Move`, used to select the owning list's move assignment during move construction.
- [Failure Reporting](../../failure-reporting/failure-reporting.md) supplies the unexpected-condition signals used by owning-list access and the assertions used by intrusive editing.
  Those assertions check non-null arguments and selected endpoint consistency conditions; they do not validate list membership or disjointness.
- [Sequence Storage](../sequence-storage/sequence-storage.md) supplies `List<T>`, the result type and append operation used by `LinkedList::toList`.
  The existing conversion has the deferred spelling mismatch described [below](#existing-limitations-and-migration-constraints).
- The host C++ environment supplies node allocation and deletion through `new` and `delete`, default initialization, and `std::conditional_t` for the const iterator's node and reference types.

Construction, assignment, destruction, equality, and callbacks selected through template arguments remain requirements on the client's C++ types.
The intrusive operations also depend on the particular member expressions described below.
Invoked client-defined operations can have additional effects.
The storage and linkage descriptions below identify changes made directly by the list implementation; those client operations may also mutate or destroy other nodes or referenced objects.
Those client operations do not make Linked Lists depend on the compiler or every other module whose objects appear in a list.
The implementations name no compiler semantic types.
The allocator-header include in `slang-linked-list.h` does not mean its nodes use Foundation's allocator classes: the node operations use ordinary `new` and `delete`.
Likewise, the broad `slang-basic.h` include in the intrusive header does not establish a service dependency on everything that aggregate exposes.
Product headers under `include/` remain existing external contracts rather than part of this module's ownership.

## Principal Services

Linked Lists remains a proposed leaf, with no public or private child modules.
Its two service areas share linked traversal and insertion but keep their different node-lifetime policies explicit:

| Service Area               | Responsibility                                                                                                                                  |
| -------------------------- | ----------------------------------------------------------------------------------------------------------------------------------------------- |
| Owned value sequences      | Allocate and destroy nodes, edit links and endpoints, find and visit values, copy or transfer list contents, and expose node-based iteration.   |
| Intrusive object sequences | Connect existing client objects, insert objects within a chain, concatenate chains, and traverse their embedded links without taking ownership. |

### Owned Value Sequences

`LinkedList` starts empty.
Clearing and destruction walk its reachable chain, saving the next pointer before deleting each node.
Deleting a node destroys its `T` value.
Any release or retention of objects referred to by that value follows `T`'s own contract, as described by [Collections](../collections.md#concepts).
For example, a raw pointer value does not make the list retain or delete the pointed-to object.

Value-taking insertion operations allocate a node, default-initialize its value, and then assign the supplied value from `const T&`.
The blank `addLast()` overload performs no value assignment, so a scalar value can remain uninitialized until the caller assigns it.
`addFirst(const T&)`, both `addLast` overloads, and node-level `insertBefore` and `insertAfter` update the stored count as well as the links and endpoints.
The link edits performed by these insertion operations do not relocate existing nodes or their values.

Node-level `removeAndDelete` updates the owning list and deletes that node.
The list-level `removeAndDelete(node, Count)` saves each successor before detaching and deleting the current node, stopping its loop at the requested count or the end of the chain.
For a positive count, the starting node must be live and belong to that list; the operation does not check membership.
Deleting nodes invalidates pointers and value references to those nodes, while clearing or destroying the list invalidates access to every node it deletes.

Two lower-level operations expose different bookkeeping behavior.
`addFirst(LinkedNode<T>*)` links an existing node without incrementing the count or replacing the node's stored list pointer.
`removeFromList` detaches a node and clears its neighbor links without decrementing the count, deleting its value, or clearing its stored list pointer.
Detached nodes are no longer reached by `clear`.
The caller arranges a detached node's later deletion or reinsertion; its stored list pointer does not keep the list alive.
Those operations do not provide a checked transfer between owners, and `getCount` returns the stored count rather than recounting reachable nodes.
Node-level edits require a live stored list pointer and linkage consistent with that list.
The public `LinkedNode` constructor initializes its links to null and stores its list pointer without itself inserting the node into that list.
Implicit node copying or moving also copies the links and stored list pointer, subject to the operations available on `T`; those member copies do not establish an independent list membership.

The `LinkedList` copy and move assignments first delete the receiving list's existing nodes, invoking their value destructors before copying or acquiring the argument's chain.
When copying from a different list, copy construction and copy assignment create separate nodes and copy-assign their values in traversal order.
The copy therefore owns different node storage, but any references contained within `T` still follow `T`'s copying rules.
When moving from a different list, move construction and move assignment transfer the existing nodes, empty the argument's endpoints and count, and rewrite every transferred node's list pointer to the receiving list.
That pointer transfer and owner-pointer rewrite do not relocate the nodes actually transferred or invoke a `T` move operation.
Self-assignment of a nonempty list deletes its nodes and leaves the list empty.

`find` compares values for equality and returns a borrowed matching node or null.
`getFirstNode` and `getLastNode` return endpoint pointers, including null for an empty list.
`getFirst` and `getLast` signal an unexpected condition when empty.
The const endpoint functions still return mutable nodes or `T&`; const iteration instead yields `const T&`.
`getNode(int)` walks forward rather than providing uniformly checked indexing: a negative index returns the head, reaching the end returns null, and attempting a further step from null signals.

The owning iterator stores the current node and a cached successor.
Incrementing follows the cached successor, so deleting only the current node can be followed by incrementing if that successor remains live.
Deleting the cached successor makes that next increment invalid, and inserting between the current node and its cached successor does not change the iterator's next step.
The iterator compares current-node pointers without checking which list produced them; dereference requires a live current node.
`forEach` has a different mutation constraint: it reads the next link after invoking its callable on the current value, so the callable must leave that current node live for the subsequent read.

`toList` is declared to return `List<T>`, and its body appends the node values in traversal order.
That target type owns separate contiguous element storage; referenced objects would still follow `T`'s copying rules.
The preallocation call has the spelling mismatch described [below](#existing-limitations-and-migration-constraints).

### Intrusive Object Sequences

`InternallyLinkedList` neither allocates, retains, nor deletes its objects.
Destroying a list object only ends the lifetime of its endpoint holder; the objects and their embedded successor links remain.
Clients keep every object used by traversal alive, including objects reached after the first element, and coordinate mutations through any aliases of the same links.
Const endpoint access returns mutable `T*`, and iteration is available through non-const `begin` and `end` only.

The successor accesses in `add` with an existing tail, interior `insertAfter`, and `append` between nonempty lists use member lookup through `T*`.
Iterator increment instead casts to `Node const*` before reading the base's `_next`.
The successor-editing descriptions and traversal consequences below concern the case where those mutator expressions select that inherited member.
For example, a public `T* _next` declared by `T` can hide the base member: the mutators then change the separate pointer while iteration still follows the base link.
Other hiding declarations can make a particular mutator expression unavailable when instantiated.
Public inheritance alone does not ensure that mutation and iteration access the same member.

`add` takes a non-null object pointer, makes that object both endpoints when the list is empty, or links the existing tail to it and updates the last pointer.
It does not clear the appended object's successor link.
`insertAfter` rewrites the relevant successor links, delegating to `add` when the supplied existing object is the recorded tail.
Callers supply valid membership and linkage: the existing object must belong to the intended chain, and the newly inserted object's embedded link cannot simultaneously encode an independent list membership.
In particular, appending an object with an existing successor can expose a longer chain without making the recorded tail describe its end.
The implementation's assertions do not establish these caller obligations.

Implicit copying or moving of a list object copies its two endpoint pointers without cloning objects or emptying the argument.
`append(const InternallyLinkedList&)` also keeps the argument's endpoints intact.
For a nonempty argument, it either adopts those endpoints into an empty recipient or connects the recipient's last object to the argument's first object and adopts the argument's last pointer.
Both list objects can then reach the same objects and mutable successor links.
A later insertion through either holder can affect traversal through the other without updating the other's recorded tail.
Concatenating a chain with itself or an overlapping chain can create a cycle; no membership or overlap check prevents that behavior.

An intrusive iterator reads the current object's `Node::_next` when incremented, rather than caching the next pointer.
The current object must therefore remain live until that read, and changing that base link changes the iterator's next step.
Destroying the endpoint holder alone does not invalidate an iterator into still-live objects, but destroying the client-owned objects does.
The list supplies no removal, count, or object-lifetime management operation.

## Existing Implementation Mapping

These complete files remain in `old-source/core/`:

| Existing Implementation            | Assigned Responsibility                                                                                                                                                           |
| ---------------------------------- | --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| `slang-linked-list.h`              | `LinkedNode`, `LinkedList`, mutable and const iterators, allocation and deletion, node and list editing, traversal, lookup, copying, moving, count access, and `List` conversion. |
| `slang-internally-linked-list.h`   | `InternallyLinkedList`, its node base and iterator, endpoint access, object insertion and concatenation, and the `InternallyLinkedListNode` alias.                                |
| `slang-internally-linked-list.cpp` | The existing translation unit that includes the intrusive header and defines no additional service.                                                                               |

No declarations in these files are assigned to another owner.
The dictionary and binary-format clients remain outside this module:

- [Associative Collections](../associative-collections/associative-collections.md) owns `OrderedDictionary`, whose hash slots point to nodes in its `LinkedList` of entries.
  It obtains a blank tail node before assigning a new entry; replacing a key deletes its old node, and rehashing builds new nodes.
  The dictionary's key policy and resulting pointer invalidation remain its responsibility.
- [Binary Layouts](../../binary-layouts/binary-layouts.md) owns `BlobBuilder` and its chunk and shard objects, allocated in the builder's arena.
  `ChunkBuilder::addContentsOf` copies the donor's intrusive list holder, explicitly resets that donor holder, and appends the saved chain.
  The client supplies that transfer policy; intrusive `append` itself does not empty the donor.
- [RIFF Data](../../riff-data/riff-data.md) owns its builder's arena and the chunk and shard objects stored there.
  Its `getChildren` and `getShards` return intrusive lists by value, borrowing the same objects and linkage rather than copying objects or payload bytes.
  The RIFF builder and any separately borrowed payload storage must outlive their corresponding uses.

## Existing Limitations and Migration Constraints

`LinkedList::toList` currently calls `List::Reserve`, whereas the assigned `List` implementation declares `reserve`.
This refinement records that mismatch without changing the conversion, its signature, or its implementation.
The raw node operations' bookkeeping differences, self-assignment behavior, const access to mutable nodes, and unchecked intrusive aliasing likewise remain existing behavior rather than newly repaired guarantees.

Failure reporting still participates in [Foundation's behavioral cycle with basic data services](../../foundation.md#failure-reporting-and-basic-data-services).
Narrowing the intrusive header's aggregate include does not remove its assertion calls or settle that ownership question.
The failure and sequence dependencies therefore remain subject to the ancestor's unresolved separation.

The current `core` target selects these headers and the intrusive translation unit through `slang_add_target` and its recursive file selection.
Enabled `slang-rt` separately selects the same core directory through `EXTRA_SOURCE_DIRS` under runtime export settings.
A future implementation transfer preserves both consumers, even though most operations are templates and the existing translation unit only includes a header.
That requirement does not create a separate Linked Lists artifact or select a general rule for sharing implementations between artifacts.

Existing callers, declaration names and signatures, and uses of the types within other services' layouts remain preservation constraints.
Any later namespace or header changes must satisfy the [root's canonical identity and binary-interface requirements](../../../slang.md#staged-migration-contracts).
This leaf does not resolve the root's [construction and artifact questions](../../../slang.md#unresolved-architectural-dependencies), Foundation's service cycles, or Collections' separate compiler adaptation in `UIntSet`.
Those continuing constraints prevent treating the proposed leaf structure as evidence that migration is ready.
