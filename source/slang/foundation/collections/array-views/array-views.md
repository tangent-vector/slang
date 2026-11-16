# Array Views

Array Views provides borrowed access, slicing, traversal, comparison, and search over contiguous elements without extending their storage lifetime.
Callers can pass one element, a native C++ array, or part of a container's storage through the same pointer-and-count interface.
The module describes those elements and provides operations over them; allocation, element lifetime, and the meaning of the elements remain with their owners.

This is a proposed public leaf of [Collections](../collections.md#decomposition), with no child modules or separate build artifact.
The implementation remains in `old-source/core/slang-array-view.h`.
The service areas below refine the parent's complete-file assignment without transferring the implementation or settling the ancestor's [unresolved boundaries](#migration-constraints).

## Concepts

[Foundation](../../foundation.md#concepts) defines borrowed views and owned allocations.
[Collections](../collections.md#concepts) distinguishes element storage from objects that an element refers to, and defines sequence positions and counts.
[Scalar Types](../../base-types/scalar-types/scalar-types.md#concepts) owns the signed `Index` and `Count` conventions; both are aliases of `Int`, rather than validated range types.

An **array view** describes contiguous elements with a buffer pointer and an element count.
The pointer can identify client storage, a container's buffer, or temporary storage whose owner is separate from the view.
Copying or slicing the view copies that description without copying, constructing, destroying, or retaining elements.
For a view of pointers, the element storage is the sequence of pointer slots; the pointed-to objects have their own lifetimes.

`ConstArrayView<T>` exposes `const T*` traversal and buffer access and `const T&` indexed access.
`ArrayView<T>` derives publicly from that type and adds mutable access when the view object is non-const.
Const access to a pointer slot does not make its pointee const, and a const view object does not make the backing storage immutable through other aliases.
The existing search operations have an additional mutation qualification described below; their const member qualification does not establish read-only element operations.

## Dependencies

- [Scalar Types](../../base-types/scalar-types/scalar-types.md) supplies `Index` and `Count` for indexing, stored counts, split positions, and search results.
- [Failure Reporting](../../failure-reporting/failure-reporting.md) supplies the assertions used for indexed access and slicing.
  Those assertions check in debug builds and become optimizer assumptions otherwise; the dependency is not limited to builds that report failures at runtime.
- The product header `include/slang.h` supplies the `SLANG_FORCE_INLINE` annotation.
  Its product declarations remain outside this implementation hierarchy.
- The host C++ environment supplies native arrays, pointer arithmetic and comparison, references, inheritance, and `size_t` for native-array extents.
  Instantiating an operation requires its expressions to be valid under normal C++ lookup, overload resolution, and conversion rules.

The current header obtains the scalar, assertion, and annotation facilities through the mixed `slang-common.h` include.
That include does not make Array Views depend on all of the common header's services.
The views allocate no storage and require no allocator or stored-callable wrapper.
[Sequence Storage](../sequence-storage/sequence-storage.md) constructs and consumes views, but the view implementation does not call or name a container service in return.
Likewise, a template's use of a client operator or predicate does not create a concrete dependency on every module that instantiates the template.

## Service Areas

We keep construction, access, and range operations together because the mutable and const interfaces share one representation and inheritance contract.
The leaf provides the following services.

### Construction and Access

The default constructors describe zero elements with a null buffer.
Pointer/count constructors and helpers store the supplied pointer and count without checking the pointer, the count's sign, or the extent of the storage.
The single-element helpers construct a count-one view from `&obj`.
With built-in address-of, that pointer identifies the argument; a client unary `operator&` can instead supply a different compatible pointer.
In particular, `makeConstArrayViewSingle` does not extend the lifetime of a temporary bound to its reference parameter.
Native-array helpers derive their count from the array extent.
The caller supplies a representable extent and live, suitably typed contiguous elements for every element access it performs.
Negative counts have no defined alternative meaning in this service; the absence of constructor validation is not a promise that later operations support them.

`getCount` returns the stored count, `getBuffer` and `begin` return the buffer pointer, and `end` computes the pointer plus the count.
`operator[]` asserts `0 <= idx && idx < m_count` before returning an element reference.
Mutable `ArrayView::getLast` directly returns the element at `m_count - 1`, requiring a live last element without performing an assertion.
There is no corresponding const `getLast` overload.

### Slicing and Traversal

`head(index)` describes the prefix before a split position, and `tail(index)` describes the suffix beginning at that position.
Both assert that the split lies between zero and the count, inclusive.
For a valid range, the endpoints therefore permit an empty prefix or suffix; the split is not an index counted backward from the end.
The mutable overloads return `ArrayView<T>`, while the const base overloads return `ConstArrayView<T>`.

Consider this example:

```cpp
int values[] = {10, 20, 30};
auto all = makeArrayView(values);
auto suffix = all.tail(1);
suffix[0] = 40;
const auto& constAll = all;
auto prefix = constAll.head(2);
```

The views describe the same native array, so the assignment changes `values[1]` and the element visible as `prefix[1]` to `40`.
The `prefix` result permits const access to its two slots, but does not prevent the earlier mutable views from assigning those slots.
All three views borrow `values` and cannot extend the local array's lifetime.

Traversal uses raw pointers rather than an iterator object that retains or checks the storage.
The existing endpoint and slicing implementations do not special-case a null buffer before pointer arithmetic.
A future transfer must preserve the existing representation and expressions without presenting them as additional validation or a general guarantee for arbitrary null pointer arithmetic.

### Search and Value Comparison

`indexOf` and `lastIndexOf` visit elements in increasing and decreasing index order, respectively, evaluating `m_buffer[i] == val` until a match.
`findFirstIndex` and `findLastIndex` use the same orders and stop when the supplied predicate returns true.
Each returns the matched index, or `-1` when no element matches.
For count zero, these searches return `-1` without accessing an element or invoking a predicate.

The value argument and the `const Func&` predicate are borrowed for the call; the implementation stores neither.
The current searches form their element expressions from the stored `T*`, even in `ConstArrayView`.
Consequently, a selected equality operator can receive mutable element access, and the predicate argument is a mutable `T` lvalue when `T` is non-const.
Any mutation must respect the actual backing object's constness and leave the storage and elements needed by the rest of the search valid.
The query names do not establish purity, and the implementation provides no snapshot or protection against relocation or reentrant changes to the view's pointer/count state.

The `operator==` member first tests `&rhs == this` and returns true if that condition succeeds.
With built-in address-of, that condition recognizes the same view object; dependent `&rhs` can instead select a client unary `operator&`, including through the element type's associated namespace.
If that first condition is false, the member rejects differing counts, obtains both buffer pointers, and compares corresponding elements through `const T*`, returning false at the first element `!=` result that tests true.
On that later path, count zero skips all element comparisons and returns true.
`operator!=` evaluates `!(*this == rhs)`, with C++ overload resolution applying to the equality and negation expressions.
The element `!=` expression used by view equality differs from the element `==` expression used by value search; the implementation does not impose a shared equivalence relation or require side-effect-free client operators.

### Memory-Range Comparison

`containsMemory(rhs)` tests `rhs.getBuffer() >= getBuffer()` and, when that succeeds, `rhs.end() <= end()`.
For ranges within the same backing array, those comparisons test whether the other range lies within this range, including their endpoints.
The operation does not compare element values, establish storage ownership or pointer provenance, or supply a total ordering for unrelated allocations.
It has no separate null-buffer or zero-count branch, so its pointer comparisons and endpoint calculations retain their existing C++ requirements.

## Storage and Caller Lifetimes

`getCount` reads only the view's stored count.
When an operation accesses an element, the caller must ensure that the element is alive and accessible at that address.
The same requirement applies when a returned pointer or reference is used to access an element.
Operations that evaluate stored pointers, compute endpoints, or compare addresses remain subject to their existing C++ requirements; the view does not make dangling-pointer operations safe.
Overlapping views are permitted; writes to shared slots are visible through aliases rather than copied into independent snapshots.
Changing a container's count or element order can change the validity or meaning of an existing range even when its allocation has not moved.
Array Views neither tracks those changes nor provides synchronization for concurrent access.

The owner of an object referenced by an element has a separate responsibility.
For example, borrowing slots containing retaining smart pointers does not itself copy those smart pointers or acquire another reference to their pointees.
Destruction of those slots ends their element lifetimes and can release the last retained reference to a pointee.
The view does not retain the predicate object or acquire ownership of objects referenced by client operators or captures.
Client operations invoked during search or comparison must preserve the validity of any view state or elements that the remaining execution accesses.

The existing producers illustrate why storage ownership remains separate.
`List::getArrayView() const` returns a mutable `ArrayView<T>` over the list's buffer, so constness of that producing container does not by itself promise const access through the result.
`ShortList::GetArrayViewResult` can instead own a temporary contiguous copy when the requested elements span its two buffers.
Its nested `arrayView` borrows that copy, whose storage is released when the owning result is destroyed; the copied slots have separate storage from the original list slots.
Those producer-specific rules remain with Sequence Storage.

## Existing Implementation Mapping

Array Views owns all declarations and definitions in `old-source/core/slang-array-view.h`, including the following related groups:

| Group                        | Existing Declarations                                                                                                                                                                                                  |
| ---------------------------- | ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| Const view                   | `ConstArrayView<T>`, its `ThisType` alias, both public constructors and its protected mutable-pointer constructor, protected buffer/count fields, const accessors, indexed access, slicing, searches, and comparisons. |
| Mutable view                 | `ArrayView<T>`, its `ThisType` and `Super` aliases, public inheritance and `using` declarations, mutable accessors and slicing, `getLast`, and both constructors.                                                      |
| Const construction helpers   | `makeConstArrayViewSingle(const T&)`, `makeConstArrayView(const T*, Count)`, and `makeConstArrayView(const T (&)[N])`.                                                                                                 |
| Mutable construction helpers | `makeArrayViewSingle(T&)`, `makeArrayView(T*, Count)`, and `makeArrayView(T (&)[N])`.                                                                                                                                  |

The file contains no separately assigned storage owner, allocator, compiler adaptation, or out-of-line implementation.
Container-producing overloads in `slang-list.h`, `slang-array.h`, and `slang-short-list.h` remain with Sequence Storage.
Clients such as Compiler Support's `SourceMap::getEntriesForLine` and `JSONContainer::getArray`/`getObject` expose views over their own entries; their storage and data interpretation remain with those clients.
The header's use by compiler code does not assign product API declarations to the compiler implementation.

## Migration Constraints

A future transfer preserves the existing declaration names and exact signatures, including const qualification, template parameters, return types, access levels, and inherited overload visibility.
In particular, `ConstArrayView(const T* buffer, Count count)` and `ArrayView(T* buffer, Index size)` retain their existing spellings even though `Count` and `Index` currently alias the same type.
The const native-array helper converts its extent with `Index(N)`, while the mutable helper uses `Count(N)`.
The protected `T* m_buffer` and `Count m_count` representation, public exposure of those fields through `ArrayView`, and public base relationship remain constraints rather than opportunities to strengthen encapsulation or constness during a port.
Existing caller uses in layouts and signatures require the [root's staged type-identity and binary-interface rules](../../../slang.md#staged-migration-contracts); this refinement does not select a namespace or ABI change.

The implementation is header-only and adds no translation unit or library.
Its consumers still need the existing assertion implementation where debug checks call Failure Reporting.
Any transfer must preserve access for static `core` clients and the enabled `slang-rt` artifact, which separately compiles Foundation code under runtime export settings.
The root's broader source-reuse and construction decisions remain open.

The dependency on Failure Reporting participates in [Collections' existing behavioral cycle](../collections.md#unresolved-boundaries): reporting a failed index assertion can construct Text strings, whose implementation uses Collections and native ownership in turn.
Narrowing the common-header include would not remove those calls.
An ancestor must resolve the affected ownership or establish a behavior-preserving separation before migration can be accepted.
This leaf refinement preserves the existing unchecked inputs, pointer expressions, client-operation behavior, and producer lifetimes; it selects no validation fix or replacement failure policy.
