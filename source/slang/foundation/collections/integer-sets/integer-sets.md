<!--
SPDX-FileCopyrightText: The Khronos Group, Inc.
SPDX-License-Identifier: CC-BY-4.0
-->

# Integer Sets

Integer Sets provides membership, increasing-order enumeration, and set algebra for unsigned integers represented by bits.
Clients use the service for occupied positions, removed archive entries, compiler analysis facts, and capability atoms; the meaning of those integers remains with the client.
Storage grows with the largest represented position, rather than only with the number of members.

This public ordinary module refines [Collections' assignment](../collections.md#existing-implementation-mapping).
Its implementation remains in `old-source/core/slang-uint-set.h` and `slang-uint-set.cpp`.
The generic bitmap operations form one concern, so the module has no children.
The shared class contract still names a compiler value, and Foundation's failure-reporting cycle remains unresolved.
Those [boundaries](#unresolved-boundaries) prevent accepting the module for migration.

## Concepts

[Collections](../collections.md#concepts) defines sets, counts, capacity, and the distinction between owning elements and retaining objects they refer to.
The integer-set representation adds the following distinctions:

- A **member** is an unsigned integer whose bit is set.
  `UIntSet::Element` is instead a 64-bit storage word, not one member.
  Word index `i` represents integers `64 * i` through `64 * i + 63`, with the lowest integer in the least significant bit.
  The public `kElementSize`, `kElementMask`, and `kElementShift` constants are respectively 64, 63, and 6, encoding the word width, bit offset mask, and word-index shift.
- The **represented extent** is the number of bit positions in the logical word sequence: `getCount()` returns the word count multiplied by 64.
  It is separate from the number of members returned by `countElements()` and from the backing list's allocated capacity.
  `UIntSet` stores no exact requested bit count after rounding to whole words.
- A **bitmask** is one word encoding membership for those 64 positions.
  The compiler's `UIntSetVal` stores such words as AST operands; it is a compiler representation rather than another generic container owned here.

For example, `UIntSet set(65)` creates two zero words, so `set.getCount()` is 128 and `set.countElements()` is zero.
After `set.add(64)`, only 64 is a member.
`set.setAll()` then sets all 128 represented positions, including positions above the constructor's argument.
The constructor's `maxVal` parameter name and `resizeAndUnsetAll` comment do not describe an inclusive maximum: the implementation rounds a count of positions, so `UIntSet(64)` initially represents 0 through 63.

## Dependencies

- [Sequence Storage](../sequence-storage/sequence-storage.md) supplies `List<Element>` for owned words and `List<T>` for the owned result of enumeration.
  Allocation, buffer transfer, capacity, destruction, and self-assignment behavior follow that existing implementation.
- [Scalar Types](../../base-types/scalar-types/scalar-types.md) supplies integer, count, and index types.
  [C++ Utilities](../../base-types/cpp-utilities/cpp-utilities.md) supplies `_Move` for transfer of the backing list.
  [Numeric Operations](../../base-types/numeric-operations/numeric-operations.md) supplies `Math::Min` and `Math::Max` for word-count comparisons.
- [Hashing](../../hashing/hashing.md) supplies `HashCode`, currently a 64-bit type.
  `UIntSet` computes its own XOR rather than invoking the general hash-combination service.
- [Failure Reporting](../../failure-reporting/failure-reporting.md) supplies the nonzero-input assertion in `bitscanForward` and the checks reached through list access and resizing.
  The dependency includes assertion behavior, not just header declarations.
- The host C++ environment supplies integer bit operations, byte fill/comparison, and the bit-scan intrinsic selected for MSVC or other compilers.
  `bitscanForward` locates the least significant set bit of a nonzero word; the iterator, enumeration, and missing-member search establish nonzero input before calling it.
  The local `intLog2` computes the word-index shift from the constant word width.

The [compiler adaptation](#compiler-adaptation) is an additional dependency in the existing class contract.
It is not accounted for by the generic services above or by a client-selected element operation.

## Principal Services

### Storage and Mutation

`UIntSet` owns its words through one `List<Element>`.
Copy construction and assignment duplicate those words into separate storage for distinct objects.
Move construction and assignment transfer the list's buffer and leave the distinct argument empty; `swapWith` exchanges the backing lists.
The current copy and move assignments do not guard self-assignment: `set = set` and `set = _Move(set)` empty the set through `List`'s assignment implementations.
Destruction releases the owned buffer.

The operations expose both membership and representation changes:

| Operation                           | Existing Behavior                                                                                                               |
| ----------------------------------- | ------------------------------------------------------------------------------------------------------------------------------- |
| `resize(size)`                      | Sets the word count to the rounded-up position count, preserving retained words and zeroing newly logical words. It may shrink. |
| `resizeBackingBufferDirectly(size)` | Sets the word count directly, with the same preservation and zeroing behavior. The unit is words.                               |
| `resizeAndUnsetAll(val)`            | Calls `resize(val)` and then clears every represented word.                                                                     |
| `setAll()` / `unsetAll()`           | Set or clear every represented bit without changing the word count.                                                             |
| `clearAndDeallocate()`              | Removes the represented words and releases the allocation.                                                                      |
| `add(val)` / `remove(val)`          | Set or clear a member's bit. Addition grows as needed; removal outside the extent does nothing.                                 |
| `addRange(list)`                    | Adds each unsigned integer in the argument list.                                                                                |
| `addRawElement(word, index)`        | ORs a bitmask into the indexed word, growing and zero-filling preceding new words as needed.                                    |

Shrinking within a word does not clear its high bits, because the rounded word count has not changed.
Shrinking to fewer words drops those words from the logical representation but may retain allocation capacity.
Growing again clears the newly logical words, including storage retained by the earlier shrink.
`getBuffer()` returns a borrowed const reference to the list; it exposes the exact logical words without transferring their ownership.
Pointers into its storage remain subject to list reallocation, transfer, deallocation, and owner lifetime.

The declaration calls the second `addRawElement` parameter `bitOffset`, but its definition and the capability generator use a word index.
The size arithmetic also has inherited limits: `resize` adds 63 before rounding, and `add` may calculate `val + 1` before growing.
Those unsigned operations can wrap, and conversion to the signed index and count types is not checked here.
Neither addition is checked for overflow before its result determines the buffer size.
The nominal unsigned-integer API therefore does not establish successful representation of every value of `UInt`.

### Queries, Set Algebra, and Hashing

`contains(val)` tests a member without growing storage.
`contains(set)` tests membership containment, accepting zero words beyond the receiver's extent.
`isEmpty()` and `areAllZero()` both scan for any nonzero word, regardless of represented extent.
`countElements()` counts set bits.
`getLSBZero()` returns the smallest missing integer; if all represented positions are members, it returns the represented extent, including zero for an empty word sequence.
The existing `areAllZero()` and `getLSBZero()` members are non-const even though their bodies only read the words.

`add(set)` and `unionWith(set)` merge another generic set by ORing shared words and retaining the larger extent.
`intersectWith(set)` ANDs shared words and clears the receiver's unmatched suffix, retaining the receiver's extent.
`subtractWith(set)` clears shared members and preserves the receiver's unmatched suffix and extent.
`hasIntersection` tests whether any shared word has a set bit in common.
The static `calcUnion`, `calcIntersection`, and `calcSubtract` operations write a result whose word count is respectively the larger input count, the smaller input count, or the first input count.
They do not share one output-aliasing behavior: `calcUnion(a, a, b)` clears `a` before reading it, losing any members contributed only by `a`.
The intersection and subtraction implementations instead read and write corresponding word indices without that preliminary clearing step.
The declarations do not specify a common aliasing guarantee.

Equality is not uniformly determined by membership.
When both operands have words, `operator==` compares the shared prefix and accepts additional zero words on either operand.
If either operand has no words, it instead requires both word counts to be zero.
Consequently a default-constructed empty set is unequal to `UIntSet(1)`, although both have no members; `UIntSet(1)` and `UIntSet(65)` are equal while both remain zero-filled.
`operator!=` negates that result.

`getHashCode()` XORs every word through an `int` accumulator and converts that accumulator to `HashCode` on return.
The accumulator's narrowing is part of the current computation, even though words and `HashCode` are 64-bit.
Extra zero words do not change the hash; the hash is neither a stable serialization nor a substitute for the equality test.
The uniformity analysis uses this equality/hash pair in its function-analysis cache key, so preserving only membership operations would miss an existing consumer contract.

### Enumeration and Borrowing

`begin()` and `end()` provide an iterator that enumerates members in increasing numeric order.
The iterator skips zero words, caches a copy of the current word's remaining bits, and finds each next member by scanning and removing the least significant remaining bit from that copy.
`getElements<T>()` performs the same ordered enumeration into a newly owned list, converting each integer to `T`.
It does not check whether `T` can represent every enumerated integer.

An iterator borrows the containing list object and does not retain the set.
It is not a snapshot: mutation can combine the cached remainder of one word with later words read from the changed list.
Moving or swapping the set also changes the storage subsequently accessed through that same list object.
No mutation-stability guarantee follows from its use of a list pointer rather than a word pointer.
Iterator equality compares the current word index and cached remaining bits, omitting container identity and the remembered current member.
Post-increment returns a reference to the advanced iterator, rather than a copy of its previous state.
These traversal operations do not establish a general standard-library iterator contract.

## Compiler Adaptation

`CapabilityAtomSet` derives from `UIntSet`, attaching compiler capability meaning to the generic membership representation.
`ASTBuilder::getUIntSetVal` reads `getBuffer()` and creates one unsigned 64-bit `ConstantIntVal` operand per word in a `UIntSetVal`.
It retains the exact word count, including trailing zero words, before requesting the cached AST value.
`UIntSetVal::toUIntSet` reconstructs that count and assigns each protected word through friendship.

The shared `UIntSet` class also declares `unionWith(const UIntSetVal&)`.
Its definition in `old-source/slang/slang-ast-val.cpp` reads the compiler value's bitmask operands, ORs shared words, and appends any remaining words.
`CapabilityTargetSet::unionWith(const CapabilityTargetSetVal&)` uses reconstruction when its mutable atom set is absent and the specialized member when that set already exists.
`CapabilityStageSet::tryJoin(const CapabilityTargetSetVal&)` also calls the specialized member when both atom sets exist.
Those actual paths make the member and privileged access part of the current contract, even though the generic implementation unit never calls the member.

## Existing Implementation and Build Consumers

The following assignment covers every declaration and definition of the generic implementation, including internal support.
It distinguishes semantic responsibility from an immediately separable file boundary.

| Existing Material                                                                                                                                            | Responsibility                                                                                                                                                                                                    |
| ------------------------------------------------------------------------------------------------------------------------------------------------------------ | ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| [`old-source/core/slang-uint-set.h`](../../../../../old-source/core/slang-uint-set.h)                                                                        | Integer Sets owns `UIntSet`, its aliases/constants, generic public operations, iterator, word storage, `_areAllZero`, `intLog2`, and `bitscanForward`, except for the compiler-specific contract described below. |
| [`old-source/core/slang-uint-set.cpp`](../../../../../old-source/core/slang-uint-set.cpp)                                                                    | Integer Sets owns the complete generic implementation: assignment, size/clear operations, set algebra, comparison, hashing, population count, and missing-member search.                                          |
| The `UIntSetVal` forward declaration, friend declaration, and specialized `UIntSet::unionWith` declaration in the same header                                | A partial compiler-facing class contract whose permitted separation still requires an ancestor decision.                                                                                                          |
| The `UIntSetVal` declarations and adaptation definitions in `old-source/slang/slang-ast-val.{h,cpp}`, `ASTBuilder::getUIntSetVal`, and capability operations | Compiler owns AST representation, construction, conversion, and capability policy; the specialized member definition remains part of the shared `UIntSet` type.                                                   |

`OrderedDictionary` uses two integer positions per hash slot for occupied and deleted markers.
The ZIP file system records removed archive-entry indices.
Compiler clients include block reachability, nonuniform-parameter analysis, GLSL binding allocation, and capability operations.
The capability generator additionally emits exact word counts and word-indexed `addRawElement` calls into generated C++.
Those clients depend on the representation services as well as abstract membership; their policies and generated formats remain with their existing owners.

`old-source/core/CMakeLists.txt` selects `slang-uint-set.cpp` into static `core` through `slang_add_target` and the recursive selection in `cmake/Glob.cmake`.
When `SLANG_ENABLE_SLANGRT` is enabled, `old-source/slang-rt/CMakeLists.txt` separately selects the core directory through `EXTRA_SOURCE_DIRS`, compiling the same implementation under its runtime export settings.
Runtime does not obtain that implementation by linking the `core` archive.
The compiler-specific member is selected with `slang-ast-val.cpp` by the compiler build, using `slang-common-objects` when the embedded-core configuration shares compiler objects between variants.
The locally built capability generator links `core` and `compiler-core`; `SLANG_GENERATORS_PATH` can instead select an imported generator executable.

## Unresolved Boundaries

The [Foundation failure-reporting cycle](../../foundation.md#failure-reporting-and-basic-data-services) remains relevant through the backing list's checked operations and the bit-scan assertion.
Failure Reporting constructs Text strings, and Text uses collection storage.
Narrowing the integer-set includes would not remove those behavioral dependencies.
No replacement failure or allocation policy is selected here.

The `UIntSetVal` member, friendship, and forward declaration introduce an additional compiler dependency within a Foundation class contract.
Moving only the generic `.cpp`, relocating the forward declaration, or describing separate semantic owners does not separate that contract.
Foundation and Compiler must establish a permitted ownership and access arrangement before the module can be accepted for migration.
The existing class identity, layout, member signatures, friendship, AST conversion, and capability callers remain constraints on that decision.
The [root's staged binary-interface rules](../../../slang.md#staged-migration-contracts) also apply to collection types appearing inside other services' signatures and layouts.
