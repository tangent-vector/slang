<!--
SPDX-FileCopyrightText: The Khronos Group, Inc.
SPDX-License-Identifier: CC-BY-4.0
-->

# Relative Pointers

Relative Pointers provides non-owning pointer fields whose targets are resolved from each field's own address and a stored byte displacement.
Clients use these fields in binary object graphs so that moving the field and target together can preserve their relationship without rewriting native addresses.
Clients are responsible for the containing storage's lifetime, the target's lifetime, record layout, and validation of the graph.

This is a public ordinary leaf of [Binary Layouts](../binary-layouts.md), within Foundation; it has no children or separate build artifact.
Its services cover field-relative access, encoding pointer values during C++ construction and assignment, and the fixed-width field representations used by binary-layout clients.
The implementation remains in `old-source/core/slang-relative-ptr.h` and its include-only translation unit, `slang-relative-ptr.cpp`.

## Concepts

[Binary Layouts](../binary-layouts.md#concepts) defines a self-relative pointer and distinguishes its signed displacement from the unsigned, explicit-base representation owned by [Offset Containers](../offset-containers/offset-containers.md).
[Foundation](../../foundation.md#concepts) defines relocatable references and borrowed views; resolving a relative pointer likewise does not extend the lifetime of the referenced storage.
[C++ Utilities](../../base-types/cpp-utilities/cpp-utilities.md#concepts) defines an object representation as the bytes occupied by a C++ object, including padding.
Moving those bytes and invoking a C++ copy or move operation are different operations, as described below.

The built-in forms are `RelativePtr32<T>` and `RelativePtr64<T>`.
They select `detail::RelativePtr32Traits` and `detail::RelativePtr64Traits`, respectively, for the general `RelativePtr<T, Traits>` template.
Their `Offset` types are signed `Int32` and `Int64`; their public `UOffset` aliases are `UInt32` and `UInt64`.
The following field and transfer contracts describe these built-in integer forms; the general template's expression requirements are recorded separately under [Traits](#traits).

## Services and Contracts

### Field-Relative Access and Lifetime

A non-null field stores the target address minus the address of the `RelativePtr` object, measured in bytes.
Resolution adds that signed displacement to the pointer object's current address.
Both operations convert addresses and offsets through `intptr_t`; a negative displacement can identify a target before the field.
The wrapper stores only its `Offset` member and has no buffer base, extent, allocator, or retained owner.
The client keeps the field's storage valid while resolving it and keeps the target alive and suitably aligned and typed while using the returned native pointer.

Default construction and setting a null native pointer store zero, and resolving zero returns null.
A target at the pointer object's own address also produces zero and therefore cannot be distinguished from null.
`get() const`, the implicit native-pointer conversion, and `operator->() const` return `T*`; a const wrapper does not add const qualification to `T`.
The conversion and arrow operations delegate to `get`, without an additional null, extent, alignment, or lifetime check.
Resolving a field does not read or copy the target object.

For a non-null native pointer, `set` first computes the `intptr_t` address difference, converts it to `Offset`, stores it, and then asserts that conversion back to `intptr_t` recovers the difference.
The 32-bit form therefore requires the computed difference to fit `Int32`.
The 64-bit form still performs its address arithmetic in `intptr_t`, so a wider stored field does not remove the host arithmetic's limits.
The pointer-to-integer conversions and signed arithmetic retain their existing host assumptions; the implementation does not establish that every pair of native addresses can be represented.
The narrowing assertion follows the store and supplies no result-returning checked conversion.
Under the existing [Failure Reporting](../../failure-reporting/failure-reporting.md#concepts) contract, `SLANG_ASSERT` is a debug assertion and otherwise an optimizer assumption, rather than release-time input validation.

### C++ Transfers and Buffer Relocation

Construction or assignment from a native `T*` computes a displacement for the destination field.
Copy and move construction and assignment first resolve the other wrapper through its implicit `T*` conversion, then pass that native pointer to the destination's `set` operation.
The destination retains the zero-sentinel and address-arithmetic limits described above.
A non-null target at the destination wrapper's own address is encoded as zero and subsequently resolves as null; the displacement round-trip assertion still passes.
The target object is neither copied nor transferred, and the moved-from wrapper is not cleared.
All three assignment overloads return `void`; the native-pointer constructor and native-pointer conversion are implicit.

Consider these local objects:

```cpp
struct Node
{
    Slang::RelativePtr32<Node> link;
};

struct Storage
{
    Node node;
    Slang::RelativePtr32<Node> source;
};

Storage storage;
storage.source = &storage.node;
storage.node.link = storage.source;
```

The first assignment gives `storage.source` a nonzero displacement to `storage.node`.
The second resolves that pointer, then encodes it for `storage.node.link`, which has the same address as its containing node.
The computed displacement is zero, so `storage.node.link.get()` returns null.
The assignments are accepted by the existing implementation; the null result follows from the zero-sentinel representation.

For example, suppose a field is at byte offset 16 in a buffer and its target is at byte offset 48.
Its stored displacement is 32.
Relocating the complete encoded buffer preserves that displacement and the relationship between the relocated field and relocated target.
By contrast, C++ copy construction of another wrapper computes a displacement to the original target at its existing address.
Copying only the encoded field bytes preserves 32, so resolution from a different field address generally identifies a different target.
Clients choose the operation that matches whether they are transferring a pointer value or relocating an encoded graph.
Preserving the address difference alone does not establish that the other fields or C++ object lifetimes in an arbitrary containing object permit byte relocation.

### Traits

The general template exposes `This`, `Value`, `RawPtr`, `Offset`, and `UOffset` aliases, with the latter two taken from the supplied `Traits` type.
`Traits::UOffset` is required as a nested name but is not otherwise used by the implementation.
The stored `Offset` is initialized from zero.
Each instantiated operation also requires its corresponding expressions: `get` compares the stored value with zero and converts it to `intptr_t`; `set` assigns zero or a value constructed from an `intptr_t` difference and uses the round-trip assertion expression.
Copy and move operations reuse those requirements through native-pointer conversion and `set`.
The existing template has no constrained-trait declaration that enforces signed-integer storage or excludes user-defined conversions and operators.
For custom instantiations, the effects of any user-defined conversion or operator come from the supplied type, rather than an integer-only restriction enforced by this template.

## Dependencies

- [Scalar Types](../../base-types/scalar-types/scalar-types.md) supplies the exact-width signed and unsigned integer aliases used by the two built-in traits.
- [Failure Reporting](../../failure-reporting/failure-reporting.md) supplies the assertion and assumption contract for the displacement round-trip check.
- The product header `include/slang.h` supplies `SLANG_FORCE_INLINE` for the existing constructors, assignments, conversion, and arrow operation.
- The host C++ environment supplies `intptr_t`, native pointers, and the pointer/integer conversions and arithmetic used to encode and resolve addresses.

The legacy header obtains these facilities through `slang-basic.h` and `slang-common.h`.
Their aggregate includes do not establish service use of collections, strings, allocation, or native smart-pointer ownership.
The built-in pointer operations allocate no storage and perform no stream or operating-system operation.

## Clients and Representation Compatibility

Compiler's [Module Storage](../../../compiler/module-storage/module-storage.md) owns Fossil's binary object-graph format and its reader and writer.
Its `FossilizedPtr<T>` derives from `RelativePtr32<T>` and inherits the constructors; `Fossil::RelativePtrOffset` names the inherited `Offset` type.
The client asserts that `FossilizedPtr<void>` occupies four bytes.
`Fossil::Header::rootValue` uses that wrapper, and `Fossil::getRootValue` calls `.get()` on the field at its actual location in the supplied bytes.
Fossil's record layouts, conditional validation, object traversal, and backing-storage lifetime remain client responsibilities.

The matching writer path uses [Blob Composition](../blob-composition/blob-composition.md).
`Fossil::SerialWriter::_initialize` requests a deferred 32-bit reference from its header chunk to its root-value chunk.
After layout, `ShardBuilder::_writeTo` subtracts the emitted field offset from the target chunk's content offset and writes a native `Int32` or `Int64` displacement.
Blob Composition computes those integer fields without constructing a `RelativePtr` or calling `set`; the emitter and reader share a representation convention rather than a pointer-operation call path.
In particular, the emitter does not inherit `set`'s narrowing assertion.
Neither this field representation nor its fixed width supplies endian conversion, portable C++ record layout, or validation of arbitrary input bytes.
Offset Containers' unsigned offsets and explicit buffer base remain a different representation and service.

## Existing Implementation Mapping

The following complete declarations and implementation belong to this leaf.
Paths are relative to `old-source/core/`; no class is split among owners.

| Existing File            | Assigned Responsibility                                                                                                                                                                                |
| ------------------------ | ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------ |
| `slang-relative-ptr.h`   | Complete `Slang::detail::RelativePtr32Traits`, `Slang::detail::RelativePtr64Traits`, `Slang::RelativePtr<T, Traits>`, and the `Slang::RelativePtr32<T>` and `Slang::RelativePtr64<T>` alias templates. |
| `slang-relative-ptr.cpp` | The include-only translation unit; all pointer operations are currently defined in the header, with no out-of-line implementation to assign separately.                                                |

The template assignment includes all five public aliases, the private default-initialized `_offset`, every constructor and assignment overload, `get`, `set`, the implicit pointer conversion, and the arrow operation.
Fossil's derived wrapper and schemas remain with Module Storage; Blob Composition's displacement emission and Offset Containers' explicit-base types remain with their sibling leaves.

## Preservation and Migration Holds

All existing names, signatures, defining identities, alias-template identities, access, class layout and member order, default initialization, inline choices, and behavior remain constraints on a future port.
That includes the `Slang` and `Slang::detail` declarations, void assignment results, implicit conversions, move behavior, store-before-assert sequence, and operation-specific custom-trait expressions.
The [root's staged migration contracts](../../../slang.md#staged-migration-contracts) require canonical C++ identity and binary-interface preservation wherever these declarations cross an existing binary interface; this document establishes no permission to substitute replacement types.

The current static `core` target selects `slang-relative-ptr.cpp` through the legacy core source glob.
Enabled `slang-rt` independently selects that directory through `EXTRA_SOURCE_DIRS` under its runtime export settings.
Both construction consumers remain part of any future source-selection change, even though this translation unit contains only an include.

The leaf's complete-class ownership requires no further service split or new sibling dependency cycle.
Its real Failure Reporting dependency still reaches Foundation's unresolved [failure/text/native-ownership cycle](../../foundation.md#failure-reporting-and-basic-data-services).
The [root artifact and source-reuse questions](../../../slang.md#unresolved-architectural-dependencies) also remain open.
This refinement selects no behavior repair, stronger validation, replacement pointer interface, or code port.
