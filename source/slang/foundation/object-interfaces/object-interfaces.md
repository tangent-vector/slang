<!--
SPDX-FileCopyrightText: The Khronos Group, Inc.
SPDX-License-Identifier: CC-BY-4.0
-->

# Object Interfaces

Object Interfaces helps implementations expose and retain product interfaces, obtain borrowed access to supported interfaces or implementation objects, and request clones through an existing cloning contract.
It also wraps a native value in a reference-counted interface and adapts an existing product object to borrowed interface lookup.
The concrete object determines which interfaces it supports and which data it owns.

The implementation remains in `old-source/core/`.
This document refines [Foundation's assignment](../foundation.md#existing-implementation-mapping); the [failure-reporting and native-ownership cycle](#unresolved-boundaries) remains a migration constraint.
The product declarations in `include/` remain existing external contracts rather than implementation files assigned to this module.

## Concepts

[Foundation](../foundation.md#concepts) defines intrusive references and borrowed views.
[Memory](../memory/memory.md#native-object-lifetimes) describes native C++ retention through `RefObject` and `RefPtr`.

- A **product interface reference** retains an object through its `ISlangUnknown::addRef` and `release` operations.
  The product `ComPtr` wrapper manages those operations; this is a different protocol from native `addReference` and `releaseReference`, even when one object implements both.
- **Interface discovery** requests a supported interface by its existing UUID through `queryInterface`.
  The shared implementations described below add a product interface reference on success.
- **Borrowed interface lookup** uses the product `ISlangCastable::castAs` operation without adding a reference.
  Its result may identify an interface, an implementation object, or contained data rather than an `ISlangUnknown` subobject.
  The provider's lifetime contract determines how long that pointer remains valid.
- A **clone** is an object requested through `ISlangClonable::clone` for a specified interface or object UUID.
  The product contract returns the requested pointer without adding a reference; the concrete implementation determines how its state is copied.
- A **boxed value** is a value stored in a reference-counted `BoxValue<T>` object and accessed through `IBoxValueBase` or its typed `IBoxValue<T>` view.
  The box exposes the contained value's address and type UUID without requiring the contained type to implement a product interface.

The `ICastable` and `IClonable` names are aliases of the existing product interfaces, not new interface identities.
A retained wrapper does not necessarily own everything reachable through that wrapper.
For example, Blobs' `UnownedRawBlob` uses this module's reference-counting base for its wrapper but borrows the caller's bytes.
The byte owner must keep that storage valid independently of the wrapper's reference count.

## Dependencies

- [Native Ownership](../memory/native-ownership/native-ownership.md) supplies the `RefObject` base and native reference operations used by `ComObject` to connect product references to native ownership.
  The standalone `ComBaseObject` instead owns its own atomic product reference count and does not derive from `RefObject`.
- [Failure Reporting](../failure-reporting/failure-reporting.md) supplies the standalone base's reference-count assertion and the precondition checks used by castable adaptation and interface cloning.
  Ordinary assertions remain a debug-build service dependency even though release builds use optimizer assumptions.
- The product headers supply UUIDs and their comparison, interface declarations, result codes, calling conventions, compiler annotations, and `ComPtr` retention and output-pointer operations.
  `include/slang-com-helper.h` supplies the `Guid` alias and UUID comparison operations; `include/slang-com-ptr.h` supplies the retaining wrapper used for adapters and cloned interfaces.
  Those dependencies do not require the compiler implementation or assign these public headers to Object Interfaces.
- The host C++ environment supplies atomic counters, allocation and deletion of concrete objects, inheritance, and construction and destruction of boxed values.
  The type supplied by a client determines the boxed value's own construction, copying, destruction, and referent lifetimes.

The `slang-basic.h` aggregate currently includes collections, strings, hashing, and other services.
The assigned operations use native ownership and failure reporting from that aggregate; the include alone does not establish direct dependencies on the other services.
For example, boxing Compiler Support's `SourceMap` does not make Object Interfaces depend on source-map interpretation, its text storage, or its collections.

## Service Areas

Object Interfaces is a proposed leaf with no child modules.
The box and unknown adapter reuse the standalone reference base, and the cast and clone wrappers use the existing product protocols.
We keep these implementation variants together because they provide object access and retention while clients choose payload types and application policy.
Its services are public within Foundation because blobs, compiler-support artifacts, compiler objects, adapters, and graphics tools use them directly.
The protected lookup hooks and adapter cache remain implementation details, not additional public services.

### Product References and Native Ownership

`ComBaseObject` supplies a standalone atomic counter initialized to zero.
Its associated macros implement interface discovery, retention, and release; the last release deletes the object through the base's virtual destructor.
Copy construction starts a new zero counter, and assignment leaves the destination's counter unchanged.

`ComObject` supplies a separate atomic product counter on a native `RefObject`.
The first product reference adds one native reference, and the last product release invokes `comFree` before releasing that native reference.
Native clients can retain additional references, so the last product release need not destroy the object.
For example, the Vulkan command buffer's `comFree` breaks its strong reference to a transient heap when the product count reaches zero, even if native owners still retain the command buffer.
Copy construction starts both counters at zero; assignment leaves the destination's existing counts unchanged.

Both query-macro families call a concrete object's `getInterface` hook.
A non-null result causes the macro to add one product reference, write the interface pointer through `outObject`, and return `SLANG_OK`.
An unsupported UUID returns `SLANG_E_NO_INTERFACE` without writing `outObject`.
The macros do not check the output pointer before writing it on success.
`ComPtr::writeRef` separately releases its previous reference and initializes its stored pointer to null before supplying the output address.
That preparation explains why callers using `writeRef` can observe null after a failed query; it is not a guarantee that the query macro clears arbitrary output storage.

### Borrowed Lookup and Adaptation

The `dynamicCast<T>` and `as<T>` overloads taking `ICastable*` return null for a null input and otherwise request `T::getTypeGuid()` through `castAs`.
They reinterpret a non-null result as `T*` without retaining it or independently validating its C++ type.
The similarly named overloads taking `RefObject*` remain Native Ownership services with their own native cast contract.

`CastableUtil::getCastable` requires a non-null product object.
It first requests an existing castable interface into a `ComPtr`; a failed query creates an `UnknownCastableAdapter` retaining the supplied object instead.
The successful-query branch asserts that the result is non-null.
The adapter returns its own interfaces before consulting the contained object, so asking the adapter for `ISlangUnknown` identifies the adapter.
Its `getContained` operation exposes the retained object's pointer without adding another reference.

The adapter reuses its last successful forwarded result when the requested UUID matches the cached UUID.
On a cache miss, it queries the contained object into a temporary `ComPtr`, records a successful non-null pointer and UUID in the cache, and releases the queried reference when the temporary leaves scope.
An unsuccessful query or a null queried pointer returns null without replacing the cached entry.
The cached pointer and the returned pointer are borrowed.
The implementation therefore requires the queried interface to remain valid after that temporary reference is released while the contained object remains retained.
The adapter does not support a provider whose queried interface has an independent lifetime that ends at that release.
Atomic reference counts do not synchronize the adapter's mutable lookup cache or the wrapped object's contents.

### Boxed Access and Cloning

`BoxValue<T>` stores a `T` member; its default constructor default-initializes that member, and its constructor taking `const T&` initializes the member from the supplied value.
`getValuePtr`, typed accessors, and lookup by `T::getTypeGuid()` borrow access to that member.
`asBoxValue<T>` first obtains `IBoxValueBase`, then checks the reported contained-type UUID before returning the typed box view.
It returns null when that lookup returns null or the contained-type UUID differs.
These templates require the used declarations and expressions to be well formed: `T` must supply the requested type UUID, and the chosen constructor must support the corresponding initialization.
The typed interface forms `T*`, so substituting a reference type for `T` is not supported.
The contained type's copy and destructor behavior determines what happens to any objects or storage it references; the box does not impose deep-copy or ownership-transfer semantics on those referents.

For example, artifact handling constructs a `BoxValue<SourceMap>`, fills its contained map, and retains the box in an artifact's castable representations.
The artifact and source-map services choose the contents and retention period; Object Interfaces supplies the wrapper and borrowed typed access.

The `clone<T>` helper requires a non-null `IClonable`, requests `T::getTypeGuid()`, and returns the resulting unretained pointer without checking the result for null.
The caller supplies the lifetime management appropriate to that returned object or interface.
`cloneInterface<T>` requires a non-null input implicitly convertible to `IClonable*`, asserts that the returned pointer is non-null, and constructs a `ComPtr<T>` that adds the first product reference.
That helper requires a type usable through the product reference-counting layout, whereas the raw `clone<T>` helper can request an implementation object.
Both helpers rely on the clone provider to return the type identified by `T::getTypeGuid()`; the pointer cast does not verify that correspondence.
Neither helper defines how much state the concrete clone copies.
For example, `ArtifactDiagnostics::clone` constructs a separate diagnostics object, and the test runner retains that clone while removing entries to calculate a summary without modifying the original diagnostics.

## Existing Implementation Mapping

The following paths are relative to `old-source/core/`.
The assignment covers the complete declarations and definitions in the first three files, including inline operations and protected implementation support.

| Existing Implementation                                                         | Object Interfaces Responsibility                                                                                                                                                                                                    |
| ------------------------------------------------------------------------------- | ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| `slang-com-object.h`                                                            | `ComBaseObject`, `ComObject`, their counters, construction and assignment, release behavior, native-reference bridge, `comFree` hook, and both associated `SLANG_COM_*_IUNKNOWN_*` macro families.                                  |
| `slang-castable.h`                                                              | Borrowed `ICastable` cast overloads, `IBoxValueBase`, `IBoxValue<T>`, `asBoxValue`, `BoxValue<T>`, `IUnknownCastableAdapter`, `UnknownCastableAdapter`, `CastableUtil`, `cloneInterface`, and their inline and template operations. |
| `slang-castable.cpp`                                                            | Existing-interface selection, adapter construction, adapter-first and forwarded lookup, and its borrowed one-entry cache.                                                                                                           |
| Only the `ICastable` and `IClonable` aliases and `clone<T>` in `slang-common.h` | Convenient names for the existing product contracts and the raw clone request helper.                                                                                                                                               |

The C++ utility, assertion, assumption, and export declarations remaining in `slang-common.h` retain [Foundation's other assignments](../foundation.md#existing-implementation-mapping).
That mixed header also exposes the scalar declarations through its include of Scalar Types' defining header, `source/slang/foundation/base-types/scalar-types/scalar-types.h`.
`RefObject`, `RefPtr`, and the native cast overloads in `slang-smart-pointer.h` remain with Native Ownership.
Concrete blobs, files, artifacts, compiler objects, and graphics objects remain clients; their interface lists, payloads, cloning policy, and `comFree` overrides are not assigned to this module.
The general interface macros and smart pointer in `include/slang-com-helper.h` and `include/slang-com-ptr.h` remain existing product contracts, including their native-reference helper macros.

The current CMake source selection includes `slang-castable.cpp` in static `core` and recompiles the same implementation into enabled `slang-rt` under runtime export settings.
The bases and templates are also instantiated by their header clients.
The [root's staged migration contracts](../../slang.md#staged-migration-contracts) require preserving both compilation consumers and auditing binary-interface uses before changing any class's defining namespace, layout, signatures, calling conventions, or annotations.
No new interface, UUID, class identity, counter policy, or product export is selected by this ownership assignment.

## Unresolved Boundaries

Object Interfaces directly uses Failure Reporting for assertions, and `ComObject` uses Native Ownership's checked last-reference release.
Failure Reporting constructs and retains Text's `String` messages, and `StringRepresentation` derives from Native Ownership's `RefObject`.
The Native Ownership to Failure Reporting to Text to Native Ownership cycle therefore constrains this module's dependencies even though Object Interfaces does not itself store a string.
Foundation must settle a behavior-preserving separation or a coherent owner for those mutually dependent services before the affected boundary can migrate.
Replacing an assertion mechanism, removing a reference-count check, or changing exception and message ownership is not part of this refinement.

The existing box-base and unknown-adapter interface declarations use the same UUID.
Their declarations remain distinct C++ types, but UUID lookup alone does not distinguish those two requests.
This document records their existing representation rather than selecting a new identifier or promising stronger lookup validation.
Their migration must preserve the current declarations and behavior unless a separate change is authorized.

Separating the three Object Interfaces declarations from `slang-common.h` is a proposed partial-file extraction, not a completed physical split of that mixed header.
Likewise, replacing the broad `slang-basic.h` include with narrower prerequisites would not remove the behavioral failure-reporting cycle.
The leaf description establishes phase-one ownership and contracts; it does not establish an acyclic migrated implementation.
