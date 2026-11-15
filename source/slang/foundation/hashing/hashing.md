<!--
SPDX-FileCopyrightText: The Khronos Group, Inc.
SPDX-License-Identifier: CC-BY-4.0
-->

# Hashing

Hashing computes and combines numeric summaries of values for collection lookup and reproducible identifiers.
It provides ordinary hashes selected for each C++ type and separate fixed algorithms whose results appear in generated programs, API results, and stored data.
The caller owns the values, their equality rules, and the choice of information to hash.

This document assigns the existing header-only implementation to one public leaf of [Foundation](../foundation.md).
The implementation remains in `old-source/core/`; no Hashing code has moved into this directory.

## Concepts

The [Foundation concepts](../foundation.md#concepts) distinguish a hash code, a stable hash, and a digest.
Hashing uses the following more specific terms:

- An **ordinary hash** is a numeric hash for lookup within a running program.
  Its value may depend on the hashing-library version or an address and is unsuitable as a persistent identifier.
  `HashCode` and `HashCode64` are aliases of the same unsigned 64-bit type; `HashCode32` is an unsigned 32-bit type.
- A **stable hash code** is a distinct 32-bit or 64-bit value computed by the fixed algorithms in `slang-stable-hash.h`.
  The separate wrapper types prevent implicit interchange with ordinary integer hashes.
  Their compatibility significance and input-representation limits are described [below](#compatibility-and-input-representation).
- A **uniform hash** is a hash advertised to the hash-table backend as already well mixed.
  The existing `is_avalanching` marker lets the backend select how to use or further mix the result; it is not a promise of collision freedom or persistence.

## Dependencies

- The C++ environment supplies fixed-width integers, type traits, byte copying, iterator operations, and string views.
  The byte-range operations borrow readable storage for the duration of the call.
- `unordered_dense`, available through [Foundation](../foundation.md#dependencies), supplies ordinary hashing for supported primitive, pointer, enum, and string-view values, together with its uniformity marker convention.
  Clients instantiate these headers, so the existing `core` and `slang-rt` targets expose that dependency publicly.

The existing `slang-math.h` and product-header includes do not establish additional service dependencies: these definitions call no math, allocation, failure-reporting, collection, or text service.
The generic hashing contract calls a client's `getHashCode()` or specialization when supplied; the type-specific meaning remains with the client's module.
For example, Text hashes character contents and Memory hashes smart pointers by the underlying address.

## Service Areas

Hashing has no child modules.
Its closely related services share the distinction between computing a value's hash and combining existing hashes:

- **Ordinary value hashing** selects a type's `getHashCode()` ahead of a supported uniform hash from `unordered_dense`.
  An explicit `Hash<T>` specialization can supply type-specific behavior instead.
  The byte-range and restricted object-byte operations provide the common machinery used by strings and other keys; the member-generation macros adapt that machinery to client types.
- **Composition** combines hashes in order, accumulates them through `Hasher`, or computes a range hash independent of element order through `symmetricHash`.
  The compiler's capability sets use the last operation because dictionary iteration order does not determine set equality.
- **Stable computation and composition** produce the separate fixed-width wrapper values and combine values of the same stable-hash type.
  These operations use their own fixed arithmetic rather than the ordinary type-selection mechanism.

For collection lookup, clients ensure that values accepted as equal by their chosen comparison produce equal hashes, including heterogeneous lookup keys.
For example, `RefPtr<T>` deliberately hashes like its underlying `T*`.
The float and double specializations instead hash their bit representations and distinguish positive and negative zero, so their hashes require a comparison that respects that distinction when used as collection keys.
The object-byte operations require the C++ unique-object-representation trait; componentwise hashing hashes member values instead of the complete object representation.

## Compatibility and Input Representation

Stable results already affect external behavior.
`spComputeStringHash` returns the 32-bit algorithm through the product API, and compiler emitters use that algorithm for string-literal hash instructions.
`getHashedName` uses the 64-bit algorithm in generated symbol names.
`ReproUtil::Header` stores a `StableHashCode32`, and the exported `ReproUtil::getTypeHash()` returns that type.
Migration therefore preserves the existing algorithms, widths, wrapper layouts, and the canonical type identity required by the [root ABI policy](../../slang.md#staged-migration-contracts).
Text continues to own formatting these numeric values; compiler and file-system clients continue to own the identifiers and stored formats that use them.

The stable header states an intent to produce the same result on all targets, but the implementation does not canonicalize its input representation.
The byte loops convert plain `char` directly to the hash width, so bytes above `0x7f` can produce different results when targets disagree on `char` signedness.
The object overloads hash native object bytes; their unique-representation check does not guarantee the same byte order or type size across targets.
Reproducibility across such targets therefore requires callers to supply a consistent representation within those limits.
These representation limits are part of the existing behavior to preserve during structural migration.

## Existing Implementation Mapping

- All declarations and definitions in `old-source/core/slang-hash.h` belong here: ordinary hash values, selection and uniformity adaptation, byte and component adaptation, composition, and accumulation.
- All declarations and definitions in `old-source/core/slang-stable-hash.h` belong here: stable value wrappers and fixed computation and combination.

Both files contain inline or template implementations, with no separate Hashing object file or library.
Their current `Slang` namespace also supplies an extension point: `Hash<ValKey>` in `old-source/slang/slang-ast-builder.h` supports heterogeneous AST lookup.
That specialization remains Compiler-owned and must specialize the same template that collections use after relocation.
The member-generation macros must also continue to resolve their hashing helpers after namespace qualification changes.
Type-owned hash methods, stable-value text formatting, the compiler's perfect-hash table generator, and product API entry points remain with their respective owners.
The two internal headers are not installed product headers; that does not remove the observed API-result, binary-interface, and stored-data constraints.
