# Names

Names gives compiler and tool clients shared identities for text spellings and access to the retained text.
Clients use those identities for token contents and spelling-based tables; declaration lookup and the choice of spelling remain with the client.

This is one public leaf of [Compiler Support](../compiler-support.md).
The implementation remains in `old-source/compiler-core/`.

## Concepts

Compiler Support defines an [interned name](../compiler-support.md#concepts).
A `Name*` identifies a spelling rather than a declaration: two declarations named `value` in different scopes can share the same name object.

A **name pool** (`NamePool`) is a spelling-to-name table that retains the name objects it contains.
Repeated lookup of the same spelling returns the existing pointer while that entry remains in the pool.
Independently populated pools need not return the same pointer for equal spellings.
Clients that compare name pointers therefore obtain their names through the same pool and keep the relevant name objects alive.
The current declarations expose both the table and `Name::text` publicly; preserving the association between an interned spelling and its pointer relies on clients leaving existing entries and spellings unchanged.

[Foundation](../../foundation/foundation.md#concepts) defines retained strings, intrusive references, and borrowed string slices.
When interning creates a name, it copies borrowed input characters into retained storage, so the input buffer need only survive the lookup call.
The returned `Name*` is borrowed, whereas the pool retains each `Name` through a `RefPtr`.
An additional retained reference can extend the name object's lifetime beyond its pool entry.
A text result returned as `String` retains the characters independently of the name object; a slice or C string returned by the text-access helpers borrows the name's character storage.

## Dependencies

- [Text](../../foundation/text/text.md) supplies retained spelling storage, borrowed slices, and the string comparison and hashing used for spelling keys.
- [Memory](../../foundation/memory/memory.md) supplies intrusive reference counting and the retained pointers that keep name objects alive.
- [Collections](../../foundation/collections/collections.md) supplies the spelling-to-name dictionary.
- The host C++ environment supplies allocation and destruction of individual name objects.

The broad `slang-basic.h` include does not assign all Foundation services as dependencies of Names.
Lexing, Tokens, Diagnostics, compiler builders, and serialization are clients of Names rather than services that interning requires.

## Service Areas

Names has no child modules.
Its services are pool lookup with optional insertion, and access to a name's text through retained or borrowed results.
Lookup uses Text's case-sensitive string-key behavior without identifier validation or spelling conversion.

Compiler owns the policy for sharing pools across its clients.
Its global `Session` contains a pool, and `Linkage` and `SharedASTBuilder` obtain that same pool rather than allocating names in AST-builder storage.
For example, the lexer interns an identifier using the supplied pool, and compiler declaration tables use the resulting pointer as a key.
AST and IR deserialization intern stored spellings into the receiving client's pool; serialized names do not preserve process-local pointer addresses.
Standalone generator tools also supply their own pools.

## Existing Implementation Mapping

All declarations and definitions in `old-source/compiler-core/slang-name.h` and `slang-name.cpp` belong here: `Name`, `NamePool`, and the text-access helpers.
The current `compiler-core` static target compiles the implementation through its directory source selection.
Compiler variants and generator tools link that target; Names does not introduce a separate library.
The header has no product-export annotations and is not among the installed product headers.

Pool ownership in compiler sessions and builders, name-bearing tokens, AST `NameLoc`, diagnostic formatting, and AST/IR serialization remain with their existing owners.
The serialization customizations for `Name` remain Compiler-owned and must refer to the same name type used by its clients after relocation.
Foundation's general string interning and name/value tables, and the spelling-convention utility assigned to [Text](../compiler-support.md#reusable-text-and-shared-compilation-vocabulary), remain separate services.
