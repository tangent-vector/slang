# SPIR-V Grammar

SPIR-V Grammar supplies descriptions of SPIR-V instructions and operand kinds for clients that need to recognize instruction names, locate result operands, or interpret named enumerants.
It loads those descriptions from JSON and exposes the same lookup operations for descriptions embedded in generated C++.
For example, the compiler uses the description of `OpIAdd` to place an explicitly named result in an inline SPIR-V instruction; choosing and checking Slang expressions remains the compiler's responsibility.

This is a proposed public leaf module of [Compiler Support](../compiler-support.md).
Its shared declarations and JSON loader remain in `old-source/compiler-core/`.
The generated embedded implementation is currently compiled with Compiler, so ownership of the complete service remains [unresolved](#generated-implementation-and-ownership).
No implementation has moved into this directory.

## Concepts

A **grammar description**, represented by `SPIRVCoreGrammarInfo`, is a reference-counted collection of name and numeric-value lookups derived from a SPIR-V core grammar JSON document.
It records the subset of the grammar needed by its clients, rather than a complete SPIR-V specification or validation model.
`SpvWord` is the existing alias for the unsigned 32-bit value used for enumerants and instruction operands.
Instruction opcodes and capability values use the external `SpvOp` and `SpvCapability` types.

An **operand kind** identifies a category named in the JSON document's `operand_kinds` array, such as `IdRef`, `MemorySemantics`, or `PairIdRefIdRef`.
`SPIRVCoreGrammarInfo::OperandKind` uses an eight-bit index assigned from that array order.
That index belongs to a particular grammar description rather than the SPIR-V binary encoding.
The type does not establish a shared index assignment between independently loaded descriptions.
A **qualified enumerant name**, represented by `QualifiedEnumName`, combines an operand kind with an unqualified enumerant spelling.
`QualifiedEnumValue` combines the kind with a numeric value for reverse lookup.

An **instruction description**, represented by `OpInfo`, records the instruction's selected classification, result-type and result-ID positions, operand-count bounds, and sequence of operand kinds.
The result positions count from zero within the operands, excluding the opcode; `-1` denotes an absent result operand.
Operand counts include any result type and result ID, and count each grammar operand once even when its binary encoding occupies several words.
The description does not expand a composite operand kind into its bases or an enumerant into its parameter operands.
For example, the `OpIAdd` entry lists `IdResultType`, `IdResult`, `IdRef`, and `IdRef`.
Its description places the result type at position zero and result ID at position one, with both operand-count bounds equal to four.

A **lookup**, represented by `Lookup<K, T>`, returns a `std::optional<T>` containing a copy of the mapped value, or `std::nullopt` when its key is absent.
When the lookup's `embedded` function pointer is non-null, `lookup` calls that function; otherwise it queries `dict`.
A miss from the function does not fall back to the dictionary.
The copied value can contain borrowed strings or pointers, so returning a value does not give the caller ownership of its backing storage.

## Dependencies

- [JSON](../json/json.md) supplies lexing, parsing, JSON storage, and conversion through Foundation's native type descriptions.
  The grammar loader describes the JSON fields it uses and converts them into its lookup representation.
- [Source Text](../source-text/source-text.md) supplies the caller's `SourceView` and the source-manager context needed to read JSON and interpret diagnostic locations.
- [Diagnostics](../diagnostics/diagnostics.md) supplies the sink and the grammar-specific error description used alongside JSON diagnostics.
- Foundation supplies [Text](../../foundation/text/text.md) for strings, slices, pooled string storage, and integer parsing; [Collections](../../foundation/collections/collections.md) and [Hashing](../../foundation/hashing/hashing.md) for indexed storage and keys; and [Memory](../../foundation/memory/memory.md) for native reference-counted ownership.
  Its [Type Descriptions](../../foundation/type-descriptions/type-descriptions.md) support the private JSON schema records.
  [Base Types](../../foundation/base-types/base-types.md) supplies scalar conventions, and [Failure Reporting](../../foundation/failure-reporting/failure-reporting.md) supplies the assertions used by loading and lookup storage.
- SPIR-V Headers supplies the opcode and capability enum definitions exposed by the shared header, and the core grammar JSON used by the embedding generator.
  The host C++ environment supplies optional results and numeric limits.
- The existing `tools/slang-spirv-embed-generator/` tool constructs the embedded lookup implementation.
  It uses [Lookup Generation](../lookup-generation/lookup-generation.md) to generate string-keyed lookup functions; the ownership constraint below governs how that build dependency can be represented.

The loader accepts a view of JSON text rather than a filename and does not choose a file system or search path.
Compiler session configuration and the generator each obtain their own text and diagnostic context before calling the loader.
Neither caller's configuration policy belongs to this leaf.

## Services and Representation

SPIR-V Grammar has no child modules.
Its principal service areas are JSON loading, lookup of instruction and operand metadata, and the shared access contract for generated descriptions.

### Name and Metadata Lookup

The ten public lookup members of `SPIRVCoreGrammarInfo` provide the following operations:

| Members                            | Mapping                                                                                                  |
| ---------------------------------- | -------------------------------------------------------------------------------------------------------- |
| `opcodes`, `opNames`               | Instruction spelling to opcode, and opcode to one instruction spelling.                                  |
| `opInfos`                          | Opcode to its instruction description.                                                                   |
| `capabilities`                     | Capability spelling to its SPIR-V capability value.                                                      |
| `operandKinds`, `operandKindNames` | Operand-kind spelling to its index, and index to spelling.                                               |
| `allEnums`, `allEnumNames`         | Qualified enumerant name to value, and qualified enumerant value to one unqualified spelling.            |
| `allEnumsWithTypePrefix`           | Concatenated operand-kind spelling and enumerant spelling to value, without a separator.                 |
| `operandKindUnderneathIds`         | An operand kind whose spelling starts with `Id` to the kind named by the remaining suffix, when present. |

For example, `MemorySemantics` with enumerant `Acquire` can be queried through `allEnums`, or as `MemorySemanticsAcquire` through `allEnumsWithTypePrefix`.
`IdMemorySemantics` maps to `MemorySemantics` through `operandKindUnderneathIds` because both kind spellings occur in the grammar.
The loader infers that relationship from spellings; it does not inspect a semantic definition of ID wrappers.

Instruction and enumerant aliases are accepted as additional forward spellings.
For repeated opcode values, the loader keeps the first JSON instruction record's `OpInfo` and primary instruction spelling.
For repeated enumerant values within a kind, `allEnumNames` keeps the first spelling inserted from an intermediate dictionary.
That reverse enumerant lookup does not promise the primary spelling or a particular alias.

`OpInfo::Class` declares more classifications than the loader recognizes.
The loader assigns `TypeDeclaration`, `ConstantCreation`, `Debug`, and `Annotation` for the corresponding JSON class strings, and assigns `Other` for every other class string.
For example, `OpIAdd` has the JSON class `Arithmetic`, but its loaded `OpInfo::class_` is `Other`.
Clients that choose instruction placement use these selected classifications together with their own opcode rules.

For ordinary required operand entries, the loader increases both operand-count bounds.
An optional `?` entry increases only the maximum; a repeated `*` entry, or an `ImageOperands` entry, sets the maximum to `0xffff` to represent no fixed bound in this description.
`numOperandTypes` counts the stored kind entries, rather than the maximum number of operands.
Clients interpreting a trailing repeated operand use the last stored kind for the remaining operands; `Lookup::lookup` does not perform that indexing for them.

### JSON Loading and Lifetimes

`loadFromJSON(SourceView&, DiagnosticSink&)` requires the view and sink to use the same source manager, as checked by an assertion.
It parses JSON, converts the required and optional fields into private C++ schema records, then constructs a new dictionary-backed grammar description.
The schema records ignore unrecognized JSON fields.
They read instruction and enumerant capability lists, operand categories, and instruction printing classes, but the loader does not retain those fields in the result.
Version and extension restrictions, enumerant parameter lists, and composite-kind bases likewise do not become lookup constraints.
The capability-name table describes capability numbers; it does not decide whether an instruction is available for a selected target.

Enumerant values can be JSON integers or strings parsed as integers, including the hexadecimal spellings used in the grammar data.
JSON parse or native-conversion failure returns a null pointer.
Native-conversion failure also produces a grammar-specific schema diagnostic.
Later construction checks diagnose conditions including unknown operand kinds, unsupported quantifier spellings or ordering, too many operand kinds, and invalid enumerant values, but can continue constructing and return a non-null description.
A non-null return therefore does not establish that the sink received no errors or that the input was fully validated.
The narrow result-position and count fields, and the integer casts used during loading, do not establish general range validation.

JSON conversion materializes the retained string slices in the JSON container's string pool.
Before the temporary container is destroyed, the loader transfers that pool to the grammar description, including the concatenated enumerant names it creates.
The loader also reserves the operand-kind array before storing pointers into it in `OpInfo` values.
The returned description therefore retains the string and operand-kind storage needed by its lookups without retaining the input view or its source manager.
For a JSON-loaded description, a client holding a returned slice or `OpInfo::operandTypes` pointer must keep the description alive and avoid invalidating its storage.
The public dictionaries and function pointers remain mutable; the type does not provide an immutable or synchronized update interface.

### Generated Implementation and Ownership

The embedding generator calls the same JSON loader and translates the populated dictionaries into C++ lookup functions.
String-keyed lookups use generated perfect-hash functions; numeric and composite-value lookups use generated switches, with static strings and operand-kind arrays backing the returned values.
The generated `getEmbeddedVersion` definition creates a dictionary-empty description whose ten lookup function pointers select those implementations.
Its function-local `RefPtr<SPIRVCoreGrammarInfo>` is initialized once and returned by mutable reference.

`freeEmbeddedGrammerInfo`, with its existing spelling, assigns null to that retained pointer.
Compiler shutdown calls the operation through `slang_shutdown`.
Other retained references, including session references, keep their description alive; clearing the accessor's pointer does not clear them.
Calling `getEmbeddedVersion` again does not rerun the initializer or recreate the cleared description.
Callers can assign through its returned reference, but the accessor does not establish a reset-and-recreate lifecycle.

The current construction has three distinct participants:

1. `old-source/compiler-core/CMakeLists.txt` compiles the shared JSON-loader implementation into static `compiler-core` and exposes SPIR-V header includes to its clients.
2. `tools/CMakeLists.txt` builds `slang-spirv-embed-generator` with `compiler-core`, or imports an existing generator when `SLANG_GENERATORS_PATH` is supplied.
3. `old-source/slang/CMakeLists.txt` invokes that generator on `spirv/unified1/spirv.core.grammar.json` beneath `SLANG_SPIRV_HEADERS_INCLUDE_DIR`.
   It compiles the resulting `slang-spirv-core-grammar-embed.cpp` in the `slang-lookup-tables` object target and includes that target in the compiler variants.
   The bootstrap executable also explicitly links `slang-lookup-tables`.

[Compiler Support's assignment](../compiler-support.md#application-protocols-and-generated-grammar) places the shared descriptions and JSON loader in this module, while the current generated definition is built under [Compiler](../../compiler/compiler.md).
Moving the shared pair alone would leave the definition of its embedded accessor, its generator's use of the loader, and their build consumers split across owners.
Those relationships require an ancestor decision before migration of the complete service.
This document does not select a new library, relocate the generator, or establish an acyclic ownership graph by treating the generated object as an independent artifact.

## Existing Implementation Mapping

Paths in this section name the current implementation; the assignments do not change declarations, behavior, ABI, or build selection.

| Existing file or region                                                                            | Responsibility                                                                                                                                                                                                                     |
| -------------------------------------------------------------------------------------------------- | ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| `old-source/compiler-core/slang-spirv-core-grammar.h`, complete file                               | Shared grammar description, lookup contract, operand and enumerant keys, instruction metadata, embedded-accessor declaration, and inline clear operation.                                                                          |
| `old-source/compiler-core/slang-spirv-core-grammar.cpp`, complete file                             | Private JSON schema records and their native descriptions, enumerant conversion, and construction of the dictionary-backed grammar description.                                                                                    |
| `tools/slang-spirv-embed-generator/spirv-embed-generator-main.cpp`, complete file                  | Existing external generator participant; its dictionary translation, generated lookup functions and accessor, file handling, and executable entry point are described here without assigning the tool implementation to this leaf. |
| `old-source/slang/CMakeLists.txt`, grammar custom command and `slang-lookup-tables` inputs         | Current construction and compilation of the generated accessor and lookup functions; the object target also contains separately generated GLSL extended-instruction lookups.                                                       |
| `old-source/compiler-core/CMakeLists.txt` and `tools/CMakeLists.txt`, relevant target construction | Current compilation of the loader and construction or import of its embedding generator.                                                                                                                                           |

Compiler owns the clients that interpret these descriptions.
`Session::setSPIRVCoreGrammar` selects and retains the embedded description or loads a caller-named JSON file.
File-read failure returns before replacing the selected description, whereas a null loader result replaces the session's retained pointer with null.
The setter's final result tests that pointer rather than the diagnostic error count, so a non-null description constructed after diagnostics can still produce `SLANG_OK`.
`parseSPIRVAsmInst` uses opcode descriptions to place result operands and guide operand-count recovery; `SemanticsExprVisitor::visitSPIRVAsmExpr` checks operands and resolves enumerants using the selected grammar.
`SpvSnippet::parse` resolves instruction names for intrinsic snippets, and SPIR-V emission uses instruction classifications and result positions when placing and reusing emitted instructions.
Those files are partial client references, not additional implementation assignments to SPIR-V Grammar.
