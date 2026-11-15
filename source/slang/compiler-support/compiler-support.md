<!--
SPDX-FileCopyrightText: The Khronos Group, Inc.
SPDX-License-Identifier: CC-BY-4.0
-->

# Compiler Support

Compiler support provides shared services for reading program text, reporting diagnostics, representing compilation outputs, and communicating with compilation tools, including their shared integration contracts.
It also provides JSON and protocol data services used by compiler clients and development tools.

This is a proposed decomposition of the existing `compiler-core` static library.
The implementation remains in `old-source/`; the accepted concrete-tool assignment and the remaining ancestor questions are recorded below.
Parsing declarations, semantic checking, Slang IR, and producing shader reflection information remain responsibilities of [Compiler](../compiler/compiler.md).

## Concepts

- A **source file** stores program text and its path information, or information about that text when the contents are unavailable.
  `SourceFile` can receive contents after construction and retains the content blob.
  A blob can borrow its bytes, so the caller must keep any borrowed backing storage valid while the text can be read through the file.
- A **source view** assigns a distinct range of source locations to one use of a source file and records interpretations such as line directives and the location that initiated an include or expansion.
  A **source location** is the compact position interpreted through a view and its `SourceManager`; it is not a self-contained filename and byte offset.
  The manager owns its files and views, and resetting or destroying the manager invalidates those objects and the interpretation of its allocated locations.
- A **source map** records correspondences from generated lines and columns to originating files, lines, columns, and optional names.
  The map representation is independent of its JSON encoding.
- An **interned name** is a text spelling represented by a `Name` obtained from a `NamePool`.
  The pool retains its names and returns the same name for repeated lookup of the same spelling within that pool; callers comparing name pointers need a shared pool and a lifetime that keeps those names alive.
- A **token** records a lexical category, flags, location, and spelling.
  Its spelling either refers to character storage or to an interned name; copying a token does not retain either backing allocation.
- A **diagnostic** reports a message with a severity and optional location or annotated ranges.
  The diagnostic sink applies configured warning groups and severity overrides, formats messages, counts errors, and routes output.
  Semantic checking determines which semantic conditions deserve diagnostics; the shared services also report failures in lexing, JSON, and command arguments.
- A **compilation artifact**, represented by `IArtifact`, describes an input, output, or related item of compilation.
  A **representation** expresses the whole artifact, such as a blob, a file, or a callable library; associated artifacts carry related information, and child artifacts form a contained hierarchy.
  This use of “artifact” differs from the root's [build artifact](../slang.md#concepts), which names an implementation library or executable.
- A **downstream compiler** implements the shared operations for compiling or converting compilation artifacts, with optional validation, disassembly, and linking operations.
  Its description identifies the compiler kind and version.
  The versioned request contains borrowed slices and pointers; its size and identifier govern compatibility across shared-library boundaries rather than transferring ownership of its inputs.
- A **JSON value** can contain a scalar directly, refer to an undecoded lexeme through a source location, or refer to strings and aggregate storage in a `JSONContainer`.
  Its backing container and any referenced program text must remain available until those dependencies have been removed or the value is no longer used.
  `PersistentJSONValue` provides separate storage for scalar values, including retained strings; it does not make arrays or objects independent of a container.
- **Native type descriptions** are Foundation's descriptions of C++ values used by JSON conversion and RPC.
  They are distinct from Slang types and shader reflection.

## Dependencies

- [Foundation](../foundation/foundation.md) supplies memory, collections, text, native type descriptions, object interfaces, blobs, hashing, digests, and failure reporting.
  Its file-system views, archives, host files, processes, dynamic loading, streams, output writers, and packet framing support file lookup, artifact materialization, command execution, and RPC transport.
- The product contracts under `include/` supply shared enum values and the blob, file-system, writer, compiler-result, metadata, and object interfaces implemented or consumed here.
  Their use does not assign the product API or the compiler implementation to this module.
- The host C++ environment supplies facilities including mutexes and numeric limits.
  The current `compiler-core` target links `fast_float` for floating-point literal conversion and exposes SPIR-V header definitions to clients of the grammar services.
- Repository generator tools and SPIR-V grammar data supply the embedded grammar implementation described below.
  Its current generated object is compiled with the compiler, so the declaration and generated implementation have not yet acquired one accepted owner.

The current artifact also locates and invokes particular external compilers and adapters.
Those concrete dependencies are covered by the accepted sibling assignment below, rather than being attributed to the shared contracts alone.

## Decomposition

All immediate children are public ordinary modules.
Compiler, adapters, and existing tools use these services directly; public visibility has the implementation-only meaning defined by the root.
Concrete storage classes and platform branches remain implementation details of the service that owns them.

| Child                                                                      | Service                                                                                                            |
| -------------------------------------------------------------------------- | ------------------------------------------------------------------------------------------------------------------ |
| [Source Text](source-text/source-text.md)                                  | Program text, path information, location interpretation, source maps, and include-file lookup.                     |
| [Names](names/names.md)                                                    | Interned compiler name spellings and their pools.                                                                  |
| [Tokens](tokens/tokens.md)                                                 | Lexical categories and token values.                                                                               |
| [Diagnostics](diagnostics/diagnostics.md)                                  | Message descriptions, configurable reporting, annotated ranges, rendering, and diagnostic lookup.                  |
| [Lexing](lexing/lexing.md)                                                 | Tokenization, token-sequence reading, and literal decoding.                                                        |
| [Located Arguments](located-arguments/located-arguments.md)                | Command arguments with diagnostic locations and sequential argument reading.                                       |
| [Artifacts](artifacts/artifacts.md)                                        | Compilation artifacts, their representations and associations, metadata storage, containers, and output embedding. |
| [Compilation Vocabulary](compilation-vocabulary/compilation-vocabulary.md) | Shared names and conversions for compilation formats, languages, tools, and settings.                              |
| [Downstream Compilation](downstream-compilation/downstream-compilation.md) | Shared compilation-tool requests and interfaces, compiler collections, and reusable invocation support.            |
| [JSON](json/json.md)                                                       | JSON text, values, writing, and conversion to and from described C++ values.                                       |
| [JSON RPC](json-rpc/json-rpc.md)                                           | RPC messages, native-value conversion, and communication over supplied packet streams.                             |
| [Source Map JSON](source-map-json/source-map-json.md)                      | JSON interchange for source maps.                                                                                  |
| [Documentation Comments](documentation-comments/documentation-comments.md) | Finding and extracting documentation text near caller-supplied declaration locations.                              |
| [Lookup Generation](lookup-generation/lookup-generation.md)                | Constructing static string lookup tables and generating their C++ implementation.                                  |
| [SPIR-V Grammar](spirv-grammar/spirv-grammar.md)                           | Loading and querying SPIR-V instruction and operand descriptions.                                                  |
| [Editor Protocol](editor-protocol/editor-protocol.md)                      | Language Server Protocol message data and descriptions for JSON conversion.                                        |

Source Text and Names use Foundation without requiring the compiler's AST or preprocessor.
Tokens uses both services; Diagnostics uses token and name formatting as well as location interpretation.
Lexing uses those services and Diagnostics.
Diagnostics accepts a caller-supplied token-highlighting callback, so its implementation does not call the lexer directly.
Documentation Comments uses Lexing and caller-supplied search categories; selecting AST declarations and producing documentation pages belong to Compiler.

Artifacts uses Source Text for path-bearing input representations, Compilation Vocabulary for target naming, and Foundation for storage, file conversion, archives, and shared-library representations.
Its default representation handler uses Source Map JSON to convert between source-map objects and JSON blobs.
Its metadata records and accessors expose facts supplied by compiler passes; collecting those facts from Slang IR remains in Compiler.
Downstream Compilation uses Artifacts and Foundation for its common request, lifetime, and command-execution mechanisms.
Located Arguments uses Source Text, Diagnostics, and Foundation's command escaping.
Compilation Vocabulary supplies the shared enum naming and conversion tables used across these services and their clients.

JSON uses Source Text, Diagnostics, and Foundation's native type descriptions.
JSON RPC adds Foundation's packet streams and optional process lifetime management; application method handlers belong to its clients.
Source Map JSON depends on JSON and Source Text, without making Source Text depend on JSON.
Editor Protocol adds message schemas above JSON; compiler-backed editor queries remain in Compiler.
SPIR-V Grammar uses JSON conversion, Source Text, Diagnostics, and external SPIR-V definitions.
Lookup Generation uses Foundation's strings, collections, hashing, files, and writers, plus Diagnostics when generation fails.

## Existing Implementation Mapping

Unless another directory is given, the paths below are relative to `old-source/compiler-core/`.
A basename followed by `{h,cpp}` denotes both files.
These are responsibility assignments, including the partial and deferred assignments that follow; they do not authorize wholesale file moves.

- **Source Text:** `slang-source-loc.{h,cpp}`, `slang-source-map.{h,cpp}`, and `slang-include-system.{h,cpp}`.
  The include service searches supplied directories and file-system views; preprocessing directives and module-import policy remain in Compiler.
- **Names:** `slang-name.{h,cpp}`.
- **Tokens:** `slang-token.{h,cpp}` and `slang-token-defs.h`.
- **Diagnostics:** `slang-diagnostic-sink.{h,cpp}`, `slang-rich-diagnostics-render.{h,cpp}`, `slang-core-diagnostics.{h,cpp}`, `slang-misc-diagnostic-defs.h`, and `slang-lexer-diagnostic-defs.h`.
  The `IRInst` and `Modifier` diagnostic-argument overload declarations in `slang-diagnostic-sink.h` describe compiler-owned extensions whose definitions are already in Compiler; they need separation from the shared declaration surface.
  Catalog entries name messages used by other services and do not move the conditions that emit those messages into Diagnostics.
- **Lexing:** `slang-lexer.{h,cpp}`.
  Diagnostics owns the lexer message descriptions and their assembly into the core lookup; using those descriptions does not make reporting depend on tokenization.
- **Located Arguments:** the `CommandLineArg`, `CommandLineContext`, `CommandLineArgs`, and `CommandLineReader` declarations and implementations in `slang-command-line-args.{h,cpp}`.
  The separate `DownstreamArgs` declaration and its implementation select named tool argument groups and interpret `-X` forwarding syntax; they belong to Compiler's Downstream Tools.
- **Artifacts:** all `slang-artifact*.h` and `slang-artifact*.cpp` files, plus `slang-slice-allocator.{h,cpp}` and `slang-source-embed-util.{h,cpp}`.
  The slice types at the start of `slang-artifact.h` and the slice allocator support the borrowed request and result interfaces as well as artifact storage.
  The associated-data files include downstream diagnostic records and post-emission metadata for bindings, coverage, synthetic resources, bindless use, cooperative types, and exported names.
  The shared metadata's tracked binding categories and synthetic-resource identifiers remain with that result contract; the compiler computes the recorded values.
- **Downstream Compilation:** `slang-downstream-compiler.{h,cpp}` and `slang-downstream-compiler-set.{h,cpp}`.
  These include the versioned request, compiler and optional path-provider interfaces, base implementations, and collections that retain compiler objects and their libraries.
  They do not choose a particular compiler or discover an adapter.
- **JSON:** `slang-json-lexer.{h,cpp}`, `slang-json-parser.{h,cpp}`, `slang-json-value.{h,cpp}`, `slang-json-native.{h,cpp}`, `slang-json-diagnostics.{h,cpp}`, and `slang-json-diagnostic-defs.h`.
  The last catalog also contains the RPC argument-validation message; its shared lookup currently assembles both sets of descriptions.
- **JSON RPC:** `slang-json-rpc.{h,cpp}` and `slang-json-rpc-connection.{h,cpp}`.
  The test-server-protocol include in the connection header has no corresponding use of its declarations in the connection implementation.
- **Source Map JSON:** `slang-json-source-map-util.{h,cpp}`.
- **Documentation Comments:** `slang-doc-extractor.{h,cpp}`.
  `old-source/slang/slang-doc-ast.{h,cpp}` supplies AST-specific selection and association outside this module.
- **Lookup Generation:** `slang-perfect-hash.{h,cpp}` and `slang-perfect-hash-codegen.{h,cpp}`.
  The former pair includes both the table construction and C++ text generation, so it is not assigned wholesale to Foundation's runtime Hashing service.
- **SPIR-V Grammar:** `slang-spirv-core-grammar.{h,cpp}` supplies the shared descriptions and JSON loader.
  The `getEmbeddedVersion` definition is generated by `tools/slang-spirv-embed-generator/` and compiled into `slang-lookup-tables` by `old-source/slang/CMakeLists.txt`; that generated implementation remains an unresolved split with Compiler.
- **Editor Protocol:** `slang-language-server-protocol.{h,cpp}`, except the trailing `LanguageServerResult<T>` helper in the header.
  That helper represents compiler-operation success and nullable results rather than a wire message, and belongs with compiler-backed editor services.
- **Shared Adapter Contracts:** the non-LLVM implementation portions of `old-source/slang-llvm/slang-llvm-builder.h`, `slang-target-builtin-type-layout-info.h`, and the request/callback declarations in `old-source/slang-glslang/slang-glslang.h` belong to Downstream Compilation.
  The builder's `SLANG_LLVM_IMPL` aliases and LLVM header inclusions belong to the LLVM adapter; its Slang IR emitter remains in Compiler.

`CMakeLists.txt` owns the current static-library assembly, precompiled-header setup, and the DXC-specific compilation settings.
Its current source selection includes the concrete integrations below; the accepted concrete-integration boundary has not been implemented by changing that build.

## Ancestor and Sibling Questions

### Concrete Tool Integration

`slang-downstream-compiler-util.{h,cpp}` combines compiler-version matching, language-to-tool defaults, the table of concrete locators, and dependent-library search.
The concrete integrations are `slang-dxc-compiler.{h,cpp}`, `slang-fxc-compiler.{h,cpp}`, `slang-gcc-compiler-util.{h,cpp}`, `slang-glslang-compiler.{h,cpp}`, `slang-llvm-compiler.{h,cpp}`, `slang-metal-compiler.{h,cpp}`, `slang-nvrtc-compiler.{h,cpp}`, `slang-tint-compiler.{h,cpp}`, `slang-visual-studio-compiler-util.{h,cpp}`, and `windows/slang-win-visual-studio-util.{h,cpp}`.
Those files discover particular tools, translate requests for them, parse their output, and invoke their implementations.
The [root migration contract](../slang.md#staged-migration-contracts) assigns this complete concrete service to Compiler's Downstream Tools, using this module's shared contracts and invocation support.
That assignment includes the actual calls into loaded implementations, not just the loading functions.

For example, `LLVMDownstreamCompilerUtil::locateCompilers` loads `slang-llvm`, resolves `createLLVMDownstreamCompiler_V4`, and stores the library and compiler in a set.
The LLVM adapter links `compiler-core` in `cmake/LLVM.cmake`.
The current `compiler-core` to `slang-llvm` to `compiler-core` cycle remains until the concrete implementations and their build consumers follow that accepted ownership.
Moving only the shared header, or only the discovery operation, would not establish the intended separation.
Migration must also preserve access for the existing callers described by the root contract.
Current test and other clients obtain static internal definitions that a shared Compiler library may hide.
No replacement library, export, or callback mechanism is selected here.
The root's compiler/bootstrap and compiled-language-library-wrapper cycles remain separate questions.

### Reusable Text and Shared Compilation Vocabulary

`slang-name-convention-util.{h,cpp}` converts spelling conventions without interpreting compiler declarations.
`slang-pretty-writer.{h,cpp}` supplies indentation, string escaping, and comma tracking without inspecting program types.
Both are candidates for [Foundation's Text service](../foundation/text/text.md), subject to its owner's refinement; their current compiler and generator clients do not make the utilities compiler policy.

[Foundation](../foundation/foundation.md#existing-implementation-mapping) deferred `old-source/core/slang-type-text-util.{h,cpp}` and `slang-type-convert-util.{h,cpp}`.
The target/language conversions and the compilation-language, tool, target, scalar-type, and compiler-setting names are proposed for Compilation Vocabulary because compiler support and compiler clients share them.
`TypeTextUtil` also contains archive-format names and file-system-mode names.
Those entries require separate ownership with the corresponding Foundation services or compiler configuration policy; assigning the whole utility to this module would hide that distinction.
Neither this document nor the Foundation page settles that split.

### Application Protocols and Generated Grammar

`slang-test-server-protocol.{h,cpp}` describes unit-test and tool-test requests, shutdown, and execution results.
Its consumers are the existing test client and server under `tools/`; the root must assign their shared protocol alongside those implementations, which lie outside its initial tool-entry-point mapping.
JSON RPC does not need those application schemas to implement message transport.

The SPIR-V grammar's shared type and JSON loader serve both the compiler and its embedding generator, while its built-in accessor is supplied by the compiler's generated lookup-table object.
The parent must reconcile those ownership and build-input relationships before that child can migrate as a complete service.
Compiler Support remains pending while these ancestor assignments and the migration of the concrete integrations remain unresolved.
