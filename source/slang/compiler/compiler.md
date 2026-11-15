<!--
SPDX-FileCopyrightText: The Khronos Group, Inc.
SPDX-License-Identifier: CC-BY-4.0
-->

# Compiler

Compiler loads programs, checks their meaning, produces target code, and exposes information about their declarations and shader parameter layouts.
It also provides compiler-backed editing, documentation, bytecode execution, and reproduction services, and prepares the language libraries bundled with the compiler.

The [root](../slang.md#decomposition) assigns this concern to a public build artifact, currently the `slang` target with the installed base name `slang-compiler`.
The decomposition below proposes ordinary implementation children within that assignment.
The implementation remains in `old-source/`; the [unresolved boundaries](#unresolved-boundaries) prevent treating the proposal as a completed acyclic module graph.

## Concepts

The [root concepts](../slang.md#concepts) distinguish implementation modules and build artifacts from modules in the Slang language.
[Compiler Support](../compiler-support/compiler-support.md#concepts) defines program text, locations, names, tokens, diagnostics, compilation artifacts, and downstream compilers.
[Foundation](../foundation/foundation.md#concepts) defines storage ownership, reference counting, blobs, and file systems.
Compiler adds the following concepts:

- A **global session**, implemented by `Session`, provides shared built-in declarations and downstream-tool configuration for multiple compilation sessions.
  A **compilation session**, implemented by `Linkage`, supplies the configuration and loaded-module context for related compilation work.
  The corresponding product interfaces are `slang::IGlobalSession` and `slang::ISession`; the internal names do not match that pair of interface names.
- A **translation unit** is the group of program text compiled together by the front end.
  A translation unit may include more than one file.
  A **language module**, represented by `Module`, groups the compiler's representations of one translation unit.
  The compiler uses the module's checked declarations for imports and reflection.
  The module can also hold generated or loaded Slang IR; editor requests may retain checked modules without IR.
- The **AST** represents declarations, statements, and expressions and records information established by semantic checking.
  Related **compile-time values**, represented by `Val`, describe values such as types, integer expressions, and conformance evidence; they are distinct from syntax nodes with a unique location.
  A **declaration reference**, represented by `DeclRef`, identifies a declaration together with its specialization or lookup context.
  An `ASTBuilder` allocates nodes and manages value construction.
  A non-root builder retains its parent so that nodes allocated by ancestors outlive nodes allocated by descendants.
- **Slang IR** is the compiler's intermediate program representation, used for linking, specialization, analysis, transformation, and target emission.
  Instructions represent types and values as well as executable operations, and operand uses record references between instructions.
  An `IRModule` manages instruction storage and associated analysis state.
- A **component type** represents a unit of linkable program code, such as a compiled module, an entry point, a composition, or a specialization.
  Component requirements identify other components needed to complete that program.
  A **target request** selects an output format and its options; a **target program** associates a component type with a target request and caches target-dependent results on demand.
- A **capability requirement** describes combinations of target features under which a declaration or operation is available.
  `CapabilitySet` represents alternative supported combinations rather than one undifferentiated feature bit set.
  A **profile** combines a shader stage and profile version, with additional capability information recorded by `Profile`.
- A **program layout** describes shader parameters and their target-dependent storage, resource usage, and bindings.
  **Reflection** exposes program declarations, types, and layouts through the product API; it is distinct from Foundation's descriptions of native C++ data.
- A **serialized module** stores the compiler's module representations and supporting information for later loading.
  The **fossil format** is the relocatable object-graph representation used by the serialization machinery.
  Neither a serialized module nor an embedding object library is a separate implementation build artifact in this proposal.

## Dependencies

- [Foundation](../foundation/foundation.md) supplies storage, ownership, collections, text, hashing and digests, files and file-system views, archives, compression, host processes and libraries, writers, and profiling.
  The current compiler links the static `core` target.
- [Compiler Support](../compiler-support/compiler-support.md) supplies source files and locations, interned names, tokens and lexing, diagnostics, include lookup, compilation artifacts, downstream interfaces, JSON, documentation-comment extraction, and editor-protocol messages.
  The current compiler links `compiler-core`.
  Compiler policy determines imports, semantic diagnostics, particular downstream-tool selection, and the contents of results recorded through those shared interfaces.
- [Adapters](../adapters/adapters.md) supplies the runtime-loaded LLVM and GLSL/SPIR-V integrations and compiled-language-library providers.
  The shared requests and LLVM builder interface belong to Compiler Support; the compiler-side LLVM emitter interprets Slang IR and calls that builder.
  Loading the implementation remains a dependency on the adapter, even when the request declarations are shared.
- External toolchain interfaces supply DXC/FXC, NVRTC, Tint, Metal, GCC/Clang, and Visual Studio compilation where selected.
  The accepted concrete integration assignment below includes their discovery, request translation, invocation, and output parsing, rather than only their loaders.
  SPIR-V headers supply target-format definitions; `libcmark-gfm` supplies Markdown parsing for literate Slang input.
  Editor formatting additionally invokes a discovered `clang-format` executable.
- Product headers under `include/` supply the compiler interfaces, options, scalar types, and export conventions that this artifact implements.
  The host C++ environment supplies synchronization, timing, allocation, and platform facilities; optional mimalloc integration changes this artifact's allocation implementation.
- Repository generators, grammar data, CMake configuration, and `prelude/` supply generated declarations, target lookup data, version/configuration headers, and language/runtime preludes.
  The bootstrap executable supplies compiled built-in libraries.
  Standard-module rules use that bootstrap executable with the generated core archive, `slangc` where selected, or a supplied host bootstrap during cross-compilation.
  These are construction dependencies, as detailed below; the existing generator and executable implementations are not reassigned into this artifact.
- Generated host output can require [Runtime](../runtime/runtime.md).
  Compiler emits its calls and arranges its downstream library input; Runtime itself performs the resulting failure reporting and imported-library access.
  This generated-program requirement does not mean the compiler implementation links `slang-rt`.

## Candidate Decomposition

The children are ordinary implementation modules, not independently linked libraries.
Each public child is available to other implementation modules under the root's visibility rules; the product API remains the interface in `include/`.
All immediate children are public; existing tool and inspection clients also use compiler implementation services.

| Child                                                                   | Visibility | Service                                                                                                                                     |
| ----------------------------------------------------------------------- | ---------- | ------------------------------------------------------------------------------------------------------------------------------------------- |
| [Compilation](compilation/compilation.md)                               | Public     | Sessions, compilation requests and options, loaded and linkable program components, and coordination of compilation services.               |
| [Language](language/language.md)                                        | Public     | Program syntax and semantic values, preprocessing, parsing, name lookup, checking, and translation from checked declarations into Slang IR. |
| [Target Requirements](target-requirements/target-requirements.md)       | Public     | Target capability combinations, profiles, and stages.                                                                                       |
| [Intermediate Programs](intermediate-programs/intermediate-programs.md) | Public     | Slang IR construction, inspection, linking, specialization, validation, analysis, and transformation.                                       |
| [Layout and Reflection](layout-and-reflection/layout-and-reflection.md) | Public     | Target-dependent data layout and shader parameter binding, together with declaration, type, and layout reflection.                          |
| [Target Output](target-output/target-output.md)                         | Public     | Target program generation, target text and binary emission, downstream compilation coordination, and delivery of generated artifacts.       |
| [Downstream Tools](downstream-tools/downstream-tools.md)                | Public     | Concrete tool discovery and defaults, tool-specific requests and invocation, and interpretation of tool results.                            |
| [Module Storage](module-storage/module-storage.md)                      | Public     | Reading and writing compiler modules, their AST/IR and location data, and the object-graph formats used for those representations.          |
| [Language Libraries](language-libraries/language-libraries.md)          | Public     | Built-in and standard language-library definitions, compilation, embedding inputs, loading, and built-in cache handling.                    |
| [Compiler Diagnostics](compiler-diagnostics/compiler-diagnostics.md)    | Public     | Compiler message descriptions and formatting of compiler-specific values for shared diagnostic reporting.                                   |
| [Documentation](documentation/documentation.md)                         | Public     | Associating documentation comments with declarations and producing declaration documentation.                                               |
| [Editor Services](editor-services/editor-services.md)                   | Public     | Workspace/document versions, compiler-backed editor queries, formatting, and Language Server Protocol service handling.                     |
| [Bytecode Execution](bytecode-execution/bytecode-execution.md)          | Public     | The compiler's bytecode format, decoding and inspection, and execution through the bytecode interpreter.                                    |
| [Reproduction](reproduction/reproduction.md)                            | Public     | Saving, validating, loading, and extracting compilation reproducers with their configuration and files.                                     |
| [Recording and Replay](recording-and-replay/recording-and-replay.md)    | Public     | Recording compiler-interface operations, replaying them against compiler objects, and inspecting recorded streams.                          |

Compilation selects services for the request; preprocessing-only and editor requests need not generate IR, and pass-through output can bypass Language's AST-to-IR translation.
For requests that use Slang IR, Language can generate it from checked declarations and Intermediate Programs processes it.
Layout and Reflection computes layouts used by reflection and code generation; Target Output produces compilation artifacts and requests concrete tools through Downstream Tools.
Module Storage makes compiled representations available for later sessions, while Language Libraries uses compilation and storage to prepare and load the bundled declarations.
Documentation and Editor Services use checked declarations.
Target Output writes bytecode defined by Bytecode Execution, whose decoder and interpreter read that format.
These service relationships explain the proposed concerns, but the current reverse calls described below still require refinement before they form an acyclic implementation decomposition.

## Existing Implementation Mapping

Unqualified basenames below refer to `old-source/slang/`.
`{h,cpp}` denotes both files, and a `slang-ir-*` or similar family includes its generated-source templates unless an exception is stated.
The assignments describe service responsibilities, including mixed files; they are not instructions to move filename families without declaration-level separation.

- **Compilation:** session and component implementations in `slang-global-session.{h,cpp}`, `slang-session.{h,cpp}`, `slang-module.{h,cpp}`, `slang-linkable.{h,cpp}`, `slang-linkable-impls.{h,cpp}`, `slang-entry-point.{h,cpp}`, `slang-target.{h,cpp}`, and `slang-target-program.{h,cpp}`.
  Request configuration and coordination are in `slang-compile-request.{h,cpp}`, `slang-end-to-end-request.{h,cpp}`, `slang-translation-unit.{h,cpp}`, `slang-compiler-tu.cpp`, `slang-compiler-options.{h,cpp}`, and `slang-options.{h,cpp}`.
  Compiler API dispatch and internal/product conversions are in `slang-api.cpp`, `slang-compiler-api.{h,cpp}`, and `slang-internal.h`.
  The mixed service implementations in these files have the explicit exceptions below.
- **Language:** the `slang-ast-*` family, except `slang-ast-natural-layout.{h,cpp}`; `slang-syntax.{h,cpp}`, `slang-visitor.h`, `slang-parser.{h,cpp}`, `slang-preprocessor.{h,cpp}`, `slang-lookup.{h,cpp}`, and the `slang-check*` family, except `slang-check-out-of-bound-access.{h,cpp}` and downstream-session management at the start of `slang-check.cpp`.
  `slang-ast-print` and `slang-ast-dump` inspect or print compiler representations; they do not emit a target program.
  `slang-lower-to-ir.{h,cpp}` owns the AST-to-IR translation, and `slang-mangle.{h,cpp}` supplies declaration-based linkage names.
  `slang-markdown.{h,cpp}` recognizes literate input and extracts Slang code blocks; it is an input-language service, separate from documentation output.
  `slang-base-type-info.{h,cpp}` and `slang-type-system-shared.{h,cpp}` define the shared program vocabulary used by AST, IR, and bundled library definitions: base types, address spaces and access qualifiers, memory scopes, annotation kinds, texture parameters, and work-graph barrier flags.
- **Target Requirements:** `slang-capability.{h,cpp}`, `slang-capabilities.capdef`, `slang-profile.{h,cpp}`, and `slang-profile-defs.h`.
  `slang-capability-val.h` and the operations involving those AST values in `slang-capability.cpp` cross into Language, as described below.
- **Intermediate Programs:** `slang-ir.{h,cpp}`, `slang-ir.h.lua`, and the `slang-ir-*` family, except the layout calculations and SPIR-V snippet service identified below.
  This includes the instruction schemas, stable serialization names, builders, analysis and utility operations, linking and specialization, automatic differentiation, instrumentation and metadata collection, optimizations, and target-specific validation and legalization.
  `slang-check-out-of-bound-access.{h,cpp}` also belongs here: its checker examines IR array accesses despite the filename's similarity to AST checking.
  `slang-legalize-types.{h,cpp}` supplies the compound type/value representations used during IR legalization despite the header's older front-end-oriented introduction.
  `slang-container-pool.h` manages temporary reuse of pointer containers within IR processing.
- **Layout and Reflection:** `slang-type-layout.{h,cpp}`, `slang-parameter-binding.{h,cpp}`, `slang-hlsl-to-vulkan-layout-options.{h,cpp}`, `slang-ast-natural-layout.{h,cpp}`, and `slang-ir-layout.{h,cpp}`.
  These include layout rules, size/alignment calculations, resource accounting, offsets, and parameter binding.
  `slang-reflection-api.cpp` also exposes checked declarations and types, not only layouts; `slang-reflection-json.{h,cpp}` produces reflection output.
  Target-format classification and intermediate-language selection currently mixed into the layout implementation require the separation below.
- **Target Output:** the `slang-emit*` family, `slang-code-gen.{h,cpp}`, `slang-intrinsic-expand.{h,cpp}`, `slang-extension-tracker.{h,cpp}`, `slang-pass-through.{h,cpp}`, `slang-pass-wrapper.{h,cpp}`, `slang-artifact-output-util.{h,cpp}`, and `slang-lookup-spirv.h`.
  Emission includes text targets, direct SPIR-V, the compiler-side LLVM builder calls, VM bytecode, dependency files, and target prelude generation.
  The pass wrapper supplies validation/dumping/profiling hooks around IR processing; individual transformations remain in Intermediate Programs.
  `slang-ir-spirv-snippet.{h,cpp}` parses target intrinsic snippets used during SPIR-V preparation and emission.
- **Downstream Tools:** the concrete integrations in `old-source/compiler-core/`, assigned to Compiler by the [root migration contract](../slang.md#staged-migration-contracts):
  `slang-downstream-compiler-util.{h,cpp}`, `slang-dxc-compiler.{h,cpp}`, `slang-fxc-compiler.{h,cpp}`, `slang-gcc-compiler-util.{h,cpp}`, `slang-glslang-compiler.{h,cpp}`, `slang-llvm-compiler.{h,cpp}`, `slang-metal-compiler.{h,cpp}`, `slang-nvrtc-compiler.{h,cpp}`, `slang-tint-compiler.{h,cpp}`, `slang-visual-studio-compiler-util.{h,cpp}`, and `windows/slang-win-visual-studio-util.{h,cpp}`.
  Their shared compiler interfaces, retaining compiler sets, reusable command-line invocation base, and request/result contracts stay with Compiler Support.
  The `DownstreamArgs` portion of `slang-command-line-args.{h,cpp}` also belongs here; located arguments and sequential reading remain in Compiler Support.
  The complete service assignment is accepted as semantic ownership; the source and current `compiler-core` build have not changed.
- **Module Storage:** the `slang-serialize*` family, `slang-fossil.{h,cpp}`, and `slang-module-library.{h,cpp}`.
  They cover serialization operations and fossil storage, module containers and RIFF chunks, AST/IR encoding, source-location data, and loading a serialized library into compiler objects.
- **Language Libraries:** `core.meta.slang`, `hlsl.meta.slang`, `glsl.meta.slang`, `diff.meta.slang`, `slang-core-module.cpp`, and `slang-core-module-textures.{h,cpp}`.
  The texture files generate library declarations; they are not target-code emitters for a user's program.
  The two embedding implementations and generation rules in `old-source/slang-core-module/`, the Slang definitions, configuration template, documentation, and resource-production rules throughout `old-source/standard-modules/`, and `old-source/core/slang-builtin-module-cache.{h,cpp}` belong to this concern.
  The neural library's adjacent Slang unit-test inputs remain library validation material, rather than a second test-framework implementation.
- **Compiler Diagnostics:** `slang-diagnostics.{h,cpp}`, `slang-rich-diagnostics.{h,cpp}`, `slang-rich-diagnostics.h.lua`, `slang-diagnostics.lua`, `slang-diagnostics-helpers.lua`, and `diagnostics/type-errors.lua`.
  These define and assemble compiler messages and format AST/IR values; Compiler Support continues to own the diagnostic sink and its reusable rendering machinery.
- **Documentation:** `slang-doc-ast.{h,cpp}` and `slang-doc-markdown-writer.{h,cpp}`.
  The first service chooses and associates AST declarations with extracted comments; the second produces declaration pages and their navigation.
- **Editor Services:** the `slang-language-server*` family, except its built-in-library text entry point described below; `slang-workspace-version.{h,cpp}`; and the compiler-operation `LanguageServerResult<T>` helper currently at the end of `old-source/compiler-core/slang-language-server-protocol.h`.
  That protocol header's message schemas remain with Compiler Support.
  The language parser, preprocessor, and checker produce the information described by `slang-content-assist-info.h`; the compiler/editor exchange contract is a mixed responsibility discussed below.
- **Bytecode Execution:** `slang-vm-bytecode.{h,cpp}`, `slang-vm.{h,cpp}`, and `slang-vm-inst-impl.{h,cpp}`.
  The bytecode decoder and execution validation are part of this service; `slang-emit-vm.{h,cpp}` remains the Target Output producer.
- **Reproduction:** `slang-repro.{h,cpp}` and `slang-repro-validator.{h,cpp}`.
  Their saved request state and captured file-system contents differ from serialized compiled modules and from recorded API operation streams.
- **Recording and Replay:** the stream, decoder, replay context and handlers, recording conversions, proxy implementations, and proxy-generation macros throughout `old-source/slang-record-replay/`.
  These files compile conditionally into the compiler target rather than forming a separately loadable recording library.
  The existing `tools/slang-replay/` client uses the exported context and decoder services; its executable implementation remains outside this mapping.

`slang-compiler.h`, `slang-compiler-fwd.h`, and the nearly empty `slang-compiler-api.cpp` are integration/aggregation surfaces, not additional concerns.
`slang.cpp` supplies build-tag and effective-profile operations, and `slang-compiler.cpp` validates language versions; these belong with Compilation's configuration service.
`slang.natvis`, `slang.natjmc`, and `slang_lldb.py` provide debugger integration across the compiler representations.
`slang-mimalloc.cpp` provides artifact-specific global allocation and, for the selected Windows shared build, unload handling; it remains artifact integration rather than another language or Foundation service.
The local `CMakeLists.txt` assembles variants and generated inputs for these services.

### Mixed-File Boundaries

Session and component declarations contain operations for several services.
The object model and request coordination belong to Compilation, but that does not assign every implementation of a method on those objects to the same child:

- `Session::compileBuiltinModule`, built-in load/save/read operations and built-in text access in `slang-global-session.cpp`, the built-in cache/provider loading functions in `slang-api.cpp`, and standard-module provider/path/decompression helpers in `slang-session.cpp` implement Language Libraries.
  The `getBuiltinModuleSource` entry point at the end of `slang-language-server.{h,cpp}` is also library-text access rather than an editor query implementation.
  The surrounding session creation, ownership and request dispatch remain Compilation.
- `Session::_setSharedLibraryLoader`, `resetDownstreamCompiler`, and `getOrLoadDownstreamCompiler`, together with `SinkSharedLibraryLoader` at the start of `slang-check.cpp`, implement session/tool coordination rather than semantic checking.
  Global-session locator, tool-path, transition and prelude settings remain part of that same coordination.
  The concrete locators and compilers they call are assigned to Downstream Tools.
- Request preprocessing and parsing helpers such as `extractSourceSegments`, `preprocessSourceSegments`, and `parsePreprocessedSegments` in `slang-compile-request.{h,cpp}` implement Language's input processing; the request's sequencing of front-end work remains Compilation.
  `slang-content-assist-info.h` holds both editor-supplied checking inputs and parser/preprocessor/checker results, so its producer contract must remain available to Language without requiring Language to use the editor's workspace or query implementation.
- `slang-type-layout.cpp` defines target-classification functions declared in `slang-target.h`, LLVM-backed built-in-layout queries, and `getIntermediateSourceLanguageForTarget` declared in `slang-target-program.h`.
  Compilation owns target configuration/classification, Layout and Reflection owns layout calculation and its tool query, and Target Output owns intermediate-language selection.
  The layout-bearing IR instruction declarations and builders in `slang-ir-insts.h` remain IR representation; computing a layout does not transfer ownership of the instruction representation.
- `slang-capability-val.h` represents capability information as AST `Val` nodes, and the corresponding freeze/conversion implementations in `slang-capability.cpp` use `ASTBuilder`.
  Language owns the `Capability*Val` methods, operations accepting those nodes, and the representation bridge; Target Requirements owns the capability combinations and operations independent of AST storage.
  Likewise, compiler-specific diagnostic-argument overloads declared in Compiler Support's sink header need a compiler-owned declaration surface alongside their AST/IR formatting definitions.
- `slang-emit.cpp` mixes target-output coordination with IR transformations, validation, and layout queries.
  `linkAndOptimizeIR` coordinates the requested pipeline for Target Output, but transformations such as `lowerDiffTypeInfoInsts`, `lowerSumVectorMatrixInsts`, and `removeWeakUseInsts` belong to Intermediate Programs, as do its static IR rewrites and `checkStaticAssert` validation.
  Its `findEntryPointLayout`, `getScopeStructLayout`, and `getGlobalStructLayout` queries belong with Layout and Reflection.
  Assigning all of this file to emission would hide those distinct responsibilities.
- `SPIRVEmitSharedContext` in `slang-ir-spirv-legalize.h` is used by both SPIR-V legalization and emission.
  Its parsed-snippet cache and emitter-specific state cannot be assigned to an independent generic IR layer merely because of the filename.
  The context and `getParsedSpvSnippet` service belong to Target Output, while the IR rewrite remains Intermediate Programs; the current mutual use remains unresolved below.
- Compiler API forwarding functions in `slang-api.cpp` remain Compilation integration, while coverage-manifest JSON formatting is result/reflection output and record/replay controls dispatch to Recording and Replay.
  Reproducer output, reflection output, and built-in documentation requested by the end-to-end/session APIs remain implementations of their named services rather than separate command-line executables.

## Construction and Packaging

The current build uses several kinds of inputs and outputs that cannot be inferred from directory names:

| Existing Build Participant                                                                                   | Role                                                                                                                                                                                                                                                                                          |
| ------------------------------------------------------------------------------------------------------------ | --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| `slang`, `slang-without-embedded-core-module`, and `slang-common-objects`                                    | Final compiler library, bootstrap library variant, and shared C++ object compilation used to avoid compiling common implementations twice. When embedding is disabled, the bootstrap library name aliases `slang`.                                                                            |
| `slang-embedded-core-module`, `slang-no-embedded-core-module`, and the two corresponding module-text targets | Object-library variants providing compiled built-in bytes or built-in text, or the non-embedded alternatives. They are linked into compiler variants, not separately installed product libraries.                                                                                             |
| `slang-bootstrap` and `generate_core_module`                                                                 | The locally built bootstrap links the non-embedded compiler variant; cross builds can supply a host bootstrap executable instead. The generation step invokes bootstrap to produce the core archive and core/GLSL embedding headers. The executable implementation remains with Tools.        |
| Standard-module compilation rules                                                                            | Bootstrap or `slangc` invocations, with the generated core archive, that produce the serialized module tree and manifests under the configured build location. Cross builds can use a supplied host bootstrap; installation chooses loose resources or the separately owned provider library. |
| `slang-capability-defs`, `slang-capability-lookup`, `slang-lookup-tables`, and `slang-fiddle-output`         | Generated object libraries and an interface target carrying generated-source ordering/include requirements. Their declarations and tables follow the service they describe.                                                                                                                   |
| `generate_core_module_cache`                                                                                 | Runs `slang-core-module-cache` to prefix the already-generated core archive with the final compiler file's modification timestamp, producing the non-embedded shared build's runtime cache.                                                                                                   |
| `slang-proxy`                                                                                                | A conditional Windows compatibility shared library installed as `slang.dll`, forwarding to `slang-compiler.dll`, owned by the root's [Compiler DLL Proxy](../compiler-dll-proxy/compiler-dll-proxy.md) artifact. The Unix compatibility symlink remains Compiler packaging.                   |

The GLSL and optional standard-library provider wrappers remain [Adapters](../adapters/adapters.md) artifacts.
Compiler owns the language definitions and prepared resources they publish, not those wrappers.
The existing output filenames, product interfaces, module formats, export settings and construction order constrain migration.
No new library target or build procedure is selected by this document.

## Unresolved Boundaries

The [root's bootstrap and provider cycles](../slang.md#unresolved-architectural-dependencies) remain unresolved.
The observed build orders distinct compiler variants correctly; collapsing those variants into one architectural artifact still gives Compiler a construction dependency on a tool that links Compiler.
Loading a provider whose contents Compiler prepared likewise preserves a dependency in both directions at the proposed owner level.
The root assigns the independently installed Windows proxy to its public [Compiler DLL Proxy](../compiler-dll-proxy/compiler-dll-proxy.md) artifact, whose parent is the root package.
That settled ownership does not resolve the compiler/bootstrap or provider dependencies above.

The root has accepted the complete Downstream Tools assignment described by [Compiler Support](../compiler-support/compiler-support.md#concrete-tool-integration).
For example, `LLVMDownstreamCompilerUtil::locateCompilers` creates an LLVM-backed compiler through the loaded adapter, and the concrete wrappers perform the subsequent tool calls; moving only the locator would leave part of the implementation dependency in Support.
The shared `DownstreamCompilerBase`, `CommandLineDownstreamCompiler`, request/result contracts, and retaining `DownstreamCompilerSet` remain in Support.
Current source selection still compiles the concrete integrations into `compiler-core`, so the reassignment has not yet removed its adapter dependency in the implementation or build.
Migration must preserve existing test and other callers of those static internal definitions, including when a shared Compiler library hides them, under the [root's migration conditions](../slang.md#staged-migration-contracts).

The SPIR-V grammar also remains split: Compiler Support owns the shared grammar representation and JSON loader, but this artifact's generated `slang-lookup-tables` object defines `SPIRVCoreGrammarInfo::getEmbeddedVersion`.
Compiler and the existing embedding generator both use that grammar contract.
The ancestor must settle the complete service's generated implementation and construction inputs before either sibling claims the split is resolved.

Within Compiler, the candidate children expose several current reverse dependencies that require further refinement:

- Compilation constructs and retains AST builders, modules, and IR, while `SharedASTBuilder` and `IRModule` carry a `Session*` and use global-session services.
  `Linkage` also calls checking and deserialization when loading modules, while semantic import checking calls back into its module-loading services.
  IR dumping can request downstream SPIR-V disassembly through the session, so the dependency is not limited to keeping a pointer in the IR object.
  A one-way diagram from sessions to the front end would omit those contracts.
- The AST-to-IR translator needs both representations, reflection performs semantic queries, and layout processing creates or queries IR layout information.
  IR and bundled libraries also use the program vocabulary currently defined alongside Language's AST implementation.
  Conversely, `SizeOfIntVal` and `AlignOfIntVal` folding in `slang-ast-val.cpp` calls `ASTNaturalLayoutContext` to compute sizes and alignments.
  Compiler Diagnostics formats AST and IR values that those same services report.
  Merely separating declaration headers from definitions does not settle these service relationships.
- AST member access can deserialize declarations on demand: `ContainerDeclDirectMemberDecls` in `slang-ast-decl.cpp` calls its serialized-member operations implemented in `slang-serialize-ast.cpp`.
  Module Storage constructs Language representations, but Language also invokes those storage-backed operations during ordinary member access.
- `SPIRVEmitSharedContext` carries state shared by `legalizeIRForSPIRV` and emission, including requirements discovered during legalization.
  Intermediate Programs and Target Output cannot claim independent boundaries until their shared preparation/emission operation has a coherent owner.
- Language parsing and checking produce editor information through the mixed content-assist contract, and recording code converts reflection objects through compiler internals.
  Treating either facility as an independent low-level protocol library would conceal those dependencies.

Foundation's deferred compiler cache is assigned to Language Libraries, but its shared build utility client in `tools/slang-core-module-cache/` must remain accounted for during extraction.
The compiler-session prelude setup and deferred-core-module option query in `old-source/core/slang-test-tool-util.{h,cpp}` are integration helpers for existing tools; their final ownership remains an ancestor question alongside the status, path, and test-selection portions of that utility.
The mixed archive/file-system naming policy deferred by Foundation and Compiler Support is also still pending; Compiler does not claim the whole utility.

Compiler remains on the refinement worklist.
The child stubs identify the next concerns to investigate, without claiming an accepted acyclic artifact arrangement or a migration-ready internal dependency graph.
