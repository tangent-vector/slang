<!--
SPDX-FileCopyrightText: The Khronos Group, Inc.
SPDX-License-Identifier: CC-BY-4.0
-->

# Slang Implementation

The `slang` package provides compilation and program inspection for Slang and supported input languages, together with the tools and runtime services needed to use the resulting programs.
We separate reusable implementation services, compiler services, optional integrations, and product entry points so that each concern has an identifiable owner.

This document describes a proposed organization of the implementation.
The mapping below identifies the existing code and build targets from which that organization is being extracted; the proposed directories do not imply that the code has already moved or that new library boundaries have been established.
The product API headers in the repository's `include/` directory remain an existing contract outside this implementation hierarchy.

[Random Numbers](foundation/random-numbers/random-numbers.md) is the first service whose implementation has been transferred into this hierarchy.
The owning module documents identify the current implementation locations and any remaining legacy dependencies.

## Concepts

- A **module** is a named implementation concern, represented by a directory containing its documentation and implementation files.
  Its canonical unqualified name is written in kebab case, such as `source-reader`.
  The primary document uses that name as its basename: `source-reader.md`.
  A public child uses `source-reader/source-reader.md`; a private child uses `_source-reader/source-reader.md` because the `_` directory prefix marks visibility and is not part of the module name.
  An implementation module is distinct from a module written in the Slang language.
- A **package** is a module that groups child packages and build artifacts and contains no implementation code of its own.
  The root `slang` module is a package.
- A **build artifact** is a module built as a static library, dynamic library, or executable.
  Its parent is a package, and its children are ordinary implementation modules rather than further build artifacts.
  An artifact's existing installed filename may differ from its architectural name.
- A **public child** exposes services to modules outside its parent's subtree.
  Public visibility within this implementation hierarchy does not make those services part of the supported product API in `include/`.
- A **private child** exposes services only within its parent's subtree.
  Its directory name begins with `_`.
- A **dependency** means that a module requires another module's services or contract.
  For build artifacts, the dependency may be satisfied by linking or by loading another artifact at runtime.
  A build dependency additionally identifies a tool or generated input required to construct an artifact.

Dependencies between the proposed modules must be acyclic.
The requirement includes dependencies on linked or runtime-loaded implementations and dependencies on tools or inputs used to build an artifact.
It is not satisfied merely by making dependencies between header contracts acyclic.

An implementation module may declare a dependency only on a module that is a child or deeper descendant of one of the following:

- An ancestor of the depending module.
- A module explicitly declared as a dependency by an ancestor of the depending module.

These alternatives identify possible dependency targets, rather than implicitly declaring a dependency on every permitted target.
The same inheritance principle applies to external packages: a descendant may use a package or service already declared by an ancestor, or a service within a package declared by an ancestor.
The visibility restriction applies in addition to inheritance: when a dependency target is a private child, its direct parent must be an ancestor of the depending module.

For example, a child of compiler may declare a dependency on a public foundation module because foundation is a descendant of their common ancestor, `slang`.
That permission does not allow the compiler child to use `foundation/_allocator`, because foundation is not an ancestor of the compiler child.
The example names a hypothetical private child only to illustrate access; it does not assign foundation's decomposition.

A parent may assemble and re-export its children's services; a child uses the relevant sibling service rather than depending on the assembled parent interface.
A refined child document records the dependencies that the child's own services require.
The root's dependency categories and responsibility assignments do not constitute a complete dependency inventory for every child.

## Dependencies

The root package has no implementation or link step of its own.
The following contracts and build inputs are available to its descendants, where the dependency is needed:

- The product API in `include/` supplies shared types, interfaces, result codes, and platform/export declarations used by the implementation and its clients.
- The host C++ environment and operating-system interfaces supply allocation, threading, files, processes, and dynamic loading.
  Platform-specific access belongs behind the relevant implementation service.
- Utility libraries supply hashing containers, compression, and numeric parsing.
  The current `core` and `compiler-core` targets select dependencies including unordered_dense, miniz, LZ4, and fast_float; optional allocation support uses mimalloc.
- Compiler toolchains and format definitions supply optional downstream compilation and target-format support.
  Current examples include LLVM/Clang, glslang, SPIR-V headers and tools, DXC, and NVRTC.
  Their use can require build-time headers, link dependencies, or a tool/library located at runtime.
  A refined owning child document identifies which of those cases applies.
- Repository build infrastructure in `cmake/` and generator executables currently implemented under `tools/` produce metadata, embedded library contents, and generated implementation files.
  Those existing tool implementations remain outside the implementation hierarchy covered here, even when their outputs are prerequisites for an artifact.
- Language-library material in `prelude/` supplies declarations and runtime headers used when compiling programs and building the compiler's bundled libraries.
  Its current location is outside the implementation hierarchy covered here.

These are dependency categories, not a requirement that every artifact link every listed library.
The current build definitions provide evidence about the dependencies in use.
Refining an owning child includes identifying its actual dependencies and their purposes; the categories above do not establish that the child's dependency analysis is complete.

## Decomposition

All direct children are public within the implementation hierarchy.

| Child                                                          | Kind           | Responsibility                                                                                                                                                     |
| -------------------------------------------------------------- | -------------- | ------------------------------------------------------------------------------------------------------------------------------------------------------------------ |
| [Foundation](foundation/foundation.md)                         | Build artifact | Reusable data structures, memory and text services, and access to the host environment.                                                                            |
| [Compiler Support](compiler-support/compiler-support.md)       | Build artifact | Shared services for reading program text, reporting diagnostics, representing compilation outputs, and supporting common compilation-tool requests and invocation. |
| [Compiler](compiler/compiler.md)                               | Build artifact | Program loading, checking, transformation, code generation, reflection, compiler-backed editing, and concrete downstream-tool integration.                         |
| [Compiler DLL Proxy](compiler-dll-proxy/compiler-dll-proxy.md) | Build artifact | The optional Windows compatibility DLL that forwards compiler exports from `slang.dll` to `slang-compiler.dll`.                                                    |
| [Runtime](runtime/runtime.md)                                  | Build artifact | Runtime operations called by generated host programs, including failure reporting and dynamic-library access.                                                      |
| [Adapters](adapters/adapters.md)                               | Package        | Separately loadable integrations with external toolchains and providers of compiled language-library contents.                                                     |
| [Tools](tools/tools.md)                                        | Package        | Executable and binding entry points through which clients use the compiler and related services.                                                                   |

Foundation and compiler support supply reusable services below the compiler.
Adapters implement separately loadable services that the compiler can request without linking the external implementation into the compiler library.
Compiler Support owns the shared adapter contracts, reusable invocation bases, and collections of compiler objects.
Compiler's existing Downstream Tools concern owns the complete concrete integrations: discovery, defaults and version selection, tool-specific request translation, invocation, output parsing, and dependent-library search.
The loaded external implementations remain with Adapters.
These assignments distinguish the common contracts from their concrete clients and implementations; the clients still depend on the implementations they load.
Tools turn client requests into calls to those services.
Runtime supports generated host programs and does not perform compilation for them.

Compiler DLL Proxy owns the existing `slang-proxy` artifact, enabled only on Windows when the compiler is shared and `SLANG_ENABLE_SLANG_PROXY` is enabled.
It depends on Compiler for its forwarded exports and the final compiler DLL used to generate the export list.
The Unix compatibility symlink remains Compiler packaging rather than another build artifact.

The compiler concern includes the code and resources that define bundled language libraries and the machinery that prepares their contents for embedding.
A separately loadable wrapper that publishes compiled library contents belongs to adapters.
The existing build also installs loose `.slang-module` files, which are serialized language modules rather than C++ libraries or executables.
The child decomposition must account for those resources without inventing a C++ build artifact for each resource or treating an object-library build step as an independently shipped library.

## Staged Migration Contracts

Each reusable Foundation service has one owning module even when the existing build compiles its implementation into both static `core` and enabled `slang-rt`.
Phase-two ports preserve both compilation consumers and their respective export settings; Runtime continues to compile those implementations separately.
The Random Numbers port follows this arrangement.
The general rules for source reuse across artifacts remain an [open architectural question](#unresolved-architectural-dependencies).

A Foundation C++ type used across an existing binary interface keeps its canonical identity, defining namespace, and layout during migration.
Its operations retain their existing names and signatures, and its declarations retain their export annotations.
For example, Runtime's C-linkage entry points pass `Slang::String` by value, and the string and native-ownership declarations, including `Slang::RefObject`, carry `SLANG_RT_API` annotations.
C linkage does not remove those C++ ABI requirements.
A module namespace may re-export the same declaration with a `using` declaration or type alias; the class definition remains in its existing namespace.
The pitch's correspondence between modules and C++ namespaces was tentative, so the prototype preserves these identities rather than requiring every declaration to move namespaces.
Unannotated internal types may move to their owning module namespaces after their binary-interface uses are audited, as Random Numbers did.
This limited namespace policy does not relax preservation of behavior or existing declaration names and signatures.

The concrete downstream integration assignment is accepted as semantic ownership, but its implementations still compile into legacy `compiler-core`.
Before their migration is accepted, source selection must remove those concrete implementations from Compiler Support and leave its shared services independent of Compiler policy.
Existing test and other callers must retain access to the internal operations they currently obtain from static definitions, including when the final Compiler library hides those symbols.
Moving the definitions into a shared Compiler library alone does not establish that linkability.
The migration must account for those build consumers without assuming new product exports, another library, or a callback protocol; no such mechanism is selected here.

## Unresolved Architectural Dependencies

The current CMake construction graph distinguishes individual targets and compiler variants.
With core-module embedding enabled, `slang-bootstrap` links `slang-without-embedded-core-module`, runs to generate embedded contents, and supplies inputs needed to build the final `slang` compiler target.
That target and variant ordering is acyclic; the final compiler is not the compiler variant used to generate its own inputs.
The organization must preserve that construction constraint.
It does not establish acyclicity after those distinct targets are assigned to the broader modules proposed here.

The compiler library variants contain ordinary session-creation and provider-loading services in their shared implementation.
The bootstrap driver's particular construction invocations select session modes that do not require already-generated built-ins.
Those invocations explain how the build proceeds, but do not remove the libraries' other capabilities from the architecture.
Separating target names or shared interfaces therefore does not prove an acyclic combined link, load, and construction graph.

Collapsing the observed link, load, and construction dependencies to the proposed owners exposes unresolved cycles:

- Compiler requires the bootstrap executable in tools to construct bundled language-library contents, while that bootstrap executable links a compiler variant.
  Both compiler variants currently belong to the proposed compiler artifact, yielding compiler to tools to compiler.
- Compiler loads compiled-library wrappers assigned to adapters, while constructing those wrappers requires language-library contents prepared by compiler.
  Assigning the implementation and its generated contents to different owners yields compiler to adapters to compiler.
  The GLSL wrapper and optional `slang-standard-modules` wrapper both expose this ownership question.

The current `compiler-core` implementation also loads adapter implementations while the LLVM adapter links `compiler-core`.
The accepted Downstream Tools assignment gives Compiler ownership of the complete concrete client service.
The current `compiler-core` to `slang-llvm` to `compiler-core` cycle remains until the implementations and their build consumers follow that assignment.
Moving a shared header or only `LLVMDownstreamCompilerUtil::locateCompilers` would leave the remaining concrete calls unaccounted for.

The compiler/bootstrap/provider assignments and the physical ownership of reused compiler and driver implementations still require a decision about artifacts and construction stages.
Foundation's existing separate runtime compilation must also be represented explicitly in that model.
The preservation contracts above do not select a general source-reuse rule or qualify the acyclicity requirement.
The affected artifact assignments and the whole architecture remain provisional while those rules are unmet.

## Existing Implementation Mapping

Paths in this section name the existing repository layout under `old-source/`.
Each owning module's mapping records the implementations transferred into the new hierarchy.
The assignments identify concerns rather than wholesale file moves; shared files and resources need a more precise assignment in the owning child's decomposition.

- **`old-source/core/`**
  Foundation owns the reusable services currently built into the static `core` target by `old-source/core/CMakeLists.txt`.
  Runtime also compiles these files under its own export settings, so migration preserves both consumers of the one Foundation-owned implementation.

- **`old-source/compiler-core/`**
  Compiler Support owns its shared services for program text, lexing, diagnostics, compilation outputs, downstream contracts, and JSON/protocol data.
  Compiler's Downstream Tools owns the complete concrete integrations identified in [Compiler's mapping](compiler/compiler.md#existing-implementation-mapping), including actual invocation and output parsing.
  The local `CMakeLists.txt` still builds both groups into static `compiler-core`; these are partial responsibility assignments, not completed source or build changes.

- **`old-source/slang/`**
  Compiler owns most of this directory: compiler API/session coordination, front end, IR, transformations, target emission, layout/reflection, serialization, and editing services.
  Its `.meta.slang` inputs and bundled-library preparation also belong to the compiler concern.
  The generated capability/lookup objects and fiddle output are build components of that implementation, not separate product libraries.
  Compiler DLL Proxy owns only the conditional Windows `slang-proxy` construction and installation concern in this directory's `CMakeLists.txt`, including the generated forwarding stub and export list.
  Compiler retains the final library, its variants, and its Unix compatibility symlink packaging.

- **`old-source/slang-record-replay/`**
  Compiler owns the recording and replay integration.
  `old-source/slang/CMakeLists.txt` conditionally includes these files in the compiler target; they do not form an independent library.
  The compiler API implementation includes recording/replay headers, and replay code includes compiler internals, so these directory boundaries do not establish separate acyclic modules.

- **`old-source/slang-core-module/`**
  Compiler owns the language-library generation and embedding machinery.
  The local build definitions produce embedded and non-embedded object-library variants, and invoke the bootstrap compiler to generate bundled modules.
  These variants support compiler construction rather than defining a separate public compiler service.

- **`old-source/slang-glsl-module/`**
  Adapters owns the runtime-loaded `slang-glsl-module` wrapper, which exposes generated GLSL library contents.
  Compiler owns preparation of those contents; the wrapper's local build definition explicitly requires the compiler bootstrap's generated header.
  This is a partial split across the two concerns.

- **`old-source/standard-modules/`**
  Compiler owns preparation of the Slang-language library inputs and serialized `.slang-module` resources.
  This directory's build definitions compile the modules and describe their installation.
  The eventual internal organization of library code and resources remains part of the compiler decomposition.

- **`old-source/slang-monolithic-standard-module-library/`**
  Adapters owns the optional `slang-standard-modules` runtime-loaded wrapper, built by this directory's `CMakeLists.txt`.
  It exposes compiled standard modules by import path.
  The Slang inputs and compilation remain with compiler, while the wrapper depends on their generated outputs.

- **`old-source/slang-glslang/`**
  Adapters owns the optional `slang-glslang` runtime-loaded implementation defined by its local `CMakeLists.txt`.
  Compiler Support owns the shared request contract currently in `slang-glslang.h`; the concrete locating and invoking client still in `old-source/compiler-core/slang-glslang-compiler.cpp` belongs to Compiler's Downstream Tools.
  This is a partial assignment of the directory.

- **`old-source/slang-llvm/`**
  Adapters owns the LLVM-backed implementation.
  `cmake/LLVM.cmake`, rather than a local `CMakeLists.txt`, builds `slang-llvm` or selects a fetched binary.
  Compiler support owns the shared builder contract currently in `slang-llvm-builder.h`; LLVM-specific definitions in that header remain part of the adapter concern.
  The compiler-side Slang-IR emitter remains in `old-source/slang/slang-emit-llvm.cpp`; the LLVM-side builder resides in this directory.
  These are partial assignments that require separation of shared declarations from LLVM implementation details.

- **`old-source/slang-rt/`**
  Runtime owns the `slang-rt` library and its exported operations.
  Its local build definition recompiles `old-source/core/` using runtime export settings, so the directory alone does not contain the full implementation compiled into this artifact.

- **`old-source/slangc/`**
  Tools owns the `slangc` executable entry point.
  `tools/CMakeLists.txt` also builds the same entry-point code as `slang-bootstrap`, linked against a compiler variant without the embedded core module.

- **`old-source/slang-dispatcher/`**
  Tools owns the dispatcher executable, whose local build definition emits the `slang` program.

- **`old-source/slang-wasm/`**
  Tools owns the WebAssembly bindings.
  The Emscripten build uses an executable target to produce bindings and JavaScript/WebAssembly outputs; that target kind does not describe a conventional host executable.

Existing programs under `tools/`, such as `slangd`, `slangi`, the test runners, and the generators, remain outside this mapping's implementation scope.
Some call compiler services and some construct build inputs.
The tools package described here initially covers the entry points currently under `old-source/`; including an existing tool as a build participant does not relocate its implementation.
