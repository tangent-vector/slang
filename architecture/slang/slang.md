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

| Child                                                    | Kind           | Responsibility                                                                                                                                                                             |
| -------------------------------------------------------- | -------------- | ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------ |
| [Foundation](foundation/foundation.md)                   | Build artifact | Reusable data structures, memory and text services, and access to the host environment.                                                                                                    |
| [Compiler Support](compiler-support/compiler-support.md) | Build artifact | Shared services for reading program text, reporting diagnostics, representing compilation outputs, and communicating with compilation tools, including their shared integration contracts. |
| [Compiler](compiler/compiler.md)                         | Build artifact | Program loading, checking, transformation, code generation, reflection, and compiler-backed editing services.                                                                              |
| [Runtime](runtime/runtime.md)                            | Build artifact | Runtime operations called by generated host programs, including failure reporting and dynamic-library access.                                                                              |
| [Adapters](adapters/adapters.md)                         | Package        | Separately loadable integrations with external toolchains and providers of compiled language-library contents.                                                                             |
| [Tools](tools/tools.md)                                  | Package        | Executable and binding entry points through which clients use the compiler and related services.                                                                                           |

Foundation and compiler support supply reusable services below the compiler.
Adapters implement separately loadable services that the compiler can request without linking the external implementation into the compiler library.
The proposed assignment places shared adapter contracts in compiler support.
That separates ownership of shared declarations from ownership of implementations, but does not remove the loaders' runtime dependencies on adapter implementations.
Tools turn client requests into calls to those services.
Runtime supports generated host programs and does not perform compilation for them.

The compiler concern includes the code and resources that define bundled language libraries and the machinery that prepares their contents for embedding.
A separately loadable wrapper that publishes compiled library contents belongs to adapters.
The existing build also installs loose `.slang-module` files, which are serialized language modules rather than C++ libraries or executables.
The child decomposition must account for those resources without inventing a C++ build artifact for each resource or treating an object-library build step as an independently shipped library.

## Unresolved Architectural Dependencies

The current CMake construction graph distinguishes individual targets and compiler variants.
With core-module embedding enabled, `slang-bootstrap` links `slang-without-embedded-core-module`, runs to generate embedded contents, and supplies inputs needed to build the final `slang` compiler target.
That target and variant ordering is acyclic; the final compiler is not the compiler variant used to generate its own inputs.
The organization must preserve that construction constraint.
It does not establish acyclicity after those distinct targets are assigned to the broader modules proposed here.

Collapsing the observed link, load, and construction dependencies to the proposed owners exposes unresolved cycles:

- Compiler requires the bootstrap executable in tools to construct bundled language-library contents, while that bootstrap executable links a compiler variant.
  Both compiler variants currently belong to the proposed compiler artifact, yielding compiler to tools to compiler.
- Compiler loads compiled-library wrappers assigned to adapters, while constructing those wrappers requires language-library contents prepared by compiler.
  Assigning the implementation and its generated contents to different owners yields compiler to adapters to compiler.
  The GLSL wrapper and optional `slang-standard-modules` wrapper both expose this ownership question.
- Compiler support locates and loads adapter implementations, while the LLVM adapter links compiler support.
  `LLVMDownstreamCompilerUtil::locateCompilers` and the `slang-llvm` link dependencies expose compiler support to adapters to compiler support.

Extracting a shared interface does not remove any of those runtime implementation or generated-input dependencies.
The compiler, compiler-support, adapters, and tools assignments therefore remain provisional and require architectural revisiting.
The proposed hierarchy has not yet satisfied its acyclicity requirement for those portions of the implementation.

## Existing Implementation Mapping

Paths in this section name the existing repository layout under `source/`.
During migration, the same original files reside under `old-source/` until transferred into the new hierarchy.
The assignments identify concerns rather than wholesale file moves; shared files and resources need a more precise assignment in the owning child's decomposition.

- **`source/core/`**
  Foundation owns the reusable services currently built into the static `core` target by `source/core/CMakeLists.txt`.
  Runtime also compiles these files, so migration affects both builds.

- **`source/compiler-core/`**
  Compiler support owns the services currently built into the static `compiler-core` target by its local `CMakeLists.txt`.
  Its lexer, diagnostics, compilation-output services, downstream integrations, and JSON/protocol code still require internal decomposition.

- **`source/slang/`**
  Compiler owns most of this directory: compiler API/session coordination, front end, IR, transformations, target emission, layout/reflection, serialization, and editing services.
  Its `.meta.slang` inputs and bundled-library preparation also belong to the compiler concern.
  The generated capability/lookup objects and fiddle output are build components of that implementation, not separate product libraries.

- **`source/slang-record-replay/`**
  Compiler owns the recording and replay integration.
  `source/slang/CMakeLists.txt` conditionally includes these files in the compiler target; they do not form an independent library.
  The compiler API implementation includes recording/replay headers, and replay code includes compiler internals, so these directory boundaries do not establish separate acyclic modules.

- **`source/slang-core-module/`**
  Compiler owns the language-library generation and embedding machinery.
  The local build definitions produce embedded and non-embedded object-library variants, and invoke the bootstrap compiler to generate bundled modules.
  These variants support compiler construction rather than defining a separate public compiler service.

- **`source/slang-glsl-module/`**
  Adapters owns the runtime-loaded `slang-glsl-module` wrapper, which exposes generated GLSL library contents.
  Compiler owns preparation of those contents; the wrapper's local build definition explicitly requires the compiler bootstrap's generated header.
  This is a partial split across the two concerns.

- **`source/standard-modules/`**
  Compiler owns preparation of the Slang-language library inputs and serialized `.slang-module` resources.
  This directory's build definitions compile the modules and describe their installation.
  The eventual internal organization of library code and resources remains part of the compiler decomposition.

- **`source/slang-monolithic-standard-module-library/`**
  Adapters owns the optional `slang-standard-modules` runtime-loaded wrapper, built by this directory's `CMakeLists.txt`.
  It exposes compiled standard modules by import path.
  The Slang inputs and compilation remain with compiler, while the wrapper depends on their generated outputs.

- **`source/slang-glslang/`**
  Adapters owns the optional `slang-glslang` runtime-loaded implementation defined by its local `CMakeLists.txt`.
  Compiler support owns the shared request contract currently in `slang-glslang.h`, which compiler support already includes when locating the library.
  This is a partial assignment of the directory.

- **`source/slang-llvm/`**
  Adapters owns the LLVM-backed implementation.
  `cmake/LLVM.cmake`, rather than a local `CMakeLists.txt`, builds `slang-llvm` or selects a fetched binary.
  Compiler support owns the shared builder contract currently in `slang-llvm-builder.h`; LLVM-specific definitions in that header remain part of the adapter concern.
  The compiler-side Slang-IR emitter remains in `source/slang/slang-emit-llvm.cpp`; the LLVM-side builder resides in this directory.
  These are partial assignments that require separation of shared declarations from LLVM implementation details.

- **`source/slang-rt/`**
  Runtime owns the `slang-rt` library and its exported operations.
  Its local build definition recompiles `source/core/` using runtime export settings, so the directory alone does not contain the full implementation compiled into this artifact.

- **`source/slangc/`**
  Tools owns the `slangc` executable entry point.
  `tools/CMakeLists.txt` also builds the same entry-point code as `slang-bootstrap`, linked against a compiler variant without the embedded core module.

- **`source/slang-dispatcher/`**
  Tools owns the dispatcher executable, whose local build definition emits the `slang` program.

- **`source/slang-wasm/`**
  Tools owns the WebAssembly bindings.
  The Emscripten build uses an executable target to produce bindings and JavaScript/WebAssembly outputs; that target kind does not describe a conventional host executable.

Existing programs under `tools/`, such as `slangd`, `slangi`, the test runners, and the generators, remain outside this mapping's implementation scope.
Some call compiler services and some construct build inputs.
The tools package described here initially covers the entry points currently under `source/`; including an existing tool as a build participant does not relocate its implementation.
