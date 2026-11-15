<!--
SPDX-FileCopyrightText: The Khronos Group, Inc.
SPDX-License-Identifier: CC-BY-4.0
-->

# Adapters

The adapters package groups separately loadable integrations with external toolchains and providers of compiled language-library contents.
Keeping LLVM/Clang and glslang implementations in separate libraries lets a distribution supply those integrations without linking their external implementations into the compiler library.

Adapters is a package, with no implementation or link step of its own.
Its four proposed children correspond to existing runtime-loaded libraries; their implementations remain in `old-source/`.
The [root's unresolved dependencies](../slang.md#unresolved-architectural-dependencies) still prevent accepting the complete artifact graph as acyclic.

## Concepts

The [root concepts](../slang.md#concepts) define packages, build artifacts, public visibility, and link, load, and construction dependencies.
[Compiler Support](../compiler-support/compiler-support.md#concepts) defines downstream compilers and compilation artifacts.

- A **library entry point** is an exported function that a client can locate by name in a loaded implementation library.
  These functions are separate from shader entry points in a program being compiled.
- A **compiled language library** contains serialized Slang modules that the compiler can load instead of compiling their declarations again.
  A **provider library** publishes those bytes through library entry points; it does not perform the Slang-language compilation that prepared them.
- A **JIT library** exposes symbol lookup for machine code generated in memory.
  The LLVM integration implements the product `ISlangSharedLibrary` interface for that purpose without requiring a shared-library file on disk.
- A **build tag** identifies a Slang build using its full version string.
  The compiler compares provider-library tags with its own tag before using their compiled contents.
  That check is separate from the versioned entry points and request layouts used by toolchain integrations.

## Dependencies

The children use the following services and inputs inherited through the root package.
This is the package's combined dependency inventory; each child must refine the subset its own implementation uses.

- [Foundation](../foundation/foundation.md) supplies native storage and reference-counted objects, collections, text, blobs, and host-library services.
  The locally built LLVM library links `core`, as do the two provider libraries.
  The glslang wrapper includes Foundation's exception-configuration macros without linking `core`.
- [Compiler Support](../compiler-support/compiler-support.md) supplies compilation artifacts, diagnostics associated with results, and shared downstream-compiler contracts used by the LLVM integration.
  The locally built LLVM library links `compiler-core`.
  The shared LLVM-builder and glslang request contracts also belong to Compiler Support; moving their headers alone would not change the current load dependencies.
- LLVM and Clang supply C/C++ compilation, LLVM code construction and emission, JIT execution, target information, and FileCheck matching.
  `cmake/LLVM.cmake` can select a prebuilt `slang-llvm` library or compile the integration against system-provided LLVM and Clang.
- glslang and SPIRV-Tools supply GLSL compilation and SPIR-V optimization, validation, disassembly, and linking.
  The glslang wrapper links their implementations; its optional allocator integration uses the same mimalloc target selected for SPIRV-Tools.
- [Compiler](../compiler/compiler.md) supplies the prepared GLSL-library header and serialized standard modules consumed by the provider-library builds.
  The bootstrap compiler and the existing monolithic-library embedding generator are construction dependencies, rather than implementations linked into either provider library.
  The embedding generator compresses standard-module contents with Foundation's LZ4 service; the compiler decompresses them when loading, while the provider only returns the stored bytes.
- Repository CMake configuration supplies the build-tag header used by both provider libraries.
  The product headers and host C++ environment supply interfaces, scalar types, calling conventions, allocation, and synchronization.
- The existing test contract in `tools/slang-test/filecheck.h` supplies the FileCheck interface implemented by the LLVM library.
  Its ownership is an ancestor question described below; using that header does not mean the LLVM library links the test executable.

## Decomposition

All four immediate children are public build artifacts.
The package introduces no ordinary implementation child and no additional library.

- **[LLVM Toolchain](llvm-toolchain/llvm-toolchain.md)** implements LLVM/Clang services in `slang-llvm`.
  Its services include C/C++ compilation to host-callable code, LLVM code construction and emission, JIT symbol lookup, target-layout queries, and the existing FileCheck implementation.
  This child implements LLVM-specific operations; Compiler interprets Slang IR and selects calls to the shared builder interface.
- **[GLSL and SPIR-V Toolchain](glsl-and-spirv-toolchain/glsl-and-spirv-toolchain.md)** implements `slang-glslang`.
  It translates GLSL to SPIR-V and provides the SPIR-V processing operations supplied by glslang and SPIRV-Tools.
  Calling services prepare the initial client request from compilation inputs and options, select the compiler, and discover the integration.
  The wrapper also normalizes received request layouts in its compatibility entry points.
- **[GLSL Library Provider](glsl-library-provider/glsl-library-provider.md)** implements `slang-glsl-module`.
  It publishes the compiler-prepared GLSL library as a blob and exposes the build tag used to check that library before loading it.
- **[Standard Library Provider](standard-library-provider/standard-library-provider.md)** implements the optional `slang-standard-modules` library.
  It publishes compressed serialized modules by import path, as an alternative to installing the loose standard-module directory.
  The compiler continues to own import resolution, feature checks, decompression, and loading the serialized modules.

The two provider libraries are different build artifacts from the serialized modules they contain.
The core-module object-library variants and loose `.slang-module` resources remain with Compiler; they are not additional children of Adapters.
The existing output names, version suffixes where configured, library entry points, and cross-library contracts constrain later migration.

## Existing Implementation Mapping

The following assignments describe responsibility, including the mixed headers that require separation before migration.
Within each item, an unqualified filename uses the first file's directory.

- **LLVM Toolchain:** `old-source/slang-llvm/slang-llvm.cpp`, `slang-llvm-builder.cpp`, `slang-llvm-jit-shared-library.{h,cpp}`, `slang-llvm-filecheck.cpp`, and `README.md`.
  The first file implements the Clang-backed downstream compiler and target-layout query; the builder file implements LLVM code construction and its generated outputs.
  The JIT files manage LLVM execution machinery and expose symbol lookup; the FileCheck file implements matching for the existing test interface.
  In `slang-llvm-builder.h`, only the LLVM header inclusions and `SLANG_LLVM_IMPL` type aliases belong to this child.
  The opaque client declarations, builder options and enums, and `ILLVMBuilder` interface belong to Compiler Support's Downstream Compilation service.
  `old-source/compiler-core/slang-target-builtin-type-layout-info.h` is also a shared contract rather than LLVM implementation.
  `cmake/LLVM.cmake` currently assembles or selects the artifact.
  `old-source/slang-llvm/` has no local `CMakeLists.txt`.
- **GLSL and SPIR-V Toolchain:** `old-source/slang-glslang/slang-glslang.cpp`, `CMakeLists.txt`, and `slang-glslang.version-script`.
  These provide the wrapper implementation, external link and visibility configuration, and exported-name list.
  `slang-glslang.h` contains the shared request versions, callback and function types, and request-conversion operations assigned to Compiler Support's Downstream Compilation service.
- **GLSL Library Provider:** `old-source/slang-glsl-module/slang-embedded-glsl-module.cpp` and its local `CMakeLists.txt`.
  The wrapper contains a static blob over the generated byte array.
  The input header is generated by the bootstrap invocation in `old-source/slang-core-module/CMakeLists.txt`; preparing that content remains with Compiler.
- **Standard Library Provider:** `old-source/slang-monolithic-standard-module-library/slang-monolithic-standard-module-library.cpp`, its local `CMakeLists.txt`, and `README.md`.
  The wrapper publishes the generated path-to-bytes table; its build definition coordinates per-configuration embedding from the standard-module output manifest.
  The Slang-language inputs and their compilation remain in `old-source/standard-modules/` under Compiler.
  The existing generator in `tools/slang-generate-monolithic-standard-module-library/` is a build participant outside this mapping's implementation scope.

The compiler-side loaders in `old-source/slang/slang-api.cpp`, `slang-session.cpp`, and `slang-global-session.cpp` remain with Compiler.
The Slang IR emitter in `old-source/slang/slang-emit-llvm.cpp` and layout-query caller in `slang-type-layout.cpp` also remain with Compiler.
The concrete LLVM and glslang discovery/invocation code currently in `old-source/compiler-core/` belongs to Compiler's Downstream Tools under the [accepted sibling assignment](../compiler-support/compiler-support.md#concrete-tool-integration).

## Ancestor Decisions Before Migration

The root's compiler-to-provider construction/load cycle remains unresolved.
For example, `generate_glsl_module_header` depends on a bootstrap compilation of the GLSL library, the GLSL provider embeds that output, and the compiler loads the provider through `tryLoadBuiltinModuleFromDLL`.
The optional standard-library provider similarly requires modules prepared by Compiler and supplies bytes that Compiler loads during import resolution.
Separating the provider wrapper from the prepared library content does not remove those dependencies.

The LLVM integration's link dependency on Compiler Support is also real.
The root has accepted Compiler's ownership of the complete concrete integration service, including the actual calls into loaded implementations.
Those calls still compile into `compiler-core`, so its current load/link cycle with `slang-llvm` has not been removed.
Migration must satisfy the [root's source-selection and existing-caller conditions](../slang.md#staged-migration-contracts) before Compiler Support's implementation dependency can be considered removed.
This semantic assignment does not resolve the separate provider or bootstrap cycles.

The root must also assign the FileCheck interface and its test-result/callback contract alongside the existing test infrastructure.
The LLVM library implements that interface, while the test runner loads the implementation by its versioned entry point.
Those shared declarations need an explicit owner before the LLVM child can claim a complete dependency analysis.

Adapters remains on the refinement worklist until these ancestor questions are resolved.
The child stubs identify the four existing services without claiming that their internal decomposition or migration is complete.
