---
generated: true
model: claude-sonnet-5-5
generated_at: 2026-11-03T00:00:00Z
source_commit: 2518a942d59b1228fc8cc70a37739baab82b6002
watched_paths_digest: 0b13514236df58d535ec2ed36e2311b6d01cb5c18433dcafd2320abf4f924a3f
warning: "Auto-generated. May drift from source. Do not edit by hand."
---

<!--
Local implementation link destinations were mechanically relocated from
source/ to old-source/ for this checkout.
The `source_commit` and `watched_paths_digest` above still record the original generation inputs.
-->

# Architectural Overview

This document is the entry point for the LLM-generated architectural
documentation under [docs/generated/design/](..). It introduces Slang as a
codebase: the public artefacts it produces, the major subsystems that
make up the source tree, and the central data objects that flow through
a compilation. After reading it you should know which subdirectory under
[source/](../../../../old-source) implements any given concern, and where to
go next for detail. It is written for a competent C++ engineer who has
not yet opened the Slang source tree.

## Purpose

Slang is a shading-language compiler. It takes Slang (and
HLSL-compatible) source and produces target code — DXIL, SPIR-V, GLSL,
Metal Shading Language, WGSL, C++, CUDA, or PyTorch glue — together with
reflection / layout information about shader parameters.

The build produces four primary public artefacts:

- `slangc`: the command-line compiler driver, built from
  [source/slangc/](../../../../old-source/slangc).
- `slang-compiler` (a shared library): the embeddable compiler; its
  public C API and COM-style interfaces are declared in
  [include/slang.h](../../../../include/slang.h). Under
  `SLANG_ENABLE_SLANG_PROXY` a `libslang` symlink (Unix) and a
  forwarding `slang.dll` (Windows) keep the historical names working.
- the WebAssembly bindings built from
  [source/slang-wasm/](../../../../old-source/slang-wasm), exposing the
  compiler to JavaScript with a generated `interface.d.ts`.
- `slang-rt`: optional runtime support used by code emitted to non-GPU
  targets, built from [source/slang-rt/](../../../../old-source/slang-rt).

The compiler is built with CMake. The core library's build configuration
lives in
[source/slang/CMakeLists.txt](../../../../old-source/slang/CMakeLists.txt);
each peer subdirectory under [source/](../../../../old-source) has its own
`CMakeLists.txt`. Paths there are written relative to `slang_BINARY_DIR`
rather than `CMAKE_BINARY_DIR`: the `copy_slang_headers` target stages
[include/](../../../../include) plus the generated `slang-tag-version.h`
into `${slang_BINARY_DIR}/$<CONFIG>/include` (lines 257-269), so a parent
project embedding Slang with `add_subdirectory` collects the headers
under Slang's own build directory.

## Top-level decomposition

The source tree is layered: lower layers do not depend on upper ones,
and the public API in [include/](../../../../include) is the boundary.

### Foundational layers

- [source/core/](../../../../old-source/core) — platform-agnostic C++
  utilities: containers, strings, smart pointers, file-system
  abstractions, hashing, allocation. Representative file:
  [slang-basic.h](../../../../old-source/core/slang-basic.h). Setting
  `SLANG_ENABLE_MIMALLOC` makes
  [source/core/CMakeLists.txt](../../../../old-source/core/CMakeLists.txt)
  link `mimalloc-static` into `core`.
- [source/compiler-core/](../../../../old-source/compiler-core) — language-
  agnostic compiler infrastructure
  ([CMakeLists.txt](../../../../old-source/compiler-core/CMakeLists.txt)):
  lexer
  ([slang-lexer.cpp](../../../../old-source/compiler-core/slang-lexer.cpp)),
  diagnostic sink
  ([slang-diagnostic-sink.h](../../../../old-source/compiler-core/slang-diagnostic-sink.h)),
  source-location encoding, downstream-compiler glue (DXC, FXC, GCC,
  glslang), and the artifact / blob model for compiled outputs.

### The compiler proper

- [source/slang/](../../../../old-source/slang) — the Slang frontend, AST,
  intermediate representation, IR passes, and emit backends; the bulk of
  the compiler. Subgroups by file-name prefix: `slang-ast-*` (AST),
  `slang-parser*` and `slang-preprocessor*` (frontend), `slang-check*`
  (semantic checking), `slang-lower-to-ir*` (AST to IR lowering),
  `slang-ir.*` and `slang-ir-insts.*` (IR core), `slang-ir-*.cpp` (IR
  passes), `slang-emit*` (code emission), `slang-serialize*`
  (serialization), `slang-capability*` (capability lattice),
  `slang-diagnostics*` (diagnostic catalog). See
  [module-map.md](module-map.md) for the file-level inventory.
- [prelude/](../../../../prelude) — per-target prelude headers shipped
  with emitted text targets so the downstream toolchain can compile the
  result, e.g.
  [slang-cuda-prelude.h](../../../../prelude/slang-cuda-prelude.h).

### Standard libraries

- [source/slang-core-module/](../../../../old-source/slang-core-module) and
  the `*.meta.slang` files in [source/slang/](../../../../old-source/slang)
  (e.g. [core.meta.slang](../../../../old-source/slang/core.meta.slang),
  [hlsl.meta.slang](../../../../old-source/slang/hlsl.meta.slang)) — the core
  module defining built-in types, intrinsics, and operator mappings,
  embedded into `slang-compiler` at build time. When
  `SLANG_EMBED_CORE_MODULE` is off and `slang-compiler` is shared,
  [source/slang/CMakeLists.txt](../../../../old-source/slang/CMakeLists.txt)
  adds a `generate_core_module_cache` target (`slang-core-module.bin`).
- [source/slang-glsl-module/](../../../../old-source/slang-glsl-module) and
  [source/slang/glsl.meta.slang](../../../../old-source/slang/glsl.meta.slang)
  — analogous module for GLSL-flavoured intrinsics.
- [source/standard-modules/](../../../../old-source/standard-modules)
  ([CMakeLists.txt](../../../../old-source/standard-modules/CMakeLists.txt))
  — standard-module sources that are shipped but not embedded. The
  subdirectories `neural`, `experimental` (currently the work-graph
  module), `numerics`
  ([CMakeLists.txt](../../../../old-source/standard-modules/numerics/CMakeLists.txt)),
  and `functional`
  ([CMakeLists.txt](../../../../old-source/standard-modules/functional/CMakeLists.txt))
  build separately distributed serialized modules.

### Downstream-compiler shims

- [source/slang-llvm/](../../../../old-source/slang-llvm) — LLVM-based JIT /
  static compilation glue
  ([slang-llvm.cpp](../../../../old-source/slang-llvm/slang-llvm.cpp)).
- [source/slang-glslang/](../../../../old-source/slang-glslang) — bridge to
  Khronos `glslang` for SPIR-V generation via GLSL
  ([slang-glslang.cpp](../../../../old-source/slang-glslang/slang-glslang.cpp)).
  An exported entry point must also be listed in
  `slang-glslang.version-script`, or the client's symbol lookup returns
  null (e.g. `glslang_linkSPIRV`, `glslang_freeLinkResult`).
- [source/slang-dispatcher/](../../../../old-source/slang-dispatcher) — the
  `slang` command-line dispatcher
  ([main.cpp](../../../../old-source/slang-dispatcher/main.cpp)), which
  launches `slangc` (`compile`), `slangi` (`interpret`), and other named
  subcommand executables, and handles `help` and `version` itself.

A downstream compiler is reached through `IDownstreamCompiler`, declared
in
[slang-downstream-compiler.h](../../../../old-source/compiler-core/slang-downstream-compiler.h).
Capabilities only some compilers have are separate interfaces: for
instance `IDownstreamCompilerPathProvider` (line 403), whose `getPath`
returns the on-disk path of the loaded compiler library. It is reached
through `castAs` / `getObject`, not `getInterface`, which backs the
ref-counting `queryInterface`. `DownstreamCompilerBase` defaults
`getPath` to `SLANG_E_NOT_AVAILABLE`; only shared-library-backed
compilers override it.
[slang-llvm.cpp](../../../../old-source/slang-llvm/slang-llvm.cpp)
implements the pattern on `LLVMDownstreamCompiler`.

### Runtime and bindings

- [source/slang-rt/](../../../../old-source/slang-rt)
  ([CMakeLists.txt](../../../../old-source/slang-rt/CMakeLists.txt)) —
  runtime library linked into host-style CPU outputs, including the
  PyTorch binding output; CUDA source does not take this dependency.
- [source/slang-record-replay/](../../../../old-source/slang-record-replay)
  — recorder/replayer for the public Slang API
  ([replay-context.cpp](../../../../old-source/slang-record-replay/replay-context.cpp)).
  When the CMake option `SLANG_ENABLE_RECORD_REPLAY` (default `ON`) is
  `OFF`, the record-replay sources are left out of the `slang` library
  and `slang_loadReplay` / `slang_loadLatestReplay` return
  `SLANG_E_NOT_AVAILABLE`.
- [source/slang-wasm/](../../../../old-source/slang-wasm) — WebAssembly
  bindings
  ([slang-wasm-bindings.cpp](../../../../old-source/slang-wasm/slang-wasm-bindings.cpp)).
  The Emscripten binding block is the authoritative list of what
  JavaScript can reach, e.g. `GlobalSession::createSession`,
  `Session::loadModuleFromIRBlob`, and `Module::serialize`.

### Driver and tooling

- [source/slangc/](../../../../old-source/slangc) — the `slangc` command-line
  driver ([main.cpp](../../../../old-source/slangc/main.cpp)).
- [tools/](../../../../tools) — auxiliary developer tools (testing,
  reflection, code generation, fiddle, embed, profiling), e.g. the test
  runner
  [slang-test-main.cpp](../../../../tools/slang-test/slang-test-main.cpp).

### Auxiliary trees (outside `source/`)

- [tests/](../../../../tests) — the test corpus, mostly `.slang` inputs
  driven by `slang-test`, laid out by area, e.g.
  [tests/language-feature/](../../../../tests/language-feature).
- [extras/](../../../../extras) — developer scripts not built into any
  binary, e.g. [formatting.sh](../../../../extras/formatting.sh).
- [external/](../../../../external) — vendored third-party dependencies
  and git submodules (glslang, lz4, miniz, ...), built via
  [external/CMakeLists.txt](../../../../external/CMakeLists.txt).

### Build-time generated code

The `FIDDLE(...)` macro in AST and IR headers expands to code generated
under `build/source/slang/fiddle/`; IR opcodes come from
[slang-ir-insts.lua](../../../../old-source/slang/slang-ir-insts.lua) and
diagnostics from
[slang-diagnostics.lua](../../../../old-source/slang/slang-diagnostics.lua)
plus the tables under
[source/slang/diagnostics/](../../../../old-source/slang/diagnostics).

## Compilation request lifecycle

A Slang compilation flows through a small set of central objects
declared in [include/slang.h](../../../../include/slang.h) and headers
under [source/slang/](../../../../old-source/slang), including
`slang-translation-unit.h`, `slang-session.h`, `slang-target.h`,
[slang-compile-request.h](../../../../old-source/slang/slang-compile-request.h),
`slang-end-to-end-request.h`, and
[slang-module.h](../../../../old-source/slang/slang-module.h).

- `Session` — global-session-scoped compiler state: built-in modules, the
  AST builder, and the global type-checking environment. The public
  interface is `slang::IGlobalSession`; the implementation class is in
  [slang-global-session.h](../../../../old-source/slang/slang-global-session.h).
- `Linkage` — a configuration scope bundling search paths, preprocessor
  macros, target settings, and a source manager, so modules can resolve
  `import`s against each other. It implements the public
  `slang::ISession` (see
  [slang-session.h](../../../../old-source/slang/slang-session.h)); so
  "session" means two things: `IGlobalSession` is created once and
  reused, `ISession` is the per-configuration "compile session".
- `TranslationUnitRequest` — a collection of source files sharing a
  namespace; Slang files passed to `slangc` share one unit, HLSL inputs
  get one each.
- `FrontEndEntryPointRequest` — a function name plus the profile whose
  stage it is compiled for (e.g. `main` as compute). Declared in
  [slang-compile-request.h](../../../../old-source/slang/slang-compile-request.h).
- `TargetRequest` — an output format combined with a profile
  (e.g. SPIR-V at `glsl_450`).
- `FrontEndCompileRequest` — drives the front end (parse, check, lower
  to IR) for a set of translation units; declared in
  [slang-compile-request.h](../../../../old-source/slang/slang-compile-request.h).
- `CodeGenContext` — drives the back end (IR passes, emit); declared in
  [slang-code-gen.h](../../../../old-source/slang/slang-code-gen.h). It
  replaces the historical `BackEndCompileRequest`.
- `EndToEndCompileRequest` — the umbrella object behind one `slangc`
  invocation, declared in `slang-end-to-end-request.h`.
- `Module` — the front-end output for a translation unit. Implements
  `slang::IModule`, contains both the checked AST and the lowered IR, and
  is the unit the `.slang-module` format describes. Declared in
  [slang-module.h](../../../../old-source/slang/slang-module.h).
- `IComponentType` — the linkable-program abstraction: a `Module`, an
  entry-point binding, or a composite of these; the back end consumes a
  single composite.

Front-end work is separated from back-end work: parsing and reusable
module checking are largely target-independent, but entry-point
validation compares capability requirements and profile stages against
every `TargetRequest` in
[slang-check-shader.cpp](../../../../old-source/slang/slang-check-shader.cpp);
code generation happens in the back end.

## Where the public API lives

[include/slang.h](../../../../include/slang.h) is the canonical public
header. It declares the COM-style interfaces (`ISession`, `IModule`,
`IComponentType`, ...) and the free functions that create a session.
With [include/slang-com-helper.h](../../../../include/slang-com-helper.h)
and [include/slang-com-ptr.h](../../../../include/slang-com-ptr.h), this
is the binary-stable surface applications link against.

Anything under [source/](../../../../old-source) is implementation. The
public-header rules in [CLAUDE.md](../../../../CLAUDE.md) (no enum
re-ordering, no mid-vtable changes, no removal) preserve ABI
compatibility, so new API is added by appending:

- **Appending a method** to the end of an interface vtable (e.g.
  `IGlobalSession::saveBuiltinModule`).
- **A fresh UUID'd interface** for a capability not every implementation
  has (e.g. `IBindlessResourceMetadata`), obtained via `castAs`.
- **Appending an enumerator.** New `CompilerOptionName` values take an
  explicit integer before the `CountOf` sentinel; the latest are
  `DiagnosticFormat = 162` and
  `EnableExtendedHLSLBackwardsCompatibility = 163`. A superseded option
  stays, deprecated: `UseMSVCStyleBitfieldPacking = 128`, replaced by
  `BitfieldPackingRules = 160`.
- **Tail-extending a struct behind `structSize`.** Trailing members are
  written only when the caller's `structSize` covers them (e.g.
  `SyntheticResourceInfo::bindlessIndex`).

Superseded entry points are normally deprecated, not deleted
(`IGlobalSession::addBuiltins` is `[[deprecated]]`). Two departures:
`getDownstreamCompilerVersion` was replaced in place by
`getDownstreamCompilerPath` (commit `b9a17f86b1`, last vtable slot), and
`CompilerOptionName::CompilerVersion = 153` was removed outright,
leaving a numeric hole so the integer is not reused.

The language version is separate from the ABI and enumerated by
`SlangLanguageVersion`: `SLANG_LANGUAGE_VERSION_2025` / `_2026` have
aliases `_202A` / `_202B`, plus `_202C = 2027` for the in-progress
version, whose value the header warns may change.

## Reading guide

To go deeper, follow one of these paths:

- For a file inventory by subsystem, read
  [module-map.md](module-map.md).
- For inter-subsystem dependencies, read
  [dependency-graph.md](dependency-graph.md).
- For the end-to-end flow, one document per stage, start at [../pipeline/overview.md](../pipeline/overview.md).
- For concerns that span every stage, see the
  [../cross-cutting/](../cross-cutting) tree:
  [diagnostics](../cross-cutting/diagnostics.md),
  [IR instructions](../cross-cutting/ir-instructions.md),
  [targets](../cross-cutting/targets.md),
  [core module](../cross-cutting/core-module.md), and
  [serialization](../cross-cutting/serialization.md).
- For syntax, see [../syntax-reference/](../syntax-reference).

The handwritten notes in [docs/design/](../../../design) overlap with
this tree; particularly useful are
[overview.md](../../../design/overview.md),
[ir.md](../../../design/ir.md), and
[parsing.md](../../../design/parsing.md).
