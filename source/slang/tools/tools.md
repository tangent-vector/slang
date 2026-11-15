<!--
SPDX-FileCopyrightText: The Khronos Group, Inc.
SPDX-License-Identifier: CC-BY-4.0
-->

# Tools

Tools provides command-line and JavaScript access to the compiler and its editing services.
Its compiler driver submits compilation requests and reports their results, its command dispatcher launches installed tools, and its WebAssembly bindings translate JavaScript calls and values into compiler operations.

Tools is a package with no implementation or link step of its own.
The initial assignment covers the entry points in `old-source/slangc/`, `old-source/slang-dispatcher/`, and `old-source/slang-wasm/`.
Existing implementations under the repository's `tools/` directory remain outside that assignment, including the interpreter, language-server executable, test infrastructure, and generators.
Those programs can still be clients, launched commands, or construction dependencies of the artifacts described here.

## Concepts

The [root concepts](../slang.md#concepts) define packages, build artifacts, visibility, and link, load, and construction dependencies.
[Compiler](../compiler/compiler.md#concepts) defines global and compilation sessions, component types, program layouts, reflection, and serialized modules.
[Compiler Support](../compiler-support/compiler-support.md#concepts) defines diagnostics and compilation artifacts; those artifacts are distinct from executable build artifacts.

- A **compiler driver** adapts a process invocation to compiler requests, diagnostic output, and an exit status.
  The compiler owns the interpretation of compiler options and the requested compilation operations.
- A **bootstrap compiler** is the host-runnable compiler-driver variant used to prepare bundled language libraries during a build.
  Its compiler library can compile built-in declarations without already containing their compiled representation.
  A cross-compilation build can import an existing host bootstrap executable instead of constructing one with the target toolchain.
- A **command dispatcher** chooses and launches another installed executable from the command name or invocation form.
  The `slang` dispatcher runs tools such as `slangc` and `slangi` in child processes; it does not call their compiler APIs in its own process.
- A **JavaScript binding** translates JavaScript values and calls to the C++ compiler and editing interfaces through Emscripten.
  The binding includes its own descriptor conversion, result conversion, and object-lifetime adaptation.

## Dependencies

The following inventory combines the children's dependencies; refining a child must identify the subset its services require.

- [Compiler](../compiler/compiler.md) supplies linked compilation, reflection, module serialization, built-in-library access, and editor services.
  `slangc` links the final compiler target; `slang-bootstrap` links `slang-without-embedded-core-module`, which aliases the final target when core-module embedding is disabled.
  The WebAssembly binding also directly uses the internal language-server implementation.
- [Foundation](../foundation/foundation.md) supplies text, collections, native and interface ownership, blobs, output writers, host paths, and executable discovery.
  Each entry-point artifact links `core`.
  The driver's current session-setup and exit-status helpers cross the unresolved utility boundary described below.
- [Compiler Support](../compiler-support/compiler-support.md) supplies the editor-protocol values converted by the WebAssembly binding.
  The WebAssembly target links `compiler-core`; its compiler dependency uses the other shared compilation services.
- Product headers under `include/` supply the compiler and reflection contracts, result codes, and interface smart pointers used by the drivers and bindings.
  The host C and C++ environment supplies native entry points, argument handling, standard I/O, and process APIs.
  Native compiler-driver targets also link `Threads::Threads`.
- Emscripten supplies the JavaScript interoperability and binding generator.
  The WebAssembly target explicitly links miniz and LZ4 alongside its static compiler dependencies.
  Compiler-generated capability and lookup objects and generated declaration inputs are required by that build; they retain their ownership in Compiler.
- The bootstrap build consumes the generated `prelude` objects and compiler lookup objects.
  The existing `slang-embed` generator and headers under `prelude/` prepare those prelude inputs.
  These object libraries are construction components, rather than independently shipped tool libraries.
- The `slangc` build requires the GLSL provider target and generated core-module cache when those targets exist.
  These are ordering and availability requirements; the compiler performs provider loading.
  The provider implementation remains in [Adapters](../adapters/adapters.md), and the cache remains in Compiler.
- The dispatcher requires the executables it launches at runtime, located beside the dispatcher.
  `compile` selects `slangc`, `interpret` and invocation without a command select `slangi`, and other external subcommands select `slang-<name>`.
  Launching the interpreter does not assign its implementation to this package.

## Decomposition

All immediate children are public build artifacts within the implementation hierarchy.
Their implementation remains in `old-source/`, and their own refinement determines their ordinary implementation children.

| Child                                                                   | Existing Products and Service                                                                                                                       |
| ----------------------------------------------------------------------- | --------------------------------------------------------------------------------------------------------------------------------------------------- |
| [Command-Line Compiler](command-line-compiler/command-line-compiler.md) | The `slangc` executable and its `slang-bootstrap` build variant adapt command-line requests to compilation and bundled-library preparation.         |
| [Command Dispatcher](command-dispatcher/command-dispatcher.md)          | The `slang-dispatcher` target produces the `slang` executable, which selects installed commands and forwards their arguments, I/O, and exit status. |
| [WebAssembly Bindings](webassembly-bindings/webassembly-bindings.md)    | The Emscripten `slang-wasm` target exposes compilation, reflection, and editor services to JavaScript.                                              |

The bootstrap variant shares the compiler-driver implementation, but has a different compiler-library dependency and build role.
Grouping the two independently built executables under one child records that shared implementation; the final artifact boundary and physical ownership of the reused driver code remain an ancestor question.
The WebAssembly target uses CMake's executable kind to emit `slang-wasm.js`, `slang-wasm.wasm`, and the generated `interface.d.ts` declarations.
Those files form one binding distribution; the target is not a conventional host executable, and its TypeScript declarations are not another library.
The existing output filenames and process, binding, and compiler-API contracts constrain migration.

## Existing Implementation Mapping

- **Command-Line Compiler:** all of `old-source/slangc/main.cpp` and its local `CMakeLists.txt`.
  The implementation initializes writers and a global session, adapts native arguments, configures the request, invokes compiler argument processing and compilation, and translates the result to a process exit status.
  `tools/CMakeLists.txt` also compiles or imports the bootstrap variant.
  The locally compiled variant sets `SLANG_BOOTSTRAP` to select its internal session setting.
  Compiler option semantics, built-in compilation, and generated library contents remain in Compiler.
- **Command Dispatcher:** all of `old-source/slang-dispatcher/main.cpp` and its local `CMakeLists.txt`.
  The implementation includes dispatch selection, local help/version output, executable lookup, argument quoting, platform process launching, and child-status reporting.
  The interpreter and other launched commands remain their own implementations outside this mapping.
- **WebAssembly Bindings:** all of `old-source/slang-wasm/slang-wasm.h`, `slang-wasm.cpp`, `slang-wasm-bindings.cpp`, and the local `CMakeLists.txt`.
  These files declare and implement JavaScript-facing compiler/reflection wrappers, descriptor validation and conversion, error/result conversion, editor-value conversion and calls, and Emscripten registrations including TypeScript descriptor declarations.
  The binding's language-server wrapper creates the compiler-owned editor implementation; it does not take ownership of the editor algorithms or shared protocol schemas.

The mappings above cover the three assigned legacy directories, not every helper their entry points call.
The relevant partial-file and shared-contract questions remain explicit below rather than assigning those files wholesale.

## Ancestor Decisions Before Migration

The [root's bootstrap cycle](../slang.md#unresolved-architectural-dependencies) remains unresolved.
The actual construction order distinguishes the non-embedded compiler variant, the bootstrap executable that uses it, and the final compiler that consumes generated built-in contents.
Collapsing both compiler variants into the proposed Compiler artifact still yields Compiler to Tools to Compiler.
Importing a host generator changes which executable is constructed locally, but does not settle the architectural ownership of that service.
Standard-module rules also invoke the bootstrap executable or `slangc` according to their build configuration; preparing those modules remains Compiler's responsibility.

`old-source/core/slang-test-tool-util.{h,cpp}` combines several concerns.
The session-prelude setup and deferred-core-module option query serve compiler-driver integration; path helpers combine host mechanisms with installation-layout policy; subtest selection serves the existing test runner.
Its exit-status declarations are also used by Foundation's no-exception signal handler, so assigning the entire utility to Tools would introduce a Foundation dependency on tools and compiler-session setup.
The ancestor must settle those partial assignments while preserving shared status values and current clients.
The rendering-backend vocabulary and coarse availability probes in `old-source/core/slang-render-api-util.{h,cpp}` likewise serve existing rendering and test applications outside this package's initial mapping, rather than compiler target selection.

The shared test-server schemas in `old-source/compiler-core/slang-test-server-protocol.{h,cpp}` describe test execution, shutdown, and captured results for `tools/slang-test/` and `tools/test-server/`.
The FileCheck interface in `tools/slang-test/filecheck.h` uses `TestResult` and `TestMessageType` from `tools/unit-test/` for its result and diagnostic callback, and is implemented by the LLVM adapter.
Those contracts need an ancestor assignment alongside their actual test applications before the affected siblings can complete their dependency analysis.
The current scope does not justify adding an unrelated shared tool library to contain them.

The root assigns the Windows `slang-proxy` compatibility library to its public [Compiler DLL Proxy](../compiler-dll-proxy/compiler-dll-proxy.md) artifact.
It forwards compiler exports through `slang.dll`; it is distinct from the `slang` dispatcher executable and is not assigned to Tools by sharing that base name.

Tools remains on the refinement worklist until these ancestor questions are resolved.
The children are service stubs, not an accepted acyclic artifact graph or a code-porting plan.
