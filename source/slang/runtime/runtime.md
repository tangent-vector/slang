<!--
SPDX-FileCopyrightText: The Khronos Group, Inc.
SPDX-License-Identifier: CC-BY-4.0
-->

# Runtime

Runtime provides operations called by generated host programs to report a fatal runtime failure and find functions in shared libraries.
It adapts Foundation's text, object-lifetime, and host-library services to calls emitted by the compiler.
Runtime does not compile programs or choose how Slang calls are marshalled into native calls.

This is the public build artifact assigned by [Slang Implementation](../slang.md#decomposition), currently built as `slang-rt`.
Its small runtime-specific interface is a leaf; reusable facilities compiled into the same library retain their ownership in Foundation.
No implementation has moved into this directory.

## Concepts

The [root concepts](../slang.md#concepts) define build artifacts and dependencies, and the [Foundation concepts](../foundation/foundation.md#concepts) define text storage and reference-counted ownership.
Runtime adds the following distinctions:

- A **runtime library token** is the opaque value returned to generated code for a loaded library.
  The existing implementation returns a borrowed `ISlangSharedLibrary*` through `void*`, backed by an entry in its retaining cache; the token is not the operating system's library handle.
- An **imported function** is a native function that generated code locates by library and symbol name before calling through its address.
  The compiler determines the native signature and argument conversion; Runtime performs library and symbol lookup.
- A **fatal runtime failure** is an error for which Runtime reports a message and aborts the host process.
  This path is distinct from Foundation's [signals](../foundation/foundation.md#concepts) and ordinary operations that return a failure result.

## Dependencies

- [Foundation](../foundation/foundation.md) supplies strings, native and interface ownership, collections and hashing for the library cache, and Host Services for shared-library loading and symbol lookup.
  The runtime library currently compiles the Foundation implementations again rather than linking the static `core` library.
- Product headers under `include/` supply the shared-library interface, interface smart pointer, calling-convention declarations, and platform/export macros.
  Those contracts are inherited through the root; their use does not require compiler services.
- The host C and C++ environment supplies standard-error output and process termination.
  The Windows failure path additionally displays a native message box.

The current runtime build links miniz, LZ4, `Threads::Threads`, and the platform dynamic-loader libraries, and exposes unordered_dense as a public build dependency.
Foundation's compressed-data and host services account for those link dependencies.
The runtime's build settings are separate from `core`'s, including its export configuration.

## Service Areas

Runtime has two public service areas and no child modules:

- **Fatal failure reporting:** The abort entry point writes the supplied message to standard error, displays a Windows message box on Windows, and aborts the process.
  A library-load or symbol-lookup failure uses this same terminating path rather than returning a recoverable error.
- **Imported-library access:** The loading entry point obtains a shared-library interface and retains it in a cache keyed by the requested path.
  The lookup entry point finds an exact symbol name, then tries the existing stdcall-decorated spelling if needed.
  An absent library token requests lookup through the current process's library representation.

The cache owns references to its library interfaces until its entries are destroyed.
The returned library tokens and function addresses do not transfer those references to callers.
There is no runtime release entry point, and the existing cache operations provide no synchronization for concurrent requests.
The cache belongs with this small lookup service; it does not introduce a general caching service or another module.

## Compiler and ABI Contracts

`generateDllImportFuncs` in `old-source/slang/slang-ir-dll-import.cpp` generates calls to `_slang_rt_load_dll` and `_slang_rt_load_dll_func` when implementing DLL-imported functions for C++ output.
It also supplies the byte count used by the decorated-name lookup and generates the native-call wrapper.
Those compiler transformations remain in Compiler.
The host prelude in `prelude/slang-cpp-host-prelude.h` includes the runtime declarations, and `old-source/slang/slang-emit.cpp` selects that prelude for host-style targets.
When arranging downstream compilation for such a target without pass-through, `old-source/slang/slang-code-gen.cpp` adds the `slang-rt` library input.
Generating these dependencies does not make Runtime depend on Compiler.

The three entry points use C linkage and `SLANG_MCALL`, but their signatures pass `Slang::String` by value.
The interface therefore still depends on compatible C++ type identity, layout, ownership operations, and calling conventions; C linkage alone does not make it a C data interface.
Foundation's string and native-ownership declarations also carry `SLANG_RT_API` export annotations.
The [root's staged migration contract](../slang.md#staged-migration-contracts) preserves these canonical types in their defining namespaces and keeps both compilation consumers with their existing export settings.
A module namespace may re-export those same declarations.
The runtime entry-point names and signatures, calling conventions, and artifact names remain unchanged during migration.

## Existing Implementation Mapping

- `old-source/slang-rt/slang-rt.h` declares the three runtime entry points and the associated runtime/prelude export macros.
- `old-source/slang-rt/slang-rt.cpp` implements fatal reporting, the retaining library cache, library loading, and symbol lookup.
- `old-source/slang-rt/CMakeLists.txt` assembles the optional static or shared runtime target, includes the core implementations, configures exports and link dependencies, and specifies installation and version properties.
- The implementations under `old-source/core/` retain the service assignments in [Foundation's mapping](../foundation/foundation.md#existing-implementation-mapping).
  Their separate compilation into Runtime is a build-consumer requirement, not a second semantic owner for those services.

The prelude and compiler files described above are clients and integration inputs, rather than implementations assigned to Runtime.
The existing scalar, vector, resource, and other generated-program support in `prelude/` also remains outside this module's assigned implementation scope.

## Ancestor Decision

The final artifact arrangement must represent Foundation's service ownership together with its separate compilation under runtime export settings.
Simply replacing that compilation with a link to `core`, or treating every included core service as a second Runtime-owned implementation, would select a different arrangement without establishing ABI and build equivalence.
The root's staged migration contract settles those preservation constraints without selecting a general model for source reuse across artifacts.
Runtime remains pending for that [general artifact decision](../slang.md#unresolved-architectural-dependencies) and for verification that a later port preserves the shared types and both build consumers.
This document does not establish a completed artifact boundary or a new export policy.
