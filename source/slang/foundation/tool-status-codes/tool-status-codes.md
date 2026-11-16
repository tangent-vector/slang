<!--
SPDX-FileCopyrightText: The Khronos Group, Inc.
SPDX-License-Identifier: CC-BY-4.0
-->

# Tool Status Codes

Tool Status Codes supplies the shared vocabulary for outcomes reported by Slang executable tools and by their in-process and test-server callers.

This is a public ordinary leaf of [Foundation](../foundation.md), with no child modules or separate build artifact.
Its primary header is `tool-status-codes.h` in this directory.
The canonical `Slang::ToolReturnCode` definition is exposed as the same enum by the leaf namespace, Foundation's aggregate header, and the retained legacy compatibility header.

## Concepts

[Foundation](../foundation.md#concepts) defines a tool status code and distinguishes it from a `SlangResult` and the operating system's representation of process termination.
The `Slang::ToolReturnCode` enum supplies that vocabulary.
The [root concepts](../../slang.md#concepts) define implementation visibility and dependencies; public visibility here does not add a supported product API.

## Dependencies

The assigned enum requires only the host C++ language's scoped-enumeration and built-in integer facilities.
Its declaration uses no other implementation service or product-header declaration.

The primary header has no includes.
The retained mixed header `old-source/core/slang-test-tool-util.h` includes the primary after `slang-std-writers.h`, which supplies the writer, session, string, and collection types used by excluded utility declarations.
Those utility prerequisites are not dependencies of the enum.
[Failure Reporting](../failure-reporting/failure-reporting.md), executable entry points, and their invocation adapters consume the vocabulary rather than supplying services it needs.

## Outcome Vocabulary

The leaf's single service is the existing set of named outcomes.
The following table records the current integer values because executable entry points and test-server records exchange those values:

| Enumerator              | Value | Outcome                                                                                                                            |
| ----------------------- | ----- | ---------------------------------------------------------------------------------------------------------------------------------- |
| `InternalCompilerError` | `-2`  | The client reported an internal compiler error; an ordinary diagnosed abort can also produce this status with exceptions disabled. |
| `CompilationFailed`     | `-1`  | The client reported a compilation failure; output may still have been produced.                                                    |
| `Success`               | `0`   | The client reported success for the requested operation.                                                                           |
| `Failed`                | `1`   | The client reported failure for the requested operation.                                                                           |
| `Ignored`               | `2`   | The client reported an ignored invocation, for example because an optional feature was unavailable.                                |
| `FailedToRun`           | `3`   | The caller could not run the tool or obtain its result.                                                                            |

Clients decide how their operations produce and interpret these outcomes.
For example, `slangc`'s `main` converts the `SlangResult` returned by `innerMain` through `TestToolUtil::getReturnCode`.
The in-process test runner and test server use the same converter after calling a tool entry point.
The test server records the operation result and converted tool status in separate fields.
The converter, the compiler driver's normalization of compilation failures, and the test runner's decision to compare output or retry an invocation remain client policies.
In particular, a test expecting a compilation error can pass after the invoked compiler reports `CompilationFailed`.

The same diagnosed input error can lead to different tool statuses depending on whether exceptions are enabled.
With exceptions disabled, `handleSignal` uses `InternalCompilerError` for every signal kind if execution reaches its exit call after the breakpoint.
That includes `SignalType::AbortCompilation` raised after an ordinary fatal input diagnostic.
For example, the compiler's `SemanticsVisitor::ensureDecl` can diagnose a cyclic reference; the diagnostic sink reports that fatal error and signals an abort.
With exceptions enabled, `handleSignal` throws `AbortCompilationException` for an abort instead.
In the compiler's exception-catching path, `EndToEndCompileRequest::compile` keeps `SLANG_FAIL` for that exception when an error was already recorded.
The `slangc` driver's `_compile` normalizes that result to `SLANG_E_INTERNAL_FAIL`, which `TestToolUtil::getReturnCode` maps to `CompilationFailed`.
Clients cannot infer from `InternalCompilerError` alone that a compiler bug caused the failure.

Clients can produce a status without running the tool or receiving its result.
For example, `spawnAndWait` returns `Success` when it collects test requirements without executing the tool.
The same function returns `FailedToRun` if an invocation adapter fails to run the tool or obtain its result.
A missing or unreadable test-server response can produce that status after the request was sent, so `FailedToRun` does not establish whether execution started or completed.

Process exit values need their own interpretation; the enum's negative integers are not universally portable native process exit values.
The current Unix process implementation recovers a normal exit value through a signed eight-bit conversion and records termination by a signal separately; the Windows implementation converts the process exit value to `int32_t`.
Neither operation establishes that an arbitrary process result belongs to this enum.
Integer validation and test-result classification remain outside this leaf.

## Existing Implementation Mapping

- `tool-status-codes.h` in this directory defines only the `Slang::ToolReturnCode` enum, including its enumerators and client documentation.
  The `slang::foundation::tool_status_codes` namespace and `slang::foundation` aggregate explicitly reexport that same enum.
- `old-source/core/slang-test-tool-util.h` includes the primary header for existing clients.
  That compatibility header also defines the export macro, `ToolReturnCodeSpan`, `TestToolUtil`, entry-point function type, and utility declarations outside this leaf.
- No definition in `old-source/core/slang-test-tool-util.cpp` belongs here.
  Its result and integer converters, path and installation-layout helpers, compiler-session prelude setup, deferred-core-module option query, and subtest selection retain the pending [ancestor assignments](../foundation.md#existing-implementation-mapping).
- `old-source/core/slang-signal.cpp` is a consumer owned by Failure Reporting.
  With exceptions disabled, `handleSignal` uses `InternalCompilerError` for the exit call reached if execution continues past its breakpoint.
  The handler uses that value for every signal kind in that configuration, including an ordinary diagnosed compilation abort.
- `old-source/slangc/main.cpp`, the invocation and comparison code in `tools/slang-test/slang-test-main.cpp`, and the execution code in `tools/test-server/test-server-main.cpp` remain with their clients.
  The integer fields and serialization in `old-source/compiler-core/slang-test-server-protocol.{h,cpp}` describe the test-server protocol, not another part of the vocabulary service.
  The current return-code unit coverage in `tools/slang-unit-test/unit-test-test-tool-util.cpp` primarily checks client conversion policy and remains with that utility's tests.

## Preservation Constraints

The primary header preserves the canonical `Slang::ToolReturnCode` identity, scoped-enum representation with its default `int` underlying type, enumerator order, names, integer values, and explicit-versus-implicit initializer choices.
The leaf and parent `using` declarations name the same enum; a second enum with matching values would not preserve the type's identity.
Existing function signatures, result mappings, process decoding, protocol fields, comparison behavior, and retry decisions also remain unchanged.

The enum is declared in a header and has no separate implementation object.
The leaf's `CMakeLists.txt` registers the primary header on the existing `core` and enabled `slang-rt` targets without adding a translation unit or target.
That registration describes header ownership rather than an enum implementation object.
Both targets compile `old-source/core/slang-test-tool-util.cpp`, whose converter definitions use the enum through the retained compatibility header.
The runtime compiles that utility under its own export settings, as required by the [root's staged binary-interface contracts](../../slang.md#staged-migration-contracts).

The independent vocabulary declaration does not resolve Foundation's [Failure Reporting, Text, and Memory cycle](../foundation.md#failure-reporting-and-basic-data-services) or the [remaining tool-integration ownership questions](../../tools/tools.md#ancestor-decisions-before-migration).
