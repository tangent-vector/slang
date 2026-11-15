<!--
SPDX-FileCopyrightText: The Khronos Group, Inc.
SPDX-License-Identifier: CC-BY-4.0
-->

# Tool Status Codes

Tool Status Codes supplies the shared outcome vocabulary used by Slang executable tools and their in-process and test-server invocations.

This is a public ordinary leaf of [Foundation](../foundation.md), with no child modules or separate build artifact.
Its existing declaration remains in `old-source/core/slang-test-tool-util.h`; this document describes ownership before header extraction.

## Concepts

[Foundation](../foundation.md#concepts) defines a tool status code and distinguishes it from a `SlangResult` and the operating system's representation of process termination.
The existing `Slang::ToolReturnCode` enum supplies that vocabulary.
The [root concepts](../../slang.md#concepts) define implementation visibility and dependencies; public visibility here does not add a supported product API.

## Dependencies

The assigned enum requires only the host C++ language's scoped-enumeration and built-in integer facilities.
Its declaration uses no other implementation service or product-header declaration.

The current mixed header includes `slang-std-writers.h` for the writer, session, string, and collection types used by excluded utility declarations.
That include does not make those services dependencies of the enum.
[Failure Reporting](../failure-reporting/failure-reporting.md), executable entry points, and their invocation adapters consume the vocabulary rather than supplying services it needs.

## Outcome Vocabulary

The leaf's single service is the existing set of named outcomes.
The following table records the current integer values because executable entry points and test-server records exchange those values:

| Enumerator              | Value | Outcome                                                                              |
| ----------------------- | ----- | ------------------------------------------------------------------------------------ |
| `InternalCompilerError` | `-2`  | An unexpected compiler failure, such as an assertion failure or exception.           |
| `CompilationFailed`     | `-1`  | A compilation failed; output may still have been produced.                           |
| `Success`               | `0`   | The tool ran normally.                                                               |
| `Failed`                | `1`   | The tool failed.                                                                     |
| `Ignored`               | `2`   | The invocation was ignored, for example because an optional feature was unavailable. |
| `FailedToRun`           | `3`   | The invocation could not be run.                                                     |

Clients decide how their operations produce and interpret these outcomes.
For example, `slangc`'s `main` converts the `SlangResult` returned by `innerMain` through `TestToolUtil::getReturnCode`.
The in-process test runner and test server use the same converter after calling a tool entry point.
The test server records the operation result and converted tool status in separate fields.
The converter, the compiler driver's normalization of compilation failures, and the test runner's decision to compare output or retry an invocation remain client policies.
In particular, a test expecting a compilation error can pass after the invoked compiler reports `CompilationFailed`.

Process exit values need their own interpretation.
The current Unix process implementation recovers a normal exit value through a signed eight-bit conversion and records termination by a signal separately; the Windows implementation converts the process exit value to `int32_t`.
Neither operation establishes that an arbitrary process result belongs to this enum.
Integer validation and test-result classification remain outside this leaf.

## Existing Implementation Mapping

- Only the `Slang::ToolReturnCode` enum, including its enumerators and documentation, in `old-source/core/slang-test-tool-util.h` belongs here.
  The header's export macro, `ToolReturnCodeSpan`, `TestToolUtil`, entry-point function type, and utility declarations remain outside this assignment.
- No definition in `old-source/core/slang-test-tool-util.cpp` belongs here.
  Its result and integer converters, path and installation-layout helpers, compiler-session prelude setup, deferred-core-module option query, and subtest selection retain the pending [ancestor assignments](../foundation.md#existing-implementation-mapping).
- `old-source/core/slang-signal.cpp` is a consumer owned by Failure Reporting.
  With exceptions disabled, `handleSignal` uses `InternalCompilerError` for the exit call reached if execution continues past its breakpoint.
- `old-source/slangc/main.cpp`, the invocation and comparison code in `tools/slang-test/slang-test-main.cpp`, and the execution code in `tools/test-server/test-server-main.cpp` remain with their clients.
  The integer fields and serialization in `old-source/compiler-core/slang-test-server-protocol.{h,cpp}` describe the test-server protocol, not another part of the vocabulary service.
  The current return-code unit coverage in `tools/slang-unit-test/unit-test-test-tool-util.cpp` primarily checks client conversion policy and remains with that utility's tests.

## Preservation Constraints

Any later extraction preserves the canonical `Slang::ToolReturnCode` identity, scoped-enum representation with its default `int` underlying type, enumerator order, names, and integer values.
A module namespace may re-export the same enum; a second enum with matching values would not preserve the type's identity.
Existing function signatures, result mappings, process decoding, protocol fields, comparison behavior, and retry decisions also remain unchanged.

The enum is declared in a header and has no separate implementation object.
The current `core` target compiles the mixed utility implementation, and enabled `slang-rt` separately compiles the legacy core directory under runtime export settings.
A future header extraction must preserve both compilation consumers and the [root's staged binary-interface contracts](../../slang.md#staged-migration-contracts).
It does not introduce another library or assign the mixed utility implementation to this leaf.

The independent vocabulary declaration does not resolve Foundation's [Failure Reporting, Text, and Memory cycle](../foundation.md#failure-reporting-and-basic-data-services) or the [remaining tool-integration ownership questions](../../tools/tools.md#ancestor-decisions-before-migration).
This refinement does not authorize a code port.
