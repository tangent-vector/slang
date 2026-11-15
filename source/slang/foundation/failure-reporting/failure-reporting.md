<!--
SPDX-FileCopyrightText: The Khronos Group, Inc.
SPDX-License-Identifier: CC-BY-4.0
-->

# Failure Reporting

Failure Reporting provides assertions and internal failure signals, including their exception or termination behavior.
It records the latest signal message for the reporting thread and supplies the exception types that callers use to distinguish invalid operations, aborted compilation, and internal errors.
Compiler diagnostics and the decision to recover from a caught failure remain with the caller.

This document refines an existing service concern, whose implementation remains under `old-source/core/`.
The [dependency cycle with basic data services](#unresolved-boundaries) prevents accepting this boundary for migration.

## Concepts

The [root document](../../slang.md#concepts) defines implementation modules, dependencies, and visibility.
[Foundation](../foundation.md#concepts) defines signals, strings, borrowed views, and tool status codes.

- An **assertion** checks a condition that the implementation expects to hold.
  Ordinary assertions check at runtime in `_DEBUG` builds and become optimizer assumptions otherwise; release assertions retain their runtime check in both builds.
  An assumption tells the C++ compiler that a condition holds and does not guarantee evaluation of its side effects.
- A **signal kind** identifies the reported failure: an unexpected condition, unimplemented operation, assertion failure, unreachable code, invalid operation, or aborted compilation.
  Aborted compilation can follow an already-reported diagnostic and does not by itself identify an internal compiler error.
- A **failure exception** is an `Exception` or one of its derived types, with a retained `String` message.
  The message is independent of the character pointer supplied by the reporting caller.
- The **last signal message** is the formatted message retained in thread-local storage by the signal handler.
  Its accessor returns borrowed characters; another signal on the same thread can replace their storage, and thread exit ends that storage's lifetime.

## Dependencies

- [Text](../text/text.md) supplies string construction, concatenation, and retained message storage for signals and exceptions.
  The signal handler records a string before choosing between exception and termination behavior, so this dependency exists with exceptions disabled too.
- The product headers supply exception-configuration and compiler/platform macros, including the breakpoint operation.
  Those existing contracts remain outside this implementation hierarchy.
- The host C and C++ environment supplies thread-local storage, environment lookup, bounded assertion-message formatting, exception transfer, and process termination.
  The Windows/MSVC branch additionally queries whether a debugger is attached and can invoke the system assertion facility.
- [Tool Status Codes](../tool-status-codes/tool-status-codes.md) supplies the shared `ToolReturnCode` vocabulary.
  The no-exception branch of `handleSignal` uses `ToolReturnCode::InternalCompilerError` without requiring the test-tool utility's conversion, comparison, compiler-session, or test-selection services.

## Service Areas

Failure Reporting remains a proposed leaf with no child modules.
Its closely connected services are:

- Assertion checks and optimizer assumptions, including the distinction between debug checks and release checks.
- Failure reporting with a signal kind, optional message, and access to the reporting thread's latest formatted message.
- Exception types and the configuration-dependent transfer of control after reporting a signal.

`handleAssert` attaches a file basename and line number before reporting an assertion signal.
The existing `SLANG_ASSERT` environment policy can suppress non-release assertions with either `release-assert-only` or `release-asserts-only`.
On Windows/MSVC, the policy also supports the system assertion and debugger-break branches; execution that continues past those operations still reports the signal.
These options affect assertion handling, rather than changing the contract that `handleSignal` does not return normally.

With exceptions enabled, invalid-operation and abort-compilation signals throw their corresponding exception types; other signal kinds throw `InternalError`.
With exceptions disabled, the handler attempts a breakpoint and then exits with the internal-compiler-error status if execution reaches the exit call.
The breakpoint can itself terminate execution.
The existing exception-try macro omits the `try` keyword in that configuration; callers separately exclude their catch clauses.

## Existing Implementation Mapping

The following files remain in `old-source/core/`:

- `slang-signal.h` and `slang-signal.cpp` provide signal kinds, reporting and assertion handlers, reporting macros, message formatting and storage, and the last-message accessor.
- `slang-exception.h` provides the base exception and its invalid-operation, internal-error, and abort-compilation subclasses, together with exception-try and exception-type-visibility macros.
- Only the assumption and assertion facilities in `slang-common.h` belong to this concern: `SLANG_ASSUME`, its fallback function, `SLANG_ASSERT`, and `SLANG_RELEASE_ASSERT`.
  The remaining scalar, portability, export, and object-interface facilities retain the assignments in [Foundation's mapping](../foundation.md#existing-implementation-mapping).

The `ToolReturnCode` enum in `slang-test-tool-util.h` belongs to [Tool Status Codes](../tool-status-codes/tool-status-codes.md).
The no-exception branch of `handleSignal` uses that shared declaration.
`ToolReturnCodeSpan`, the `TestToolUtil` converters and `isDone` predicate, and the utility's remaining services retain their pending [ancestor assignments](../foundation.md#existing-implementation-mapping); they are outside Failure Reporting.

The current build compiles these services into static `core` and separately into enabled `slang-rt` under its runtime export settings.
A future move must preserve both consumers and the [root's existing binary-interface contracts](../../slang.md#staged-migration-contracts).
Exception types also retain their defining identity, inheritance, message representation, and existing visibility annotations: typed catches cross dynamic-library boundaries, and non-Windows default type visibility supports exception-type matching across those boundaries.
The compiler's public last-internal-error accessor forwards this service's borrowed message pointer; that wrapper remains with Compiler.

## Unresolved Boundaries

The Text dependency is a behavioral cycle under Foundation's current ownership assignments.
For example, under the default assertion policy, an invalid debug index in `List::operator[]` calls `handleAssert`, which calls `handleSignal`.
The signal handler builds and retains a `String`, whose storage construction and append operations assert and whose retained representation uses native reference counting.
`RefObject::releaseReference` also asserts, so routing calls through a narrower signal header still leaves Failure Reporting depending on Text and Text depending on Failure Reporting and Memory, with Memory depending on Failure Reporting.
Disabling exceptions does not remove the string construction that starts this path.

Foundation must establish a coherent ownership boundary or behavior-preserving separation for those mutually dependent services before migration can be accepted.
The assertion/assumption portion of `slang-common.h` can be separated from scalar declarations, but that header change does not answer the behavioral ownership question.
No replacement failure mechanism or broader data-service grouping is selected by this refinement.
