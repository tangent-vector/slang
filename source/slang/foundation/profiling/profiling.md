<!--
SPDX-FileCopyrightText: The Khronos Group, Inc.
SPDX-License-Identifier: CC-BY-4.0
-->

# Profiling

Profiling counts entries into instrumented functions or named sections, accumulates their measured durations, and exposes the collected results as text or a reference-counted product object.
Clients choose which work to measure and when to collect, report, or reset the results.

The implementation remains in `old-source/core/slang-performance-profiler.{h,cpp}`.
This document refines [Foundation's assignment](../foundation.md#existing-implementation-mapping) for the existing measurement mechanism.

## Concepts

[Foundation](../foundation.md#concepts) defines borrowed views and intrusive references.
[Object Interfaces](../object-interfaces/object-interfaces.md#concepts) defines product interface references, and [Native Ownership](../memory/memory.md#native-object-lifetimes) describes the native reference-counting mechanism used by the result object.

- A **measured interval** starts when a client enters a function or named section and ends when the client returns the resulting context to the collector.
  The context contains the borrowed name pointer and a starting clock reading.
- A **collector** records the number of intervals started and the sum of completed interval durations for each name-pointer key.
  The existing `getProfiler` supplies a separate collector for each thread.
- A **profile snapshot** copies the collector's current entries into a separate `ISlangProfiler` implementation.
  It owns the copied names, counts, and durations, so later collection or reset does not change the snapshot.

A name-pointer key identifies an entry by its address, rather than by the characters at that address.
Equal spellings at different addresses can therefore produce distinct entries with the same printed name.
The collector borrows each null-terminated name until the entry is removed; the existing instrumentation macros use `__func__` or string literals.

## Dependencies

- [Associative Collections](../collections/associative-collections/associative-collections.md) supplies the ordered dictionary that associates borrowed name pointers with accumulated measurements.
  [Sequence Storage](../collections/sequence-storage/sequence-storage.md) supplies the owned list of copied snapshot entries.
- [Hashing](../hashing/hashing.md) supplies the address hash used for the dictionary's pointer keys.
  The ordered dictionary compares those keys with pointer equality.
- [Strings](../text/strings/strings.md) supplies the `StringBuilder` destination to which the text renderer appends its report.
- [Native Ownership](../memory/native-ownership/native-ownership.md) supplies the `RefObject` base and retention and release operations for the snapshot object.
  The existing native-reference macros in `include/slang-com-helper.h` connect those operations to the product reference protocol directly; the implementation does not use the Object Interfaces module's concrete reference-counting bases.
- The product headers supply `ISlangProfiler`, its UUID, interface and result conventions, and the native-reference helper macros.
  Those public headers remain external contracts outside this implementation hierarchy.
- The host C++ environment supplies `std::chrono::high_resolution_clock`, duration arithmetic and conversion, thread-local storage, and object allocation.
  The C runtime supplies name copying and the fixed-buffer formatting used by the text report.

Profiling does not use Host Services' process timer.
The selected clock's name does not establish a steady clock, CPU-time measurement, or a particular effective resolution.
The containers and native reference operations also retain their providers' assertion and failure-reporting requirements.

## Service Areas

Profiling is a public leaf within Foundation, with no child modules.
Collection, scoped instrumentation, and the two result forms share one small measurement representation.
We keep them together so that the result adapters read the same counts and accumulated durations without introducing another owner for the collector's entries.
Compiler instrumentation and report policy remain with the clients.

### Collection and Scoped Instrumentation

Entering an interval increments its entry's count immediately and records the starting clock reading in the returned context.
Exiting adds the difference between the current and starting readings to that entry's duration.
The collector does not maintain a call tree or subtract time spent in nested intervals.
For example, a compiler scope named `SemanticChecking` contains a call to the separately instrumented `checkAllTranslationUnits`; the time measured inside that call contributes to both entries.
Adding the parent and child durations counts the child's interval twice.

The scoped context pairs entry at construction with exit at destruction.
`SLANG_PROFILE` supplies the enclosing function's `__func__` name, and `SLANG_PROFILE_SECTION` supplies a stringized section name.
Both the entry and exit performed by that helper use the calling thread's collector.
The interval must finish on the thread where it began, and its corresponding entry must remain present until exit.
Resetting or disposing of the collector during an active interval is outside that usage contract.

`clear` removes the accumulated entries, and `dispose` replaces the collector's dictionary with an empty dictionary.
Neither operation destroys the thread-local collector object.
The implementation provides no collector aggregation or synchronization for concurrent access to one collector.
Its thread-local selection does not make an explicitly shared collector pointer safe to mutate concurrently.

### Text Reports and Product Snapshots

The text renderer appends entries in their first-insertion order.
It prints the name, invocation count, and total duration in milliseconds; for counts greater than fifty it also prints total duration divided by count.
It converts accumulated durations to whole microseconds before producing the fractional-millisecond text.
Each text entry is formatted through a 512-byte buffer and can be truncated.
Reporting does not clear the collector or complete active intervals.

`SlangProfiler` copies entries in the same order into a reference-counted snapshot.
The adapter requires the built-in collector returned by `getProfiler`; its constructor accesses that concrete implementation's dictionary.
Each copied name holds at most 255 bytes plus a null terminator.
The product accessors expose the entry count and copied names, the duration converted to whole milliseconds and returned as `long`, and the invocation count converted to `uint32_t`.
An out-of-range entry index returns a null name or zero for the numeric accessors.
The snapshot's name pointers refer to its own entry storage and remain valid for that object's lifetime.
Its interface-lookup hook recognizes the profiler UUID, and the product helper macros implement reference retention and release through `RefObject`.

A snapshot can include an interval that has begun but has not ended: its entry's invocation count already includes that interval, but its duration has not yet been added.
The compiler's `getCompileTimeProfile` constructs the snapshot before optionally clearing the calling thread's collector.
The snapshot therefore describes that collector's accumulated work, which need not coincide with one compilation request and does not include other threads' collectors.

## Existing Implementation Mapping

| Existing Implementation                                 | Profiling Responsibility                                                                                                                                      |
| ------------------------------------------------------- | ------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| All of `old-source/core/slang-performance-profiler.h`   | Measurement records, collector interface, scoped context and instrumentation macros, and the product snapshot declaration and storage.                        |
| All of `old-source/core/slang-performance-profiler.cpp` | Thread-local collector, pointer-keyed accumulation, text rendering, clear and disposal operations, snapshot copying, interface lookup, and product accessors. |

The pair contains one coherent service; no partial-file split or additional child is proposed.

Compiler clients remain in `old-source/slang/`.
The instrumentation sites select function and section boundaries; `slang-pass-wrapper.h` conditionally enables pass intervals according to compiler options.
`EndToEndCompileRequest::compile` clears accumulated results when `ReportDownstreamTime` is enabled and delivers the formatted report through diagnostics when `ReportPerfBenchmark` is enabled.
`EndToEndCompileRequest::getCompileTimeProfile` validates its output argument, creates a snapshot, and applies the caller's optional-clear policy.
`slang-api.cpp` forwards the public request operation and calls collector disposal during `slang_shutdown`.
The compiler retains ownership of those request, option, diagnostic, and shutdown services.

The current build selects this implementation through the recursive core source glob for static `core` and recompiles it into enabled `slang-rt` under runtime export settings.
`old-source/core/CMakeLists.txt`, `old-source/slang-rt/CMakeLists.txt`, and the source-selection helpers in `cmake/` retain their build-integration responsibility.
The [root's preservation contracts](../../slang.md#staged-migration-contracts) require maintaining both consumers and auditing existing binary-interface identities before migration.
This leaf assignment does not resolve [Foundation's low-level service cycles](../foundation.md#failure-reporting-and-basic-data-services) or [Collections' compiler-specific integer-set boundary](../collections/collections.md#unresolved-boundaries), and does not establish readiness to port their affected dependencies.
