<!--
SPDX-FileCopyrightText: The Khronos Group, Inc.
SPDX-License-Identifier: CC-BY-4.0
-->

# Random Numbers

Random Numbers provides seeded pseudorandom sequences, operations that select scalar values from those sequences, and byte-buffer filling.
The main generator interface lets clients restart a sequence from a seed or copy a generator's current state to repeat later draws independently.
The module also owns a separate compact generator with its own scalar API and sequence.

This document refines the public child assigned by [Foundation](../foundation.md#candidate-decomposition).
It describes the existing implementation to be organized; no implementation has moved into this directory.

## Concepts

- A **pseudorandom sequence** is a sequence of values computed deterministically from an initial seed and the generator's subsequent state changes.
  Calling the values random does not establish a statistical or cryptographic guarantee.
- A **generator** is a mutable object that produces a pseudorandom sequence.
  `RandomGenerator` is the common interface; `Mt19937RandomGenerator` is its existing concrete implementation, also named by the `DefaultRandomGenerator` alias.
  `Random` is a separate concrete generator and does not implement that interface.
- A **seed** is the input used to initialize or reset a particular generator implementation.
- The **generator state** is the information needed to continue producing values at the current position.
  Copying that state permits independent continuation of the sequence.
- A **draw** consumes values from a generator's sequence and advances its state.
  Convenience operations can consume different numbers of values, including none in constant-result cases.

The [root concepts](../../slang.md#concepts) define modules, dependencies, and public visibility.
The [Foundation concepts](../foundation.md#concepts) define the intrusive references used to retain heap-allocated generators.

## Dependencies

The `RandomGenerator` family uses the following sibling services within Foundation:

- [Memory](../memory/memory.md) supplies native object ownership through `RefObject` and `RefPtr`, allowing clients to retain factory-created and cloned generators.
- [Failure Reporting](../failure-reporting/failure-reporting.md) supplies assertions for numeric bounds and byte-fill invariants.

The C++ environment and product-header contracts inherited through [Foundation](../foundation.md#dependencies) supply integer types, byte counts, allocation, byte copying, and compatibility macros.
The wider include graph through native ownership does not establish additional direct service dependencies.
`Random` uses ordinary C++ arithmetic and has no dependency on the `Math` services that share its existing header.
Clients may supply seeds explicitly; `Mt19937RandomGenerator` also has a default constructor that uses the fixed seed `21452`.
Neither generator selects seeds from host entropy, clocks, or files.

## Service Areas

Random Numbers is a leaf module with three public service areas:

- **Generator lifetime and state:** `RandomGenerator` provides a shared interface for seeded creation, reset, and cloning, with native reference-counted ownership for heap instances.
  Its concrete `Mt19937RandomGenerator` type and `DefaultRandomGenerator` alias also support direct construction and value copying.
  Existing clients construct the concrete generator on the stack, so the type remains public.
- **Scalar selection and buffers:** The common family provides scalar and bulk integer generation, booleans and floats, bounded integer selection, and filling of caller-provided byte buffers.
  These operations belong together because their conversion and buffering rules determine how the same generator state advances.
- **Compact scalar generation:** `Random` provides a separate value type with seeded construction, copying, integer and floating-point generation, and bounded scalar selection.
  Its algorithm and numeric contracts remain distinct from those of the common family.

The concrete state-update operation and the byte-fragment routine remain with their respective implementations inside this leaf.
Neither operation provides an independent service requiring a private child.
Each generator has mutable state without synchronization of draws or resets; clients coordinate shared access or use independent generators.

## Clients and Reproducibility

Clients control the order of operations that consume each sequence.
For example, `obfuscateModuleLocs` in `old-source/slang/slang-ir-obfuscate-loc.cpp` hashes module location information to choose a seed, fills bytes for an obfuscated filename, then selects indices from a shrinking bag.
The compiler pass owns the hash inputs, location ordering, and mapping policy; Random Numbers owns the draws and their state consumption.

The render-test input-layout parser uses a caller-supplied generator to construct signed, unsigned, and floating-point buffer values.
Free-list, memory-arena, RIFF, process, and persistent-cache unit tests use the same services for repeatable choices and test data.
Those consumers retain their format, buffer, and test-policy responsibilities.

Reproducing output requires the same generator implementation, initial state, and sequence of operations within a compatible execution environment.
Byte output additionally depends on request sizes and boundaries, destination alignment modulo four, and host integer representation.
Changing how a buffer fill is split can also change subsequent draws.
The existing arithmetic, range behavior, and C++ evaluation choices constrain compatibility; a seed alone is not a cross-platform output specification.
The two generators are not interchangeable implementations of one seeded sequence, and neither API defines a state serialization format.

## Existing Implementation Mapping

The following assignments cover the two random-generator files completely and only the random-number portion of the math header:

| Existing File                                | Assigned Responsibility                                                                                                                                                                |
| -------------------------------------------- | -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| `old-source/core/slang-random-generator.h`   | The `RandomGenerator` interface and inline operations, the complete public `Mt19937RandomGenerator` declaration including its protected state, and the `DefaultRandomGenerator` alias. |
| `old-source/core/slang-random-generator.cpp` | Common scalar and bounded-value operations, byte filling and its file-local `_nextData` fragment routine, the default factory, and the complete concrete generator implementation.     |
| `old-source/core/slang-math.h`               | Only the complete `Random` class and its inline implementation. The `Math` class, arithmetic utilities, and floating-point conversions remain outside this module.                     |

Callers remain with their owning compiler or tool concerns.
The random-generator implementation currently compiles into both `core` and the separately compiled `slang-rt` consumer described by [Foundation](../foundation.md#build-consumers-and-ancestor-decisions).
Preserving both build consumers is an integration requirement; this leaf does not introduce a new library or settle Foundation's unresolved dependency cycles.
