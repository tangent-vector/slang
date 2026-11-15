<!--
SPDX-FileCopyrightText: The Khronos Group, Inc.
SPDX-License-Identifier: CC-BY-4.0
-->

# C Runtime Compatibility

C Runtime Compatibility lets implementation clients use the existing C-runtime spellings for bounded memory copying, character-buffer operations, formatting, and stdio access across supported host environments.
It supplies fallback definitions selected by platform guards and build-time availability checks.

The implementation remains in `old-source/core/slang-secure-crt.h`.
The leaf boundary proposed here groups those compatibility definitions; it does not prescribe a common replacement for the behavior of the selected native runtime and fallback implementations.

## Concepts

The [root document](../../../slang.md#concepts) defines implementation modules, dependencies, and visibility.
[Base Types](../base-types.md#concepts) defines character code units.

- A **fallback** is an inline definition supplied when the header's platform guard permits compatibility definitions and the corresponding `HAVE_*` macro is absent.
  Selection is per function, so one environment can use a mixture of native functions and fallbacks.
- A **buffer capacity** states how much destination storage an operation may use.
  Memory-copy and stdio-read capacities are in bytes; character-buffer capacities are in elements of `char` or `wchar_t`, including space for a terminator where the operation writes one.
  A read's element size and element count remain separate from its buffer capacity.
- **Requested truncation** is the counted-copy mode selected by `_TRUNCATE`.
  The fallback copies as much as fits before the terminator and returns `STRUNCATE` if characters remain.
  An ordinary finite copy count can also stop before the end of the input and return success when that count fits within the destination capacity.

## Dependencies

- The host C and C++ environment supplies `size_t`, `wchar_t`, variadic arguments, `FILE`, and `errno` with its error constants.
  The fallbacks call `memcpy`, `memset`, `fopen`, `fread`, `vsnprintf`, and `vswprintf` directly; the case-insensitive comparison alias selects `strcasecmp`.
  Those calls provide the underlying copying, formatting, file access, and comparison behavior.
- Repository build infrastructure in `cmake/CheckSecureCRT.cmake` checks whether each adapted function can be called on MinGW and Cygwin.
  `cmake/SlangTarget.cmake` passes the successful `HAVE_*` definitions privately to each target created through `slang_add_target`.
  These are construction inputs, not dependencies on another Foundation service.
- Compiler-predefined platform macros select the header's outer branch, and `__GNUC__` selects the printf-format checking attribute on `sprintf_s`.
  The header uses these macros directly rather than depending on the product headers' `SLANG_*` platform vocabulary.

The implementation has no dependency on another implementation module.
In particular, accepting character buffers does not require [Text](../../text/text.md)'s string ownership, encoding conversion, or reusable formatting services.
Text's `String::append(float, const char*)` and `String::append(double, const char*)` call `sprintf_s` and `strnlen_s` on their own storage; the dependency runs from Text to these lower operations.
Likewise, [Host Services](../../host-services/host-services.md)'s `FileStream` uses `fopen_s` and `fread_s`, while the fallbacks call the host CRT directly without using Slang paths, streams, or file-access policy.

## Decomposition

This is a public leaf within Base Types, with no proposed child modules.
Its service areas share the same role of adapting existing CRT spellings and operate on host types or caller-provided storage:

- **Memory copying:** `memcpy_s` copies a byte count subject to the destination byte capacity.
- **Character buffers:** `strnlen_s` and `wcsnlen_s` find bounded lengths; `strcpy_s` and `wcscpy_s` copy terminated input; `strncpy_s` and `wcsncpy_s` additionally accept a copy count or requested truncation.
  The conditional `_stricmp` alias provides the existing case-insensitive comparison spelling.
- **Buffer formatting:** `sprintf_s` and `swprintf_s` format variadic arguments into bounded narrow or wide buffers through the host formatter.
- **Stdio access:** `fopen_s` reports a host file-open result through a `FILE*` output and an integer status; `fread_s` checks the destination byte capacity before reading elements from a supplied `FILE*`.

The file operations belong here because they adapt the low-level CRT calls and their argument and result conventions.
Slang path handling, stream lifetime, access and sharing policy, and translation to Slang result codes remain with clients such as Host Services and Output Writers.

## Existing Implementation Mapping

The whole of `old-source/core/slang-secure-crt.h` belongs to this leaf: all eleven inline fallbacks, the `_TRUNCATE` and `STRUNCATE` macro defaults, the conditional `_stricmp` alias, and their guards and format-checking annotation.
There is no out-of-line implementation or separate build artifact.
The CMake probe and target-definition plumbing remain repository build infrastructure rather than additional files assigned to this leaf.

The header adds definitions only when `_WIN32` is absent, `__MINGW32__` is present, or `__CYGWIN__` is present.
Within that outer branch, `SLANG_CORE_SECURE_CRT_H` prevents repeated inclusion and each function's `HAVE_*` guard suppresses its fallback when the macro is defined.
The `_TRUNCATE` and `STRUNCATE` defaults are supplied only when the corresponding macro is absent.
The `_stricmp` alias is supplied only when `_stricmp` is not a macro and `__MINGW32__` is absent.
The CMake probes run only for MinGW and Cygwin and establish call availability, not behavioral equivalence with a fallback.

Clients include the header directly or through legacy string, character-encoding, and I/O headers.
That includes code compiled into static `core`, the separate enabled `slang-rt` compilation, compiler code, and repository tools.
The header is implementation infrastructure rather than an installed product header, and its functions have global names and no Slang export annotations.
Any later extraction must preserve those names, signatures, inline definitions, macro spellings, guard ordering, native-availability selection, and both existing Foundation compilation consumers under the [root's staged contracts](../../../slang.md#staged-migration-contracts).

## Preservation Boundaries

The current wrappers have operation-specific validation, `errno`, buffer-clearing, termination, and return behavior.
For example, the `memcpy_s` fallback returns success for a zero count before checking either pointer, whereas the counted string-copy fallbacks check their input pointers before handling a zero count.
After a host formatting call fails or reports insufficient space, the formatting fallbacks clear the first buffer element and return `-1` without invoking an MSVC invalid-parameter handler.
The `fread_s` fallback returns an element count, and its capacity check uses division by the element size after the zero-size and zero-count exits.
The existing implementation is the behavior-preservation baseline, and `tools/slang-unit-test/unit-test-secure-crt.cpp` provides existing coverage of the wrappers.
Their names do not authorize normalizing their behavior to a preferred CRT specification.

No additional dependency or ancestor ownership decision is required by this leaf's observed operations.
Foundation's [unresolved service cycles](../../foundation.md#unresolved-boundaries) and the root's [artifact questions](../../../slang.md#unresolved-architectural-dependencies) remain separate architectural constraints; this leaf's refinement does not resolve them or authorize a code port.
