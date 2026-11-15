<!--
SPDX-FileCopyrightText: The Khronos Group, Inc.
SPDX-License-Identifier: CC-BY-4.0
-->

# Callables

Callables provides stored invocation through a common signature for functions, member-function/object pairs, and client-supplied callables.
It provides wrappers that retain the stored invocation state, including an optional void-returning action that does nothing when empty.

The implementation remains in `old-source/core/slang-func-ptr.h`.
This document refines [Foundation's assignment](../foundation.md#existing-implementation-mapping) as a proposed leaf; it does not describe a completed header extraction or code port.

## Concepts

The [root concepts](../../slang.md#concepts) define modules, dependencies, and implementation visibility.
[Foundation](../foundation.md#concepts) defines a callable wrapper and an intrusive reference.
[Memory's native object lifetimes](../memory/memory.md#native-object-lifetimes) explain the reference-ownership operations used here.

- A **callable adapter** is a reference-counted object that stores one invocation target and implements the wrapper's result and parameter signature.
  The target is a function pointer, a member-function pointer together with its receiver pointer, or a callable represented by a stored C++ value or reference.
- A **receiver** is the C++ object on which a stored member function is invoked.
  A member-function adapter borrows its receiver; retaining or copying the adapter does not retain that object.
- An **empty wrapper** holds no adapter.
  `Action` permits empty invocation and supplies `clear`; `Func` invokes its adapter unconditionally and provides no empty-call behavior.

## Dependencies

- Memory's public [Native Ownership](../memory/native-ownership/native-ownership.md) service supplies the intrusive object base and smart pointer used to retain adapters and destroy them when their last reference is released.
  Adapter allocation uses C++ `new`, rather than Memory's allocator classes.
- The host C++ environment supplies function and member-function pointers, virtual dispatch, object copying and destruction, allocation, and RTTI for the existing adapter comparisons.
  The function and member-function adapters use `dynamic_cast` to recognize the same adapter type before comparing their stored targets.

The operations performed by a supplied callable and the lifetimes of its arguments remain client responsibilities.
For example, Compiler uses callbacks to ask whether an IR instruction will be folded and to emit function-return code, while platform window clients use actions to dispatch events.
Those uses do not make Callables depend on Compiler, windowing, or the other services invoked by a client's callable.
Likewise, the hashing and failure-reporting facilities reached through the native-ownership header remain dependencies of Native Ownership rather than direct callable operations.

## Principal Services

Callables is a public leaf of Foundation, with no child modules or separate build artifact.
Its closely related services form one small adapter-and-wrapper family:

- **Adapting invocation targets** provides the common `FuncPtr` interface and its function, member-function, and generic callable implementations: `CdeclFuncPtr`, `MemberFuncPtr`, and `LambdaFuncPtr`.
  Their public operations include invocation, target comparison, and explicit adapter cloning.
- **Retaining and invoking a callable** provides `Func<TResult, Arguments...>`.
  Construction or assignment from a target allocates an adapter; copying a `Func` shares that adapter through `RefPtr` rather than cloning its stored state.
- **Retaining and invoking an optional action** provides `Action<Arguments...>`.
  Copy construction and assignment share its adapter, and `clear` releases that wrapper's reference.
  Window event dispatch can therefore call an unassigned action without testing whether a handler was installed.

The adapter forms express alternative representations of the same stored-invocation service, rather than independent service boundaries.
The current public adapter classes and the distinction between `Func` and `Action` remain part of the implementation contract; choosing a leaf does not make those classes private or replace them with another callable abstraction.

## Lifetime and Invocation Contracts

The wrapper retains its adapter.
When the callable type is deduced by a `Func` or `Action` constructor or assignment, the generic adapter stores a copy of the supplied value.
That value type must support copying from a const reference and calling through const access; a function pointer can satisfy those requirements without declaring a member function.
The public `LambdaFuncPtr` template also permits an explicitly supplied lvalue-reference type, such as `F&`.
That specialization stores a borrowed reference, and const access to the adapter does not make the referenced callable const.
Copying and destroying a stored callable value follows that value type's C++ operations, which determine whether captured values own other objects or borrow them.
Copying a lambda's `this`, reference, or ordinary-pointer capture does not itself extend the referenced object's lifetime.
The member-function adapter likewise stores an ordinary receiver pointer without adding a reference.
Clients must keep every borrowed object alive for all invocations that use it.

For example, `WindowedAppBase::initializeBase` assigns `[this]() { mainLoop(); }` to one window action and a member-function/object pair for `windowSizeChanged` to another.
Both adapters are retained by their actions, but both still borrow the application object.
The example driver keeps its application alive while `Application::run` dispatches window events.
Compiler's `fixValueScoping` instead invokes a supplied folding predicate during the call; its caller's `[this]` capture does not become an owned emitter merely because the lambda is copied into an adapter.
Scheduling, event subscription, cancellation, and receiver-lifetime management remain client policies.

Explicit adapter `clone` operations allocate a distinct adapter with the same function pointer or member-function/receiver pair, or with the generic callable initialized from the stored target.
The generic adapter copies a stored value or binds the same target when its template argument is an lvalue-reference type.
Each `clone` returns an ordinary pointer to a new object whose intrusive reference count starts at zero; the caller must arrange retention or destruction.
Cloning does not make a borrowed receiver, referenced callable, or reference capture owning.
Ordinary wrapper copies do not invoke `clone`.

Target comparison is narrower than general callable equivalence.
Function adapters compare function pointers, and member-function adapters compare both the member-function pointer and receiver pointer, after recognizing the same adapter type.
Generic callable adapters always compare unequal, including when two wrappers share one such adapter.
`Func` comparison dereferences its left-hand adapter without an empty check.
These operations do not provide a general test for whether arbitrary callables compute the same result.

## Existing Implementation Mapping

The complete `old-source/core/slang-func-ptr.h` belongs to Callables: all six templates (`FuncPtr`, `CdeclFuncPtr`, `MemberFuncPtr`, `LambdaFuncPtr`, `Func`, and `Action`), their members and inline definitions, and the header guard.
There is no corresponding `.cpp` or out-of-line callable implementation.
The `RefObject` and `RefPtr` definitions in `slang-smart-pointer.h` remain with Native Ownership.
The scope-exit guard and macros in `slang-common.h` remain with [C++ Utilities](../base-types/cpp-utilities/cpp-utilities.md#principal-services).
Other callback interfaces, raw function-pointer declarations, and template algorithms that accept a callable remain with their owning services; this assignment does not collect all C++ invocation into one module.

Actual clients include `slang-ir-restructure-scoping.{h,cpp}`, `slang-ir-any-value-inference.cpp`, `slang-ir-autodiff-primal-hoist.cpp`, and `slang-emit-llvm.cpp` under `old-source/slang/`, and `tools/platform/window.h` and its window/event clients.
Their compiler algorithms and window policies are partial-file uses of Callables, not implementations assigned to this leaf.
In particular, the unrelated `SharedLibrary::FuncPtr` alias in `old-source/core/slang-platform.h` belongs to Host Services.

## Preservation Constraints and Ancestor Holds

Migration preserves existing names, signatures, overload selection, access, inheritance, member order, inline definitions, target copying, argument passing, reference retention, comparison, and empty-call behavior.
The current forwarding bodies pass their named arguments directly; this description does not introduce perfect forwarding or move-only callable support.
`Action` derives from `Func<void, Arguments...>` and also declares its own adapter member.
Its invocation, clearing, and explicit copy operations use that separate member; the inherited `Func` operations use the base member.
The commented-out alias in the legacy header is not the current representation, and replacing the class with that alias would change its layout and behavior.

The templates currently have canonical identities in `Slang` and are not installed product API declarations in `include/`.
That does not establish freedom to change their binary identities: the optional shared `platform` library returns a `Window*` whose public event fields contain `Action` objects used by other code.
The [root's staged migration contracts](../../slang.md#staged-migration-contracts) require preserving existing type identities and layouts across such interfaces; a namespace reexport can name the same declarations without redefining them.
The header-only implementation introduces no new callable library or exported product entry point.
Its native-ownership dependency remains subject to the existing static `core` and separately compiled enabled `slang-rt` consumers and their export settings.

Native Ownership still participates in the [Failure Reporting, Text, and Memory cycle](../memory/memory.md#unresolved-boundaries), and the root retains its broader artifact and construction questions.
This leaf records those ancestor holds without choosing a replacement failure mechanism or claiming a complete acyclic Foundation graph.
The ownership description does not authorize migration.
