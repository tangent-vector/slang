<!--
SPDX-FileCopyrightText: The Khronos Group, Inc.
SPDX-License-Identifier: CC-BY-4.0
-->

# Blobs

Blobs exposes contiguous byte sequences through the product blob interface.
It provides implementations that copy or take ownership of storage, borrow existing bytes, or retain another object alongside a supplied blob.
Clients choose the implementation according to the lifetime of their bytes; retaining a blob wrapper does not always retain those bytes.

The implementation remains in `old-source/core/`.
This document refines [Foundation's assignment](../foundation.md#existing-implementation-mapping); the [shared Text boundary and inherited dependency cycles](#unresolved-boundaries) remain migration constraints.
The product interfaces in `include/` remain existing external contracts.

## Concepts

[Foundation](../foundation.md#concepts) defines blobs, owned allocations, and borrowed views.
[Object Interfaces](../object-interfaces/object-interfaces.md#concepts) defines product interface references, interface discovery, and borrowed interface lookup.
[Text](../text/text.md#owned-and-borrowed-characters) describes strings, string representations, and their storage lifetimes.

A blob's **reported size** is the byte count returned by `ISlangBlob::getBufferSize`, rather than its storage capacity or a character count.
Zero bytes within that range are part of the byte sequence.
Some implementations additionally provide a zero byte immediately after the range; that byte is outside the reported size.
The product interface alone does not promise that extra byte, non-null storage for an empty sequence, or ownership of the storage.

`SlangTerminatedChars` supplies the existing product UUID used with borrowed lookup to request zero-terminated character access.
A successful lookup does not retain the object or its characters.
The blob implementations below decide whether they can satisfy that request; a zero byte somewhere within a binary payload does not establish termination at its reported end.

## Dependencies

- [C++ Utilities](../base-types/cpp-utilities/cpp-utilities.md) supplies move operations, unused-value annotations, and compiler compatibility declarations.
- [Object Interfaces](../object-interfaces/object-interfaces.md) supplies the standalone atomic product-reference base and its query, retain, and release implementations.
  Blobs determines the supported interface and contained-data lookups.
- [Heap Storage](../memory/heap-storage/heap-storage.md) supplies the allocator-matched `ScopedAllocation` used by `RawBlob`.
  [Native Ownership](../memory/native-ownership/native-ownership.md) supplies the unique-reference test and reference release used when transferring string representations.
- [Collections](../collections/collections.md) supplies the byte list stored by `ListBlob`, including its copy, move, capacity, and destruction operations.
- [Text's Strings](../text/strings/strings.md) supplies string representations, string detachment, and borrowed character ranges for `StringBlob` and the proposed text adaptations.
  Text and Collections supply the allocation behavior of their string and list constructors; Blobs does not introduce a common allocation-failure policy for them.
- [Failure Reporting](../failure-reporting/failure-reporting.md) supplies the assertion that a string representation is uniquely held before a blob adopts it.
  The reference bases and storage implementations have their own checked preconditions.
- The product headers supply `ISlangBlob`, `ISlangCastable`, UUIDs and UUID comparison, result codes, calling conventions, and `ComPtr` retention and output-reference operations.
  Those dependencies do not require the compiler implementation or assign the product headers to Blobs.
- The host C and C++ environment supplies object allocation and deletion and byte copying.
  The existing C++ `new` operations are separate from the result-returning checks around raw payload allocation.

The mixed header's `slang-allocator.h` and `<limits>` includes support the Memory-owned `ScopedAllocation`.
Their presence does not assign that class to Blobs.
File systems, compression, binary builders, artifacts, and compiler-library loading consume blobs; interpreting their payloads remains with those clients.

## Service Areas

Blobs is a proposed leaf with no child modules.
The storage variants implement one byte-access contract with explicit lifetime choices, and the retaining wrapper composes those choices without interpreting the bytes.
Its services are public within Foundation because file systems, compiler-support artifacts, compiler code, adapters, and existing tools construct and consume these implementations directly.
The protected storage and lookup hooks remain implementation support.

### Product Access and Borrowed Lookup

`BlobBase` supplies product reference counting and discovery of `ISlangUnknown`, `ISlangBlob`, and `ICastable`.
Successful `queryInterface` calls add a product reference; `castAs` returns borrowed pointers without adding one.
The last product release deletes a `BlobBase`-derived object, but each concrete implementation determines what happens to its bytes.
`StaticBlob` supplies its own lifetime behavior instead of using that base.

`StringBlob` additionally recognizes its existing class UUID and the terminated-character UUID.
`RawBlob` recognizes terminated-character access only when its allocation has a spare byte containing zero at the reported end.
`ListBlob` makes the analogous check against its list capacity and the byte after its logical elements.
Neither `UnownedRawBlob` nor `StaticBlob` recognizes terminated-character access, even when the supplied bytes happen to have that representation.

Compiler Support's `SliceUtil::getTerminated` demonstrates why that distinction matters.
It first handles empty input and a zero byte at the last reported position, then requests the terminated-character UUID before borrowing a range that needs a zero byte beyond its reported end.
Its conversion operations copy into caller-supplied storage when the supplied blob cannot provide terminated access.
The returned character range does not acquire ownership of the blob or its bytes.

### Owned Byte Storage

`RawBlob::tryCreate` copies the supplied byte range into a `ScopedAllocation` and adds a zero byte outside the reported size.
It accepts a zero-length range, rejects a null pointer with nonzero size, and clears the output blob on either invalid input or a failed terminated payload allocation.
That allocation failure includes the unrepresentable request for `size_t`'s maximum size plus its extra byte.
`RawBlob::create` returns null for those reported failures.
The blob object's C++ allocation is not converted into a `SlangResult` by either factory.

`RawBlob::moveCreate` instead swaps the supplied allocation into a fresh blob and leaves the caller's allocation empty.
It neither copies the bytes nor adds termination; the allocation's size, capacity, and contents determine the result.
The allocation must follow Heap Storage's allocator pairing.
For example, `OSFileSystem::loadFile` transfers the terminated allocation produced by `File::readAllBytes`, whereas LZ4 compression transfers an allocation sized for compressed binary bytes.
The file and compression services retain responsibility for constructing the payload and handling their own operational results.

`ListBlob::create` copies the supplied byte list, and `moveCreate` transfers its buffer, logical count, and capacity while emptying the caller's list.
The reported size is the list's logical count; spare capacity is not part of the payload and is not a promise of a terminator.
For example, `BlobBuilder::writeToBlob` transfers the output stream's byte list after constructing the binary layout.
Blobs owns the resulting list storage, while Binary Layouts owns how those bytes were arranged.

`StringBlob::create` copies nonempty input into a separate representation with one native reference.
The blob's character range excludes the additional zero terminator.
Both copying overloads use no representation for empty input and expose static empty text.
The two `moveCreate` overloads detach the input string's representation, leaving that string empty.
They adopt a uniquely referenced representation; for a shared representation, they make a private copy and release the detached reference.
A uniquely referenced allocated empty representation can therefore be adopted, whereas copying empty input or copying a shared empty representation produces the static empty form.
`StringBlob` directly deletes the representation it uniquely holds when the blob is destroyed.
That unique-storage invariant prevents ordinary `String` copies elsewhere from sharing the blob's representation; the reference count alone does not synchronize access to its character bytes.

### Borrowed Storage and Retained Scope

`UnownedRawBlob` has a reference-counted wrapper but only stores the supplied pointer and byte count.
Its factory does not validate that pointer/count pair or retain a byte owner.
The caller must keep the supplied storage valid for every later access that needs those bytes.
For example, `ZipFileSystemImpl::storeArchive(false, ...)` returns this borrowing form over the archive's storage, whereas its owning form copies through `RawBlob::tryCreate`.
Retaining the borrowing result does not keep that archive storage valid across its owner's destruction or replacement.

`StaticBlob` also borrows its bytes, and its `addRef` and `release` operations both return one without counting references or deleting the wrapper.
The caller is responsible for the lifetime of both the wrapper and its storage.
The embedded core and GLSL module providers construct static wrappers around their generated byte arrays; the owning program or loaded image supplies those lifetimes.
Calling the product reference operations does not keep a containing library loaded.

`ScopeBlob` retains a supplied non-null blob and an optional `ISlangUnknown` scope object.
It forwards byte access to the retained blob and retains a queried `ICastable` interface when available.
Borrowed lookups first try the wrapper's own interfaces, then forward otherwise unanswered requests to that contained castable interface.
Its `queryInterface` does not forward arbitrary requests to the contained blob.
The caller must choose a scope object whose retention actually preserves any external storage needed by the supplied blob; the wrapper does not infer that relationship from a pointer.

## Existing Implementation Mapping

The following paths are relative to `old-source/core/`.
The assignment includes inline factories, constructors, destructors, protected helpers, member storage, and lookup support for each listed blob class.

| Existing Implementation                                                                         | Blobs Responsibility                                                                                                                                                                                                                        |
| ----------------------------------------------------------------------------------------------- | ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| `slang-blob.h`, except the complete `ScopedAllocation` class                                    | `BlobBase`, `StringBlob`, `ListBlob`, `RawBlob`, `UnownedRawBlob`, `StaticBlob`, and `ScopeBlob`, including their complete class contracts and inline implementation.                                                                       |
| All of `slang-blob.cpp`                                                                         | Blob interface and contained-data lookup, string-representation copying and transfer, unique-storage destruction, conditional termination lookup, static lifetime operations, and forwarding borrowed lookup through the retaining wrapper. |
| Only `StringUtil::getSlice`, `getString`, and `createStringBlob` in `slang-string-util.{h,cpp}` | Proposed Blobs-owned text adaptations, subject to the shared-class ownership decision below; the remaining `StringUtil` operations and `LineParser` retain Text's assignment.                                                               |

`ScopedAllocation` remains assigned in full to Memory's Heap Storage, including allocation, resizing, copying, transfer, size/capacity bookkeeping, and optional termination.
Its physical location in the blob header does not make Blobs the allocation service.
`BlobBuilder` remains with [Binary Layouts](../binary-layouts/binary-layouts.md), and client-specific subclasses such as Compiler's `DxcBlob` remain with their respective integration services.
The product `slang_createBlob` entry point remains Compiler-owned: it calls `RawBlob::create` but additionally rejects null input or a zero size.
That wrapper's narrower input policy is not the internal factory's contract.

The current CMake selection compiles `slang-blob.cpp` into static `core` and separately into enabled `slang-rt` under runtime export settings; header factories are also instantiated by their clients.
The [root's staged migration contracts](../../slang.md#staged-migration-contracts) require preserving both compilation consumers and the existing product interfaces, UUIDs, calling conventions, and exported entry points.
The canonical `Slang` class identities, layouts, member order, names, signatures, access, and inline choices remain unchanged during a structural port.
A module reexport would refer to those same declarations rather than introduce another class or product interface.

## Unresolved Boundaries

`StringUtil::getSlice` borrows a supplied blob's bytes, removing one final zero byte from the returned range when that byte lies inside the reported size.
It returns an empty slice for a null blob or zero reported size without retaining the blob.
`getString` copies a nonempty resulting range into string storage and returns a string without a representation for an empty range.
`createStringBlob` calls the copying `StringBlob::create` overload.
These are the same three candidate adaptations identified by [Text](../text/text.md#blob-adaptations).

Their purpose fits Blobs, but all three remain members of the same `StringUtil` class as Text's splitting, parsing, and formatting operations.
Foundation must reconcile the complete class declaration and its member definitions while preserving the existing class identity and signatures.
Moving only definitions or includes does not remove the current Text-to-Blobs construction call or Blobs' use of Text storage.
This refinement does not choose a class rewrite or claim an acyclic Text/Blobs graph.

Blobs also depends on the unresolved Failure Reporting, Text, Native Ownership, and Collections relationships described by [Foundation](../foundation.md#failure-reporting-and-basic-data-services).
Its own uniqueness assertion and the checked storage/reference operations do not disappear when an aggregate header is replaced with narrower includes.
Foundation must settle those boundaries through a behavior-preserving separation or coherent ownership decision before affected services migrate.
This phase-one description authorizes neither a code port nor a change to storage, failure, reference-counting, or caller-lifetime behavior.
