<!--
SPDX-FileCopyrightText: The Khronos Group, Inc.
SPDX-License-Identifier: CC-BY-4.0
-->

# Persistent Cache

Persistent Cache provides a disk cache that maps byte digests to blobs, with an optional entry-count limit.
Clients choose the keys and the meaning of the stored bytes; the cache manages files, an index, replacement, and local usage statistics.
This is a public leaf within [Foundation](../foundation.md#candidate-decomposition), with its implementation still in `old-source/core/slang-persistent-cache.{h,cpp}`.
It introduces no new library or product interface.

## Concepts

[Digests](../digests/digests.md) defines digest values and their byte and hexadecimal forms.
[Blobs](../blobs/blobs.md#concepts) describes the storage and lifetime contracts of byte buffers, and [Host Services](../host-services/host-services.md#concepts) defines host paths and file locks.

- A **cache key** is the twenty-byte `SHA1::Digest` value supplied by a client.
  Equality compares all twenty bytes; the cache neither computes the key from the stored bytes nor checks that they match a digest.
- A **cache entry** associates a key with an entry file and a record in the cache index.
  The entry file contains the client's bytes, and the index records the key and a 32-bit usage age for replacement.
  Entry files without index records are not discoverable through `readEntry`.
- A **cache directory** is the host directory shared by one set of entry files, its `index` file, and its `lock` file.
  The key is the only entry identifier within that directory; clients choose directories or key inputs that distinguish incompatible uses.

## Dependencies

- [Digests](../digests/digests.md) supplies key storage, byte equality, and lowercase hexadecimal entry names.
- [Blobs](../blobs/blobs.md) supplies the owning `RawBlob` returned by a read.
  The product `ISlangBlob` interface supplies the byte pointer and length consumed by a write.
- [Host I/O](../host-services/host-io/host-io.md) supplies path composition and directory enumeration, whole-file transfers, index streams, and the opened file lock.
  The implementation calls those host operations directly rather than going through [File Systems](../file-systems/file-systems.md).
- [Memory](../memory/memory.md) supplies intrusive ownership of the cache object and the `ScopedAllocation` transferred into a returned blob.
  [Collections](../collections/collections.md) supplies the temporary index list, and [Text](../text/text.md) supplies retained paths and filename construction.
- [Scalar Types](../base-types/scalar-types/scalar-types.md) supplies counts and indices, and [Failure Reporting](../failure-reporting/failure-reporting.md) supplies assertions for a non-null write blob and a selected eviction entry.
- Product support headers inherited through [Foundation](../foundation.md#dependencies) supply result codes, result propagation, blob interfaces, and COM pointer transfer.
  The host C++ environment supplies `std::mutex`, its scoped guard, fixed-width integers, and byte copying and comparison.

The cache has no service dependency on compiler or graphics policy.
The text, ownership, collection, and failure dependencies retain Foundation's [unresolved service cycles](../foundation.md#unresolved-boundaries); this leaf does not resolve them.

## Service Areas and Contracts

Persistent Cache has no child modules.
Directory/index management, blob transfer, replacement, and statistics belong to one service because each read or write can update both entry usage and the shared index.
The index format and the directory visitor remain private implementation details.

### Keys and Blob Lifetime

For example, `RendererBase::getEntryPointCodeFromShaderCache` in `tools/gfx/renderer-shared.cpp` obtains an entry-point hash from the compiler and copies that hash blob into a cache key.
On any cache read result other than `SLANG_OK`, that client generates code and attempts to store the resulting blob.
It ignores the cache write result, so cache storage failure does not replace a successful compilation result.
Compiler input selection and shader-code generation remain client responsibilities.

`writeEntry` reads a non-null blob synchronously and retains neither the blob nor its buffer.
`readEntry` loads a listed file into a new allocation, moves that allocation into `RawBlob`, and transfers the blob reference through `outData`.
The returned bytes therefore remain valid while the blob is retained, independently of the cache object's lifetime or later removal of the file.
The read assigns that output before saving the updated index, so an index-write failure can return a failure result with a populated output.
Other read failures do not clear the caller's output.

### Storage and Replacement

Construction copies and lexically simplifies the directory spelling supplied by `Desc::directory`; the cache does not retain that character pointer.
Entry filenames are the forty lowercase hexadecimal characters of the key, directly beneath the cache directory.
The index begins with the `SLS$` magic and version 1, followed by a count and reserved field, then the native object bytes of the key/age records.
The reader checks the magic, version, and payload size and uses exact stream reads.
This is an existing native-layout cache format, without an endian-independent encoding or entry-content checksum.

A positive `Desc::maxEntryCount` enables replacement when a new key is written at or above that count; the default zero imposes no limit.
The implementation increments stored ages and resets a successfully read or rewritten key's age to zero.
Replacement selects the first record with the greatest nonzero incremented age.
If every age wraps to zero, no record is selected, so the new-key replacement path does not satisfy its assertion that an eviction index exists.
Rewriting a key refreshes its record and removes later duplicate records for that key without evicting another entry.
This is the current least-recent-use policy, implemented with wrapping 32-bit ages rather than timestamps.
The limit governs indexed entries rather than byte usage, and reopening an already larger index with a smaller limit does not trim it to that limit.

`clear` enumerates the immediate directory and attempts to remove each file whose simplified pathname differs from the recorded lock pathname, including files whose names are not cache keys.
That comparison uses the path strings rather than resolved file identity.
It leaves subdirectories and the cache directory itself in place.
Clients therefore need a directory dedicated to this cache.

### Coordination, Failures, and Statistics

Index operations acquire an instance-local C++ mutex followed by the directory's exclusive file lock.
The file-lock provider uses `LockFileEx` on Windows and `flock` on the other host branch.
That ordering expresses the implementation's intended coordination of participating threads and processes, but the guard discards lock and unlock failures.
The constructor also discards directory-creation, lock-opening, and initialization results; subsequent entry and clear operations report `SLANG_E_CANNOT_OPEN` when the lock file was not opened.
These facts do not establish unconditional concurrent-access safety.

An absent index or unlisted key produces `SLANG_E_NOT_FOUND`.
Index validation and I/O failures propagate from a read; failure to read a listed file removes its record if the following index write succeeds.
A write ignores an index-read failure and proceeds with the list left by that read, which can already have been resized before a payload-read failure.
If saving the index fails, a write attempts to remove a new key's file but keeps a rewritten key's file.
Entry and index files are overwritten directly, deletion failures are ignored, and `clear` ignores enumeration and removal failures.
The implementation supplies neither a transaction nor crash-durable replacement, and does not reconstruct the index by scanning orphan files.

Statistics belong to the cache instance.
Each read initially counts as a miss and becomes a hit when its entry file is read successfully, before the index is saved.
The entry count reflects the index last loaded during initialization or saved successfully by that instance; it can become stale when another instance changes the directory.
`resetStats` zeros all three counters without changing disk contents, and `clear` zeros only the entry count.
The miss increment precedes the mutex, while `getStats` returns a reference and `resetStats` writes the counters without locking.
The statistics therefore provide neither a synchronized concurrent snapshot nor an authoritative count of files on disk.

## Existing Implementation Mapping

Both legacy files belong wholly to this leaf:

| Existing File                                | Assigned Responsibility                                                                                                                                     |
| -------------------------------------------- | ----------------------------------------------------------------------------------------------------------------------------------------------------------- |
| `old-source/core/slang-persistent-cache.h`   | Complete `PersistentCache` contract and private state, including its descriptor, statistics, digest key, index records, and test friendship.                |
| `old-source/core/slang-persistent-cache.cpp` | All cache operations and private index-format implementation, including the clear visitor, rewrite and eviction policy, index header, and format constants. |

The class has no export annotation and is not an installed product interface.
Its current `Slang::PersistentCache` name, signatures, index layout, and failure behavior remain unchanged by this description.
The existing `core` target compiles the implementation, and enabled `slang-rt` separately compiles the same core directory under runtime export settings.
Any later migration must preserve both consumers under the [root's staged migration contracts](../../slang.md#staged-migration-contracts).

`tools/gfx/renderer-shared.{h,cpp}` remains a client, including renderer configuration, shader-key acquisition, compilation fallback, and adaptation to `IShaderCache` and its statistics.
`tools/slang-unit-test/unit-test-persistent-cache.cpp` remains test infrastructure; its friend access and file-system-based cleanup do not assign those services to this leaf.
Its basic, eviction, rewrite, index-write-failure, corruption, and stress cases are existing coverage, not evidence of tests run for this documentation change.
The timestamp-prefixed built-in module cache in `old-source/core/slang-builtin-module-cache.{h,cpp}` remains outside this assignment, as recorded by [Foundation](../foundation.md#existing-implementation-mapping).
