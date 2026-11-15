<!--
SPDX-FileCopyrightText: The Khronos Group, Inc.
SPDX-License-Identifier: CC-BY-4.0
-->

# Digests

Digests computes SHA1 summaries of caller-supplied bytes and provides fixed-size digest values with binary and hexadecimal forms.
It also supplies adapters that append existing values to a digest computation.
Callers choose which data to summarize and how to distinguish the parts of that data.

This is a public leaf module within [Foundation](../foundation.md#candidate-decomposition).
Its primary header and implementation are `digests.h` and `digests.cpp` in this directory.
`foundation.h` reexports its public declarations in `slang::foundation`.

## Concepts

The [Foundation concepts](../foundation.md#concepts) define digests, blobs, strings, and string slices.
[Hashing](../hashing/hashing.md) owns hash-code computation and deliberately stable hashes.
The digest value's `getHashCode` operation supplies a lookup hash from the first stored word; digest comparison and binary or textual conversion use all of its bytes.

A **digest builder** appends the byte ranges selected by its convenience operations to a stored digest computation.
`DigestBuilder<Hash>` provides that adaptation; existing callers use `DigestBuilder<SHA1>`.
The builder does not add lengths or separators between appended values.
Callers that combine values supply any required framing themselves.

## Dependencies

- [Text](../text/text.md) supplies string and slice inputs, hexadecimal digit recognition, and storage for hexadecimal output.
- [Blobs](../blobs/blobs.md) supplies `RawBlob` storage for copies of digest bytes.
  The builder also reads byte ranges through the product blob interface.
- [Collections](../collections/collections.md) supplies the contiguous list storage accepted by the builder's list adapter.
- [Failure Reporting](../failure-reporting/failure-reporting.md) supplies the assertions used by the binary/hexadecimal conversion routines.
- The product headers inherited through [Foundation](../foundation.md#dependencies) supply scalar types, `ISlangBlob`, and COM pointer helpers.
  The host C++ environment supplies byte copying, comparison and clearing, C-string lengths, and the type traits used to constrain builder inputs.

The digest implementation does not call compiler services or Hashing's algorithms.
The service cycles recorded by [Foundation](../foundation.md#unresolved-boundaries) remain unresolved by this leaf's decomposition.

## Service Areas

Digests has no child modules.
Its services fall into three areas:

- **Digest values and conversion:** Fixed-size values store their bytes inline and support equality, hexadecimal conversion, and copying to and from blobs.
  `HashDigest<N>` provides the values, and `DigestUtil` performs binary/hexadecimal conversion.
- **SHA1 computation:** Callers may summarize one buffer or incrementally supply bytes.
  Construction and `init` initialize the computation; `finalize` pads and changes the accumulated state to produce the digest.
  Restarting a computation requires initialization.
- **Appending values:** The builder accepts byte ranges, strings and slices, blobs, digests, arithmetic and enum values, and lists whose elements have unique object representations.
  Scalar and list inputs contribute their existing object bytes; strings and blobs contribute their contents.
  The builder supplies neither a separate serialization format nor an encoding independent of those representations.

These operations share one concern: producing a digest from bytes and making the resulting value usable by callers.
SHA1's block processing and padding remain private implementation details within the leaf.

## Clients and Compatibility

[Compiler Support's Source Text service](../../compiler-support/source-text/source-text.md) computes digests of stored file contents.
Compiler services combine content digests, options, compiler versions, and other inputs for module freshness and entry-point cache keys.
The compiler also appends textual digests to specialized linkage names.
Those clients own the selection, ordering, and framing of their inputs.
For example, the option-set builder sorts option keys and includes string lengths; Digests does not interpret the options.

[Persistent Cache](../persistent-cache/persistent-cache.md) uses SHA1 digest values as keys, writes those values within its binary index entries, and uses their hexadecimal form in entry file names.
The existing `tools/gfx/` shader cache converts the product entry-point hash blob into such a key.
The compiler's serialized module container writes the digest bytes into its `SHA1` data chunk.
Record/replay uses hexadecimal digests to name captured blob files and to validate the recorded name's syntax during playback.
The existing digest, persistent-cache, and file-content unit tests are further clients.

The product `getEntryPointHash` and `getSessionDescDigest` operations return digest bytes through blobs rather than exposing the internal digest types.
The implementation header is not installed, and its declarations have no export annotations.
The definitions retain their canonical `Slang` identities, and `slang::foundation::digests` imports those same declarations with using-declarations.
`foundation.h` reexports those module names without creating new types or templates.
The digest bytes, inline value representation, conversions, and builder input representation remain unchanged.

The compiler's internal `ComponentType::buildHash` virtual operation and its overrides still pass `DigestBuilder<SHA1>&`.
This internal signature does not make the operation part of the product API.
Keeping the same canonical declarations preserves that contract, the client object fields containing digest values, and the existing binary representations without requiring a new C++ identity.
The product, text, blob, collection, and native ownership types used by the header also retain their existing identities.
The [root's proposed staged migration contracts](../../slang.md#staged-migration-contracts) preserve the identities of product interfaces and Foundation types used across existing binary interfaces.
The root's artifact and source-reuse questions remain unresolved.

## Existing Implementation Mapping

The complete former `old-source/core/slang-crypto.{h,cpp}` pair now belongs to this leaf:

- `digests.h` defines `DigestUtil`, `HashDigest<N>`, `SHA1`, and `DigestBuilder<Hash>`, including the value conversions and append adapters defined inline.
  It reexports the four canonical `Slang` declarations in `slang::foundation::digests`.
- `digests.cpp` implements binary/hexadecimal conversion and the complete SHA1 computation, including initialization, incremental input, padding, block processing, and the one-buffer convenience operation.
- `CMakeLists.txt` adds the implementation to the existing `core` target and, when enabled, the separately compiled `slang-rt` target.

The hash and stable-hash headers remain assigned to [Hashing](../hashing/hashing.md).
Cache storage, file-content caching, compiler input selection, serialization containers, and recording remain with their respective clients.
The legacy crypto files are removed, and all of their include clients now use `slang/foundation/digests/digests.h`.
Both compilation consumers retain their existing settings as described by [Foundation](../foundation.md#build-consumers-and-ancestor-decisions), without introducing a library for this leaf.
