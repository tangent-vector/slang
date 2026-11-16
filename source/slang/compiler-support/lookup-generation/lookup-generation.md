# Lookup Generation

Lookup Generation constructs static string lookup tables and generates their C++ implementation.
Clients supply key spellings and either C++ value expressions or an enumerant prefix from which the service forms them.
The generated lookup compares the query with a stored spelling before its match branch assigns the associated value.
The service supports repository generators without interpreting Slang declarations, capability relationships, or SPIR-V grammar rules.

This is a proposed public leaf of [Compiler Support](../compiler-support.md), with no child modules.
The implementation remains in `old-source/compiler-core/`; no implementation has moved into this directory.

## Concepts

[Foundation](../../foundation/foundation.md#concepts) defines owned storage and borrowed string slices.
[Hashing](../../foundation/hashing/hashing.md#concepts) defines ordinary and stable hash codes; the table-specific arithmetic described here is a separate algorithm.
[Diagnostics](../diagnostics/diagnostics.md#concepts) defines diagnostic sinks and the distinction between reported errors and returned results.

- A **minimal perfect hash** assigns each member of a fixed set of distinct keys to a different slot in a table with exactly as many slots as keys.
  That assignment alone does not establish whether an arbitrary query belongs to the set.
- The **lookup parameters** (`HashParams`) contain a salt table and a destination table of retained `String` keys.
  A **salt** is the initial integer state supplied to the table's hash computation.
  The first hash selects a salt, and a second hash using that salt selects a destination slot.
  The stored key at that slot allows the generated function to reject a query outside the set.
- A **value expression** is caller-supplied C++ text emitted as the value associated with a destination slot.
  `values[i]` corresponds to `destTable[i]`, rather than to the key at position `i` in the original input list.
  The generator does not parse or type-check these expressions.

The destination strings retain their character storage independently of the input list.
The fragment generator's function-name and value-type slices are borrowed only for the call; the returned `String` retains the generated text.
The file-writing entry point uses its diagnostic sink synchronously and does not retain the sink.

## Dependencies

- Foundation's [Strings](../../foundation/text/strings/strings.md) supplies retained key strings, borrowed query and parameter slices, byte comparisons, and builders for generated text.
- [Sequence Storage](../../foundation/collections/sequence-storage/sequence-storage.md) supplies the key lists, buckets, salt and destination tables, temporary occupied-slot flags, and stable sorting by bucket size.
- [Output Writers](../../foundation/output-writers/output-writers.md) supplies formatted output through `WriterHelper` and a `StringWriter` borrowing the local builder.
  These writers assemble text in memory; they do not write the output file.
- [Host I/O](../../foundation/host-services/host-io/host-io.md) supplies `File::writeAllTextIfChanged`, which compares the requested text with decoded existing text and writes through the native text-file operation when reading fails or the texts differ.
- [Diagnostics](../diagnostics/diagnostics.md) supplies raw error reporting when construction detects duplicate keys or exhausts its salt search.
  Reading grammar files, parsing capabilities, and choosing process exit codes remain with the generator tools.
- [Scalar Types](../../foundation/base-types/scalar-types/scalar-types.md) supplies count, index, and fixed-width integer conventions.
  [Hashing](../../foundation/hashing/hashing.md) supplies the existing `HashCode32` alias used for a salt parameter; construction does not call its ordinary or stable hash algorithms.
  [Failure Reporting](../../foundation/failure-reporting/failure-reporting.md) supplies assertions for the input count and matching table lengths.
- The product headers supply `SlangResult` and its success/failure conventions.
  The host C++ environment supplies numeric limits and the facilities exposed through these providers.
  Generated fragments additionally require `std::pair`, `UInt32`, `UnownedStringSlice`, and the caller's value type to be declared in their compilation context.

The broad string-utility include does not make text splitting, escaping, or JSON parsing services of this module.
The generated C++ names and enum-header paths are text supplied by clients; using those strings does not make Lookup Generation depend on the compiler's capability implementation or on SPIR-V headers.

## Service Areas

The construction and emission services stay together because the generated hash arithmetic and table order must agree with the construction algorithm.
The enum-file operation adds file assembly and failure reporting without introducing another lookup representation.

### Constructing the Tables

`minimalPerfectHash` first rejects equal key strings.
It then assigns keys to initial buckets using an unsalted hash and processes the buckets from largest to smallest, preserving the original bucket order for equal sizes.
For each nonempty bucket, the procedure searches positive salts until every key in the bucket reaches a different unoccupied destination or the search bound is exceeded.
After finding a salt, the procedure records that salt at the bucket's original hash index and stores the keys at their selected destinations.
The input list may be `hashParams.destTable` itself; the output initialization and assignments then also mutate the input list.
For distinct nonempty keys within the count limit, a successful search returns `HashFindResult::Success` with one destination per key and both output tables sized to the input count.

For a nonzero table size `n`, the hash starts with the supplied salt, updates a `UInt32` state as `(h * 0x01000193) ^ c` for each plain `char`, and reduces the result modulo `n`.
The unsalted computation uses zero as its initial state.
The search starts at salt one for each bucket and uses no random-number service.
The same ordered input and character arithmetic therefore produce the same construction decisions.
That property does not promise identical results across targets with different plain-`char` signedness for bytes above `0x7f`.

`HashFindResult::NonUniqueKeys` leaves the supplied output parameters unchanged because the duplicate check precedes table initialization.
`HashFindResult::UnavoidableHashCollision` means the bounded salt search failed; the name does not prove that no perfect hash exists.
That failure can leave initialized tables containing assignments from earlier buckets.
The bound is the existing unsigned expression `10000 * nBuckets`, checked after incrementing a colliding salt, rather than an exhaustive search of all salts.

### Generating a Lookup Function

`perfectHashToEmbeddableCpp` returns a function definition without includes or a namespace wrapper.
The caller supplies the value type, function name, lookup parameters, and values arranged in destination order.
The function asserts that the salt table, destination table, and value list have equal lengths; it does not reconstruct or validate the relationship between their contents.

The generated function has the shape `bool function(const UnownedStringSlice& str, ValueType& value)`.
It contains static salt and key/value tables and repeats the construction algorithm's hash arithmetic.
Initializing the key/value table evaluates the supplied C++ value expressions, which can modify the output or other state before the query is checked.
After table initialization, the function hashes the query twice and compares the stored key with the complete query slice.
The match branch assigns the associated value and returns `true`; the mismatch branch returns `false` without an explicit output assignment.
The generated hash and comparison code operates on the embedded tables without calling Lookup Generation.

For example, the existing GLSL instruction generator emits `lookupGLSLstd450` to map `"Ceil"` to `GLSLstd450Ceil`.
The file-writing service builds each value expression by concatenating `"GLSLstd450"` with the corresponding destination key.
The SPIR-V embedding generator uses the lower-level fragment service instead: its `dictToPerfectHash` reads each destination key back from the dictionary and formats the associated numeric value.
Both clients preserve the correspondence between a stored key and its value after construction has rearranged the keys.

### Writing an Enum Lookup File

`writePerfectHashLookupCppFile` constructs lookup parameters, creates enum value expressions by prefixing each destination key, and writes a complete C++ file.
The file includes `core/slang-common.h`, `core/slang-string.h`, and the supplied enum header when its path is nonempty.
It places a function named `lookup` followed by the supplied enum name in namespace `Slang`.
`writeHashFile` assembles that wrapper around the generated fragment; the operation is defined in the implementation file without a declaration in the shared header.

Duplicate keys and exhausted salt searches produce raw error diagnostics and return `SLANG_FAIL` before writing a file.
The sink must be non-null and alive on those reporting paths.
On successful construction, the operation returns the result from `File::writeAllTextIfChanged`; file-write failure is not separately diagnosed by this service.
When the decoded existing text matches, the operation succeeds without rewriting the file.
The comparison includes the reader's line-ending normalization; it is not a comparison of raw file bytes.
The file operation provides no atomic replacement or rollback guarantee.

### Current Input Limits

The compiler build's existing generated tables demonstrate nonempty vocabularies of identifier-like spellings with C++ expressions appropriate to their output types.
The tools also accept other inputs, including the lookup generator's line-separated word list.
The implementation does not validate every condition needed to compile and use the emitted text:

- An empty input list returns construction success with empty tables, but fragment emission produces zero-sized table declarations and modulo-zero arithmetic.
  Construction success alone therefore does not establish that the emitted function is usable.
- An empty destination string represents an unoccupied slot during construction.
  An empty key can consequently be mistaken for an unoccupied destination; the routine does not reject that input.
- Key spellings are inserted directly between C++ double quotes, and names, value expressions, and header paths are inserted without escaping or syntax validation.
  Embedded zero bytes also interact with the generator's zero-terminated `%s` formatting.
- The input-count check requires a count below the maximum `UInt32` value through an ordinary assertion.
  The salt-search bound uses fixed-width multiplication and is not an overflow-checked size guarantee.
- Construction and generated lookup must interpret character values compatibly.
  The plain-`char` loop does not normalize byte signedness or validate an encoding.

These are limits of the existing services, separate from their two reported construction failures.
This ownership refinement does not add validation or change the algorithms.

## Existing Implementation Mapping

All paths in the following table are relative to `old-source/compiler-core/`.
Each complete file has been inspected; all its declarations and definitions serve this leaf.

| Existing File                    | Assigned Responsibility                                                                                                           |
| -------------------------------- | --------------------------------------------------------------------------------------------------------------------------------- |
| `slang-perfect-hash.h`           | `HashParams`, `HashFindResult`, and the construction and fragment-generation declarations.                                        |
| `slang-perfect-hash.cpp`         | The complete bounded construction algorithm and complete C++ function-fragment emitter, including their local hash operations.    |
| `slang-perfect-hash-codegen.h`   | The enum-file generation declaration.                                                                                             |
| `slang-perfect-hash-codegen.cpp` | Complete file assembly in `writeHashFile` and the construction, value-expression assembly, and reporting of the enum entry point. |

The local `CMakeLists.txt` currently selects these implementation files for static `compiler-core` and links legacy Foundation `core`.
The proposed ownership does not move the files, create another artifact, or change declaration names or linkage.
In particular, the construction algorithm remains here together with its emitter, rather than moving to Foundation because it computes hashes.

The following clients and outputs remain outside this leaf:

- `tools/slang-lookup-generator/lookup-generator-main.cpp` reads JSON instruction names and aliases or line-separated text, then calls the enum-file service.
  `tools/slang-capability-generator/capability-generator-main.cpp` parses and prepares capability definitions before calling the same service.
  Their input interpretation and other generated outputs remain with those tools.
- `tools/slang-spirv-embed-generator/spirv-embed-generator-main.cpp` uses construction and fragment generation inside its grammar-specific embedding operation.
  Its dictionary adaptation, switch generation, qualified-enum encoding, and `SPIRVCoreGrammarInfo::getEmbeddedVersion` definition remain outside this leaf.
- `tools/CMakeLists.txt` builds those three executables against `compiler-core` and `core`, or imports them from `SLANG_GENERATORS_PATH`.
  Their implementation locations remain outside the root's initial implementation mapping.
- `old-source/slang/CMakeLists.txt` runs the tools and compiles `slang-lookup-capability-defs.cpp` into the `slang-capability-lookup` object target.
  It compiles `slang-lookup-GLSLstd450.cpp` and `slang-spirv-core-grammar-embed.cpp` into `slang-lookup-tables`.
  Those generated compiler components, their enum and grammar contracts, and their compiler/bootstrap link consumers remain with [Compiler](../../compiler/compiler.md), as assigned by the [root mapping](../../slang.md#existing-implementation-mapping).

The generated capability and GLSL lookups have existing compiler callers; the embedded grammar supplies function pointers through `SPIRVCoreGrammarInfo::Lookup`.
Their current use does not make the generic generator responsible for capability lookup policy, shader instruction checking, or the grammar object's lifetime.
The grammar accessor's declaration/generated-definition ownership question remains with [Compiler Support](../compiler-support.md#application-protocols-and-generated-grammar).

Future migration must preserve generated filenames, emitted lookup behavior, existing callers' access to the generator services, and applicable C++ type identities and signatures under the [root preservation contracts](../../slang.md#staged-migration-contracts).
The root's unresolved construction and source-reuse rules, and unresolved boundaries in the Foundation providers, remain constraints.
This phase-one refinement establishes proposed ownership and current contracts; it does not establish migration readiness.
