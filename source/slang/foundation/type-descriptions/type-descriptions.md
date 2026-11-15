<!--
SPDX-FileCopyrightText: The Khronos Group, Inc.
SPDX-License-Identifier: CC-BY-4.0
-->

# Type Descriptions

Type Descriptions describes selected native C++ value layouts and provides operations that inspect or manipulate values through those descriptions.
Its services cover type names and categories, structure fields, numeric access, default-value tests, and construction, assignment, and destruction of described arrays and list storage.
Clients provide the meaning of their records and choose the descriptions and operation callbacks they need.

These services support JSON/native conversion and RPC in Compiler Support.
They describe the C++ records used by those clients, rather than types in a shader program, the compiler's AST type system, or the product reflection API.
The implementation remains in `old-source/core/`; the leaf boundary below refines [Foundation's assignment](../foundation.md#existing-implementation-mapping) without moving code or introducing another library.

## Concepts

[Foundation](../foundation.md#concepts) defines owned allocations, borrowed views, intrusive references, and string slices.
[Memory](../memory/memory.md#concepts) defines allocation families and arenas.
Those lifetime distinctions apply both to the descriptions and to the values being described.

- A **native type description** records a C++ value category, alignment, and size, with category-specific information such as an element description or a structure's fields.
  The existing representation is `RttiInfo` and its derived records.
  A description records existing storage layout; it does not allocate an instance of the described type or establish that every operation supports that category.
- A **described field** records a field name, its native type description, its byte offset from the containing object, and flags interpreted by default testing and JSON conversion.
  `StructRttiInfo` separately records its described superclass and its own ordered fields.
  The superclass traversal uses the same object address, without a base-subobject adjustment.
- A **field default** is the value selected for omission testing by the low bits of a field's flags.
  `RttiDefaultValue` selects normal, one, or minus one for numeric values and the corresponding truth value for booleans.
  Normal tests empty strings, lists, and dictionaries, recursively tests described structure fields, and uses the supplied test for an `Other` description.
  A field default is separate from the value produced by C++ default construction.
- A **type operation set** is the three callbacks for constructing, destroying, and assigning contiguous arrays of one described type.
  `RttiTypeFuncs` is valid only when all three callbacks are present.
  `RttiTypeFuncsMap` associates operation sets with description addresses and caches default lookup results, including invalid results.

`StructRttiInfo::Flag::Optional` controls omission of named fields in JSON objects: object input may omit the field, and object output may omit a value that passes its field-default test.
Positional structure input instead requires an array whose length equals the total described field count; positional output includes every field, regardless of this flag.
`RttiInfo::Kind::Optional` instead describes storage with a presence flag at the start and a described payload at a recorded byte offset.
The four assigned files provide that description record and operation cases, but no `GetRttiInfo` specialization for a general C++ optional container.

## Dependencies

- [Scalar Types](../base-types/scalar-types/scalar-types.md) supplies byte and index conventions used for storage traversal and array counts.
- [Memory](../memory/memory.md) supplies the arena holding copied field records, the standard allocation family used for list buffers, and the native `RefPtr` representation described by the templates.
  Description-driven list operations must allocate and release buffers through the same family as the ordinary list implementation.
- [Collections](../collections/collections.md) supplies temporary field and array-description sequences, the address-keyed operation map, and the list and dictionary representations inspected by the value operations.
  The list operations additionally use the existing detach, attach, capacity, and count contracts; this is a representation dependency rather than just use of a generic sequence interface.
- [Text](../text/text.md) supplies the described `String` and `UnownedStringSlice` types, their ordinary C++ operations, and `StringBuilder` for type-name rendering.
  A description of a borrowed slice does not retain its characters.
- [Failure Reporting](../failure-reporting/failure-reporting.md) supplies category and callback assertions, the prohibition on replacing an already valid operation set, unexpected-category assertions, and the release assertion used when list-copy allocation fails.
  The product header `include/slang.h` supplies the `SlangResult` codes, inline and unused-value annotations, and the array-length macro used by the structure provider.
- The host C++ environment supplies fixed-width integers, layout queries, pointer arithmetic, placement construction, explicit destruction, byte copying and clearing, and mutex synchronization for the shared description arena.
  The mutex guards allocation and arena reset; it does not synchronize a caller's operation map or value mutations.

JSON/native conversion and protocol record providers consume this service.
JSON parsing, diagnostics, and RPC framing remain with those clients' owners.
The broad `slang-basic.h` and product-helper includes do not establish reverse dependencies on those clients or additional services without a corresponding operation.

## Decomposition

Type Descriptions is proposed as one public leaf within Foundation, with no immediate child modules.
Its principal service areas are:

- **Description construction and inspection:** category tests, layout records, template-selected descriptions, structure-field collection, and human-readable type names.
  Providers may use a type's static `g_rttiInfo`, a supplied template specialization, `StructRttiBuilder`, or the `SLANG_MAKE_STRUCT_RTTI_INFO` field macros.
- **Operation registration and selection:** typed C++ callbacks, zero-initializable plain-data callbacks, address-keyed registration, and lazy selection of the supported default operation sets.
- **Value access and default testing:** integral and floating-point reads and writes, boolean interpretation, and recursive default tests over the described fields.
- **Storage operations:** classification for zero initialization, byte copying, and destruction; strided array traversal; and allocation and initialization of list elements.

These service areas remain together because they share complete declarations and call each other.
`RttiTypeFuncsMap::getFuncsForType` calls `RttiUtil::getDefaultTypeFuncs`; the selected structure callbacks call the recursive `RttiUtil` array operations, which query the map for nested values.
`OtherRttiInfo` also embeds an operation set beside its custom default test.
Separating metadata from operations by the two existing filename pairs would leave these relationships crossing the proposed split.
This leaf assignment keeps each existing class complete and introduces no new callback protocol or replacement representation.

## Description, Value, and Storage Contracts

### Description Identity and Lifetime

`GetRttiInfo<T>` ordinarily returns the address of `T::g_rttiInfo`.
The supplied scalar and text specializations refer to `RttiInfo::g_basicTypes`; the pointer, intrusive-pointer, list, dictionary, and fixed-array specializations hold function-local static descriptions.
The const specialization forwards to the unqualified type's description.
Address identity matters: the operation map uses description pointers as keys, and JSON conversion identifies its `JSONValue` extension by comparing the description address.
Equivalent-looking copies are not interchangeable keys.

`StructRttiBuilder` computes field offsets from a client-supplied sample object's addresses.
`make` copies its accumulated field records into the shared arena and returns a structure description by value.
The builder does not retain the sample object, copy the names, or retain the descriptions referenced by fields and the superclass.
Those pointers must remain usable for every later inspection or operation.
The macro-based provider instead creates static field and description records, uses `offsetof`, has no described superclass, and enables ignoring unknown JSON fields.

`RttiInfo::allocate` obtains shared arena storage under a recursive mutex.
The singleton's destruction releases its arena; `deallocateAll` resets the arena explicitly and is documented for shutdown after all description uses have ended.
Reset invalidates the copied field records without rebuilding the static descriptions that point to them.
The existing compiler shutdown entry point and test runner request this cleanup; the ownership assignment does not make reset safe during ongoing conversion or promise reuse afterward.

### Value Operations and Supported Categories

Callers supply storage matching the description and enough accessible elements for the requested count and byte stride.
These operations do not validate an arbitrary pointer against the recorded layout.
The description stores alignment in eight bits and size in sixteen bits, and its ordinary initializer narrows those values without a range check.
The structure builder likewise narrows field offsets to 32 bits.
Those representations describe native layout rather than a portable serialized format.

The presence of a description category does not imply uniform support across naming, default testing, callback lookup, and array operations.
For example, `EnumRttiInfo` has no enum payload implementation, enum default testing asserts, and the JSON converters return not-implemented for enums.
Dictionary descriptions record key and value types and support an empty-count default test, but neither default callback selection nor JSON conversion implements dictionary traversal.
Type-name rendering falls back to `%Unknown%` for unnamed categories it does not handle, including `Optional`.

Default callback selection handles built-in numeric and boolean sizes, strings, borrowed slices, lists, structures, and the optional category.
For other categories, default selection returns an invalid operation set; callers can supply operation sets through the map.
In particular, `OtherRttiInfo::m_typeFuncs` is not automatically consulted by that lookup.
Compiler Support's `JSONNativeUtil::getTypeFuncsMap` explicitly registers the callbacks it selects for `JSONValue`; the JSON-specific description and registration stay with that client.
Adding a valid set over an already valid cached entry violates an asserted contract, whereas an invalid entry can be replaced.

The numeric operations use the existing C++ casts and do not provide range validation.
Some misuse paths return failure; others assert and then have a fallback return value.
`setInt` asserts an integral category and returns failure for other cases; `setFromDouble` returns failure for unsupported categories.
The value-reading operations assert when their supported numeric or boolean cases do not apply.
These distinctions remain part of the existing behavior, rather than a new checked-conversion contract.

The strided array operations traverse fixed arrays, described structure fields and superclasses, optional storage, and map-selected callbacks after applying their zeroing, byte-copy, or destruction classifications.
Their switches differ from default callback selection: obtaining typed callbacks for `String` does not mean that the general non-byte-copy array switch has a string case.
The general copy and destruction switches have no direct string or intrusive-pointer case and reach their unexpected-category assertion for those cases when the preceding classification does not finish the operation.
Migration must preserve these paths and record suspected implementation defects separately, rather than silently broadening the service.

### List Representation and Allocation

The list adapter interprets ordinary lists through `List<Byte>` to access the common buffer, count, and capacity representation, then uses the element description and callbacks for typed values.
The ordinary list contract keeps all capacity slots valid, including slots beyond the logical count.
The adapter's destruction and buffer replacement therefore destroy the old capacity, while a count reduction only changes the logical count.
The callback copy operation assigns into already constructed elements.

`setListCount` allocates and constructs a new buffer, then attaches it with count equal to capacity.
The called `List::attachBuffer` requires an unattached destination; `setListCount` does not detach or destroy an existing buffer and is not a general resize operation.
It returns invalid-argument for a negative count and out-of-memory for a nonzero allocation failure, after it has selected and asserted the element callbacks.
The separate list-copy adapter grows when the incoming count exceeds the destination count, constructs and assigns the new elements, then destroys and releases the detached old buffer.
Its allocation failure uses a release assertion rather than the result-returning path.

JSON/native conversion supplies already constructed native records and uses these list allocation services while filling their fields.
On object input, omitting an optional field leaves its existing C++ value untouched; the default flags do not initialize that value.
Positional input rejects a field-count mismatch before converting any fields.
Conversions may fail after earlier fields or elements have been written, and the converters do not provide rollback.
JSON storage and borrowed-character lifetimes remain with the converter's container and its client, rather than with the native type description.

## Existing Implementation Mapping

The following complete files remain under `old-source/core/` and belong to this leaf.
The assignment includes inline operations, templates, macros, protected members, and private implementation support.

| Existing File         | Assigned Responsibility                                                                                                                                                                                                                |
| --------------------- | -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| `slang-rtti-info.h`   | The callback set and map; typed and zero-plain-data callback templates; all description records and category/default flags; `combine`; the description-selection template and its specializations; and the complete structure builder. |
| `slang-rtti-info.cpp` | The basic-description table; mutex-protected arena manager and cleanup entry points; recursive type-name formatting; structure-field copying; and operation-map lookup and registration.                                               |
| `slang-rtti-util.h`   | The structure/field provider macros and the complete `RttiUtil` declaration, including scalar access, default testing, callback selection, list allocation, classifications, and strided array operations.                             |
| `slang-rtti-util.cpp` | Checked array-size allocation; list and structure callback adapters; scalar and default-value operations; list allocation; recursive classifications; and strided construction, assignment, and destruction.                           |

There is no partial-file transfer out of those four files in this proposal.
The following related responsibilities remain with their existing owners:

- Compiler Support's `slang-json-native.{h,cpp}` owns both conversion directions, JSON field-presence and unknown-field handling, diagnostics, and its `JSONValue` operation registration.
  `slang-json-value.{h,cpp}` owns the complete `JSONValue` class, its `OtherRttiInfo`, and its custom default predicate; those members are not split out of that class.
- Compiler Support's `slang-json-rpc.{h,cpp}`, `slang-json-rpc-connection.{h,cpp}`, `slang-language-server-protocol.{h,cpp}`, and `slang-test-server-protocol.{h,cpp}` own their protocol records, static descriptions, field order, and conversion call sites.
  The RPC connection selects object or positional-array conversion and retains its conversion state.
- Compiler Support's `slang-json-source-map-util.cpp` and `slang-spirv-core-grammar.cpp` own their JSON-facing records, description providers, conversion calls, and interpretation of the converted values.
  The grammar loader explicitly transfers the JSON container's string pool to its resulting grammar object so borrowed character slices remain valid.
- Compiler's language server owns its conversion-map member and request/configuration consumers.
  `old-source/slang/slang-api.cpp` owns the product shutdown call to description cleanup; `tools/slang-test/slang-test-main.cpp` owns its additional cleanup request.
- Memory, Collections, and Text continue to own their allocator, arena, list, dictionary, intrusive-pointer, and string implementations.
  Describing or interpreting those layouts does not transfer their complete classes into Type Descriptions.

The existing unit tests in `tools/slang-unit-test/unit-test-rtti.cpp`, `unit-test-rtti-util.cpp`, and `unit-test-json-native.cpp` exercise names, scalar operations and classifications, callback registration and list copying, and a JSON/native record round trip.
Those tests are evidence of particular exercised cases, not coverage of every category or proof that all classifications and recursive paths are sound.

## Migration Constraints and Ancestor Coordination

The current build compiles both implementation files into static `core` and separately into enabled `slang-rt` under the runtime's export settings.
A port must preserve both consumers and the [root's staged migration contracts](../../slang.md#staged-migration-contracts), including existing declaration identities wherever required by binary interfaces.
No product export, namespace change, installed reflection API, or new build artifact is selected here.
The operation-pointer signatures, member access and order, category values, narrow size/alignment fields, field offsets, static initialization, shutdown lifetime, allocation family, and existing failure behavior all require preservation.

Type Descriptions depends on the Memory, Collections, Text, and Failure Reporting services involved in [Foundation's unresolved low-level cycles](../foundation.md#failure-reporting-and-basic-data-services).
Its leaf boundary does not resolve those cycles or establish migration readiness.
Foundation must coordinate any behavior-preserving separation of those dependencies; narrowing aggregate includes alone does not remove the service calls.

The descriptor records also store JSON-specific field policy, and the shared `RttiUtil` class combines all value operations.
The proposed leaf retains those complete representations and the existing dependency direction from JSON conversion to Foundation.
Moving the JSON-related fields or individual class members to another module would require a coordinated representation decision, rather than a filename-based extraction.
No such split or replacement reflection/serialization design is proposed by this refinement.
