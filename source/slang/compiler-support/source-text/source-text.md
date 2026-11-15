# Source Text

Source Text makes program text available to its readers, interprets locations in that text, and finds files through supplied search directories and file systems.
It provides the common records and lookup services used by lexing, diagnostics, generated-text mapping, and compiler file loading.
Parsing directives, expanding macros, selecting module filenames, and deciding when a file should be included remain with the callers.

This is a proposed organization of the existing implementation in `old-source/compiler-core/`.
No implementation has moved into this directory.

## Concepts

[Compiler Support](../compiler-support.md#concepts) defines source files, source views, source locations, and source maps.
[Foundation](../../foundation/foundation.md#concepts) defines blobs and borrowed views.
The following distinctions govern their use together:

- **Path information**, represented by `PathInfo`, records how text originated, the path used to find it when available, and an optional file-system identity.
  A found path may be relative, and text supplied as a string can have a display name without identifying a loaded file.
  The identity supplied by the file system is distinct from the found path and need not itself be a path.
- A source file can hold decoded UTF-8 contents or just a size and location metadata when the contents are unavailable.
  Supplying contents retains the resulting blob; an unchanged UTF-8 blob may still borrow its bytes.
  Callers must keep that backing storage valid for all readers of the text.
  Slices returned from a file borrow its contents.
- A source view separates one use of a file from other uses of the same text.
  For example, the preprocessor creates another view when it processes a repeated inclusion, so each inclusion can have its own line directives and include origin.
  The file does not encode that per-use interpretation.
- A **location range**, represented by `SourceRange`, includes both its beginning and end; a view reserves the end position for the end of its input.
  Location zero is invalid.
  Comparing raw locations does not establish ordering through nested includes; a view can also record the separate ordering supplied by the preprocessor.
- A `SourceManager` owns the files and views it creates and resolves locations through those views, optionally searching a parent manager.
  The parent pointer is borrowed, and a view can refer to a file owned by a parent manager.
  Those owners must remain alive while their objects or locations are used.
  Initializing or resetting a child manager assumes the parent has finished allocating its location ranges.
  Resetting a manager deletes its files and views and permits reuse of its location range.
- A source map uses zero-based generated and originating line/column indices, with file names, optional names, and optional originating text held separately from the entries.
  Human-readable location results use one-based line and column numbers.
  A map can be attached to a file through a retained boxed value, with an association kind distinguishing normal and obfuscated mappings.
  Emission can skip an obfuscated mapping to avoid revealing the original locations.
- An **include search path** is an ordered directory list with an optional parent list searched afterward.
  Quoted lookup first tries a path relative to the including file; system lookup starts with the directory lists.
  An absolute requested path is looked up directly.

## Dependencies

- Foundation's [Text](../../foundation/text/text.md) supplies strings, slices, pooled path spellings, encoding detection and conversion, line scanning, and path-display escaping.
- [Memory](../../foundation/memory/memory.md), [Collections](../../foundation/collections/collections.md), and [Hashing](../../foundation/hashing/hashing.md) supply arenas, native ownership, the ordered records and identity indexes, and their lookup operations.
- [Base Types](../../foundation/base-types/base-types.md) supplies scalar and portability conventions; [Failure Reporting](../../foundation/failure-reporting/failure-reporting.md) supplies assertions for location and ordering contracts.
- [Object Interfaces](../../foundation/object-interfaces/object-interfaces.md) supplies reference counting and boxed values; [Blobs](../../foundation/blobs/blobs.md) supplies the retained content objects.
- [Digests](../../foundation/digests/digests.md) supplies the SHA1 digest cached for file contents.
- [Host Services](../../foundation/host-services/host-services.md) supplies path classification.
- The product headers under `include/` define the blob and file-system interfaces, path categories, result codes, and interface identifiers used by these services.
  Using those contracts does not assign the product API to Source Text.
- The host C++ environment supplies synchronization for lazy file-content digests and line-offset caches.

Source Text does not require tokenization, diagnostic reporting, JSON, artifacts, or compiler declarations.
[Source Map JSON](../source-map-json/source-map-json.md) depends on Source Text to encode and decode maps; the map representation does not depend on that codec.
File lookup, identity, loading, simplification, and display-path requests use a supplied `ISlangFileSystemExt`, which can represent storage other than host files.
Callers can supply implementations from Foundation's [File Systems](../../foundation/file-systems/file-systems.md); the Source Text implementation does not construct those implementations.

## Decomposition

Source Text is a public leaf module within Compiler Support.
Its principal service areas are:

- **Files and locations:** path information, content decoding and retention, line and column interpretation, distinct views of shared text, manager lookup and lifetime, line-directive records, and include-order location mapping.
- **Source maps:** construction, storage, comparison, and lookup of generated-to-originating location correspondences and their associated strings.
- **File lookup and loading:** ordered include searches, file-system identity, path simplification, and optional reuse of files already registered with a manager.

Location interpretation reads maps attached to files, and file loading can populate a manager's file set.
Lexing reads a view's text and forms token locations from offsets in its range.
The preprocessor supplies line-directive records, initiating locations, and include-order segments; diagnostics and generated-text writers request the appropriate location interpretation.
The nominal interpretation first tries source-map lookup, then falls back to the view's line directives and file coordinates.
The actual interpretation ignores line directives and source maps.
The emission interpretation follows the nominal rules but skips obfuscated maps.
Compiler code owns the production of those obfuscated maps and of correspondence entries for emitted programs.

Include lookup borrows its search lists, file system, and optional manager for the duration of its use.
The manager separately retains its configured file system.
Lookup selects and loads a file without recognizing a directive, applying `#pragma once`, checking include cycles, or interpreting an import.
Compiler callers retain those policies, including the choice of candidate filenames when they use the same lookup service for modules.

## Existing Implementation Mapping

The following paths are relative to `old-source/compiler-core/`; each `{h,cpp}` denotes both files.
The assignments cover all declarations and definitions in these six files.

- `slang-source-loc.{h,cpp}` contains path-origin records, compact locations and ranges, source files and their decoding and lazy caches, source views and interpretation records, and source managers.
  This includes human-readable and pooled-path location results, map-kind and interpretation selectors, and the include-order mapping stored on views.
  It does not include the compiler's parsing of line directives or its serialized debug-location format.
- `slang-source-map.{h,cpp}` contains the map entries, generated-line indexing, pooled strings and optional contents, construction and lookup operations, and comparison support.
  The JSON representation and variable-length mapping codec in `slang-json-source-map-util.{h,cpp}` belong to Source Map JSON.
- `slang-include-system.{h,cpp}` contains search-directory records and the supplied-file-system lookup, simplification, and loading service.
  It can use a manager to reuse or populate files by identity; it does not create their per-use views.

The artifact headers included by `slang-source-loc.cpp` and `slang-include-system.cpp` do not correspond to artifact operations in their bodies.
Blob creation, boxed values, and string adaptation used there come from Foundation.
Conversely, the path-bearing blob representation in `slang-artifact-representation-impl.h` belongs to [Artifacts](../artifacts/artifacts.md), which consumes this module's path records.

The current `compiler-core` static target compiles these implementations and links Foundation's legacy `core` target.
Compiler, shared support services, downstream integrations, and repository tools consume their internal C++ declarations.
The downstream request also borrows a `SourceManager*`; this document does not establish that moving its declaration to a new namespace would preserve existing binary contracts.
Any later migration must follow the [root preservation contracts](../../slang.md#staged-migration-contracts) and preserve access for the existing build consumers.
