<!--
SPDX-FileCopyrightText: The Khronos Group, Inc.
SPDX-License-Identifier: CC-BY-4.0
-->

# Command Options

Command Options describes groups of named options and named values, looks up their spellings and client-supplied identifiers, and composes help text from those descriptions.
Applications supply the option meanings and decide how an argument changes their requests.
The existing text renderer includes the [Slang tool help conventions](#preserved-help-policy-and-remaining-dependencies) described below.

The implementation remains in `old-source/core/`.
This document refines [Foundation's assignment](../foundation.md#existing-implementation-mapping); it does not move code or change argument parsing, help output, or public interfaces.

## Concepts

[Foundation](../foundation.md#concepts) defines borrowed views, string slices, owned allocations, and intrusive references.
[Text](../text/text.md#concepts) defines slice pools and their pool-local spelling handles.

- A **command-option catalog** records categories and their entries, including names, descriptions, usage text, optional display headings, and external links.
  `CommandOptions` is the existing catalog type.
- A **category** groups either command-line options or named values.
  Option spellings share one lookup domain across all option categories; value spellings belong to the individual value category.
  Category names have a separate lookup domain.
- An **entry** describes one option or one named value, with one or more aliases.
  Both kinds use the existing `CommandOptions::Option` record; the category determines which kind the record describes.
  The first alias is the primary spelling.
- A **user value** is an optional, client-supplied signed integer identifier for a category or entry.
  `CommandOptions::UserValue` keeps that identifier separate from a catalog index; `CommandOptions::kInvalidUserValue` means no identifier was supplied.
  Command Options records the identifier without interpreting a compiler option or target enumeration encoded in it.
- **Usage text** describes how an option is written.
  A recognized `<category-name>` reference connects that text to a catalog category for help presentation; the notation does not define an argument grammar or validate an option's value.

## Dependencies

- [Text's Strings](../text/strings/strings.md) supplies character slices, builders, ASCII classification, and comparison.
  [Text Operations](../text/text-operations/text-operations.md) supplies splitting, joining, line traversal, and the reusable name/value table records accepted by catalog construction.
  [String Interning](../text/string-interning/string-interning.md) supplies owned spellings and pool-local handles for lookup and Markdown anchors.
- [Memory](../memory/memory.md) supplies arena storage for copied descriptions and combined alias lists, and intrusive ownership for renderer instances.
- [Collections](../collections/collections.md) supplies ordered category, entry, and link storage, borrowed array views, and lookup dictionaries.
  [Hashing](../hashing/hashing.md) supplies hash-code dispatch and combination for the name and user-value keys defined here.
- [Scalar Types](../base-types/scalar-types/scalar-types.md) supplies counts, indices, and integer conventions.
  [C++ Utilities](../base-types/cpp-utilities/cpp-utilities.md) supplies compatibility annotations, and [Bit Scanning](../base-types/bit-scanning/bit-scanning.md) supplies the most-significant-bit query used to try longer registered option prefixes first.
- [Failure Reporting](../failure-reporting/failure-reporting.md) supplies assertions for catalog construction and lookup preconditions.
  Product headers supply result codes and inline annotations; the host C++ environment supplies allocation and the stable sort used to group value aliases.

The aggregate `slang-basic.h` include does not establish dependencies on all Foundation services.
Despite its name, `CommandOptionsWriter` builds a string and does not call [Output Writers](../output-writers/output-writers.md) or [Host Services](../host-services/host-services.md).
The application chooses where to send the result.
Compiler tables passed to catalog construction are client data, rather than a dependency on compiler target definitions.

## Decomposition

Command Options is a public leaf within Foundation, with two related public services and their implementation support:

- **Catalog construction and lookup** records categories, aliases, descriptions, links, and user values through `CommandOptions`.
  Name lookup, reverse lookup by user value, category enumeration, and range-count queries let a client translate between its identifiers and help metadata.
  Alias suffix recognition and usage-reference scanning interpret only the catalog's own textual conventions.
- **Help composition** appends help for a catalog or for one selected category through `CommandOptionsWriter`.
  Its public style and layout settings select terminal text, Markdown, or Markdown without links, with an output builder available to the caller.
  The existing text renderer applies the Slang help conventions described below.

The renderer consumes the catalog; catalog construction and lookup do not invoke a renderer.
Name and user-value keys, pool storage, prefix-length bookkeeping, and category entry spans support the catalog.
The concrete `TextCommandOptionsWriter` and `MarkdownCommandOptionsWriter` classes, escaping and punctuation operations, anchor generation, wrapping, and indentation are implementation support rather than separate public services.
The concrete classes and some free functions are declared only in the implementation file, even where they have external C++ linkage.
That placement does not make their class names a supported client interface or establish a new library boundary.
There are no child modules in this refinement.

## Catalog and Rendering Contracts

### Construction, Lookup, and Storage

Catalog construction selects a category before adding its entries.
Each category occupies one contiguous span of the common entry list, so a populated category can receive more entries only while its span still ends at the end of that list.
Aliases and supplied user values must be unique within their respective lookup domains.
Construction is not a transactional validation API: duplicate registration can assert, internal operations can fail after changing lookup storage, and public entry-adding operations return no result code.
Clients provide consistent tables rather than recover from malformed registration through these operations.

Names and nonempty descriptive text are copied into pool or arena storage; empty text can use static storage, and a null optional string can remain a null slice.
The stored slices carry no owning reference of their own.
Returned entry references and array views additionally depend on the corresponding lists' storage and mutations.
Indices and spelling handles belong to their catalog and cannot identify an entry in another catalog.

Name lookup returns `-1` when no entry is found and is case-sensitive except for the explicit case-insensitive category operation.
Option lookup first tries the complete spelling, then registered attached-value prefixes from longest to shortest; value and category lookup do not perform that prefix search.
The string-based option registration recognizes `...` and `?...` suffixes as prefix flags, and prefix lengths must fit the existing 32-bit bookkeeping, which requires fewer than 32 bytes.
Those flags do not consume a following argument or validate an attached value, and an exact spelling can still match an option marked `CommandOptions::Flag::IsPrefix`.
For example, registering `-D?...` lets lookup of `-DNAME` find the `-D` entry; the caller still interprets `NAME` or consumes the argument after a bare `-D`.
The explicit case-insensitive category lookup is a separate operation used by help selection.

### Help State and Application Policy

A renderer copies its indentation text and owns its output builder and Markdown anchor storage.
An append borrows the supplied catalog during the call; the output and anchor cache remain on the renderer afterward.
Appending again continues that state rather than implicitly starting a new document.
The current compiler help path constructs a fresh renderer for each help request.

Full text help omits value-category bodies and option categories named `Internal` or `Repro`, then lists all category names with a `slangc -h <help-category>` instruction.
Selected-category text help renders the requested category, including a value category, and option descriptions can be followed by `slangc -h` instructions for referenced categories.
Full Markdown help renders all categories; the linked style adds navigation anchors and external links, whereas the no-link style omits that link presentation.
Selected-category Markdown rendering suppresses links even when the selected style is the linked style.
The existing escaping, anchor selection, wrapping, and category filtering behavior remain preservation constraints rather than a promise of a general Markdown processor or argument parser.

## Existing Implementation Mapping

The paths below are relative to `old-source/core/`.
The mapping accounts for the complete four files assigned by Foundation, including inline declarations, internal support, and the existing text-help conventions.

| Existing Implementation            | Current Assigned Responsibility                                                                                                                                                                                                                                                                   |
| ---------------------------------- | ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| `slang-command-options.h`          | The complete `CommandOptions` catalog contract: category and entry records, aliases and links, lookup domains and keys, flags and user identifiers, construction and lookup operations, inline views and accessors, usage queries, range queries, and protected storage and support declarations. |
| `slang-command-options.cpp`        | All catalog definitions, including copied text and links, category selection and contiguous entry spans, alias grouping, suffix recognition, exact and prefix lookup, case-insensitive category search, usage-reference scanning, and user-value range counts.                                    |
| `slang-command-options-writer.h`   | The complete `CommandOptionsWriter` class: style and layout settings, style metadata, factory and append operations, builder access, protected rendering hooks and formatting support, and retained renderer state.                                                                               |
| `slang-command-options-writer.cpp` | The style table and factory, both complete concrete renderers, Markdown escaping and punctuation support, anchor generation and cache, line wrapping and indentation, and the built-in Slang help policy in the text renderer.                                                                    |

Compiler-specific catalog construction remains in `old-source/slang/slang-options.cpp`: `initCommandOptions` supplies the compiler's categories, names, descriptions, and identifier mappings.
`Session::init` in `old-source/slang/slang-global-session.cpp` populates the catalog stored by `Session`.
`OptionsParser` in `slang-options.cpp` consumes arguments, interprets lookup results, diagnoses invalid options and values, and mutates compilation requests.
Its `_parseHelp` chooses the category, uses the selected style, adds a compiler usage preamble for full help, invokes help composition, and writes the resulting string to standard output.
`CompilerOptionSet::writeCommandLineArgs` in `old-source/slang/slang-compiler-options.cpp` also uses reverse lookup to recover option spellings; compiler-specific argument serialization stays with that consumer.
These uses do not assign the compiler's tables, diagnostics, target policy, or request processing to Command Options.

The current `old-source/core/CMakeLists.txt` compiles both implementation files into static `core` through source discovery.
Enabled `slang-rt` separately discovers and compiles the same files through `old-source/slang-rt/CMakeLists.txt`, under its runtime export settings.
The two command-options types have no export annotations of their own, but their signatures and layouts use existing Foundation types, and the renderer derives from `Slang::RefObject`.
Future migration must preserve both build consumers and the [root's staged binary-interface contracts](../../slang.md#staged-migration-contracts); this refinement selects no namespace or ABI change.
`tools/slang-unit-test/unit-test-command-options-writer.cpp` is existing coverage for the catalog and the three rendering styles, rather than another implementation assigned to this leaf.

## Preserved Help Policy and Remaining Dependencies

The text renderer combines generic layout with built-in Slang tool help policy in the same class and methods.
`TextCommandOptionsWriter::appendDescriptionImpl` filters `Internal` and `Repro` and emits the `slangc -h <help-category>` footer.
`TextCommandOptionsWriter::_appendDescriptionForCategory` emits further `slangc -h` guidance after an entry description when the entry's usage text references catalog categories.
Neither method calls compiler code; both interpret supplied metadata using those built-in conventions.
Foundation owns this complete catalog and help service, including those existing presentation conventions.
The renderer uses catalog metadata and Foundation services, while the compiler client retains option meanings, argument processing, and compilation requests.
Keeping the renderer together preserves its current behavior and class contract without introducing a compiler-service dependency.

Command Options also uses services in [Foundation's unresolved failure-reporting and text dependencies](../foundation.md#failure-reporting-and-basic-data-services).
Its assertions, arena ownership, dictionaries, and strings do not remove those cycles, and replacing aggregate includes would not establish an accepted acyclic graph.
