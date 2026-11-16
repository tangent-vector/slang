# Documentation Comments

Documentation Comments finds and extracts documentation text near declaration locations supplied by a caller.
Clients select the declarations and the search category for each location; this service returns text, a visibility value, and the input index needed to associate the result with its declaration.
Selecting AST declarations, attaching documentation to them, interpreting documentation commands, and producing pages or editor text remain with [Compiler](../../compiler/compiler.md).

This is a proposed public leaf of [Compiler Support](../compiler-support.md).
The implementation remains in `old-source/compiler-core/`; no implementation has moved into this directory.

## Concepts

[Source Text](../source-text/source-text.md#concepts) defines source files, views, locations, and their lifetimes.
[Tokens](../tokens/tokens.md#concepts) defines lexical categories, flags, and spelling; [Lexing](../lexing/lexing.md#concepts) defines token lists.

- **Documentation text** is the text selected from comment tokens and normalized by the [extraction rules](#extract-text-and-whitespace).
  The extractor calls that text markup, but does not interpret Markdown or Doxygen commands.
- A **search item** (`SearchItemInput`) pairs a source location with a caller-selected `SearchStyle`.
  The extractor looks for an exact token start at the location and uses the style to choose which lexical searches to try around that token.
  The extractor does not infer the declaration kind from the program.
- A **comment type** (`MarkupType`) classifies the comment's lexical marker and whether it denotes documentation before or after a declaration.
  That classification is separate from the search style: a variable search can try both directions, but each candidate must have the matching comment type.
- **Documentation visibility** (`MarkupVisibility`) is a lexical label with `Public`, `Internal`, and `Hidden` values.
  It is independent of the declaration's language access modifiers and is reported even when no documentation comment is found.
  Each consumer decides how to use the label.

## Dependencies

- [Source Text](../source-text/source-text.md) supplies manager lookup, file contents, view-relative locations, and physical line access.
- [Lexing](../lexing/lexing.md) supplies comment-preserving token collection and its token-list representation.
  [Tokens](../tokens/tokens.md) supplies the categories, flags, locations, and spellings inspected by the searches.
- [Names](../names/names.md) supplies the temporary pool used when the lexer interns identifiers.
- [Diagnostics](../diagnostics/diagnostics.md) supplies the caller's sink, to which the lexer can report failures during extraction.
  The extractor has no documentation-specific diagnostic catalog.
- Foundation's [Text](../../foundation/text/text.md), [Collections](../../foundation/collections/collections.md), and [Memory](../../foundation/memory/memory.md) supply strings, slices, line splitting, output lists, sorting, and temporary spelling storage.
  [Base Types](../../foundation/base-types/base-types.md) and [Failure Reporting](../../foundation/failure-reporting/failure-reporting.md) supply scalar conventions and assertions.
- The existing product/base contracts supply `SlangResult` and its result constants.

Extraction requires neither compiler AST declarations nor a preprocessor.
It scans the supplied file text, including text within inactive conditional branches.
The extractor does not expand macros or consult line directives and source maps when finding comments or calculating physical line numbers.
The broad basic header does not imply dependencies on file I/O, processes, external compilers, or generated compiler metadata.

## Service Areas

Documentation Comments has no child modules.
Its services combine batch location handling, lexical comment selection, text extraction, and visibility assignment.
These operations share the same file scan and its temporary token storage.

### Batch Locations and Results

`DocMarkupExtractor::extract` accepts an array of search items, a source manager, an optional diagnostic sink, and caller-owned lists for views and results.
It sorts the inputs by raw location, resolves each searchable location through `SourceManager::findSourceView`, and groups the resolved locations by `SourceFile*`.
That lookup does not search parent managers recursively.
An unresolved location or `SearchStyle::None` produces an output with `viewIndex == -1` and `Public` visibility.

The view list is not cleared.
The extractor reuses an existing entry for the same file or appends a discovered view, so several input views of one file share one output view.
It converts each location to a file offset, then searches using that offset in the selected output view's location range.
The returned view therefore need not be the input location's original view.
The extractor lexes each selected file once for the batch, retaining comments and the end-of-file token while discarding whitespace and newline records.

There is one result per input.
Results are sorted by output view index and then file offset, with unresolved or disabled items before resolved items.
Existing view entries affect that order, and ordering equal keys is not stable.
`inputIndex` identifies the original input; `viewIndex` identifies the selected view and remains nonnegative for a resolved item even if no comment is found.
This grouping is not a global order through nested includes.

Result text is a retained `String`, independent of the temporary tokens, name pool, and spelling arena destroyed after extraction.
The returned `SourceView*` values are borrowed and remain subject to their managers' lifetimes and reset rules.
The extractor resizes the result list and assigns each result's indexes and visibility, but assigns `text` only when extraction succeeds for that item.
A new result list therefore has empty text for misses; reused result storage can retain text from an earlier call on a miss.

### Select Comments Around a Location

The search-style choices below describe the current lexical searches.
Every style that tries two locations prefers documentation before the declaration.

| Search Style   | Locations Tried                                                                                 |
| -------------- | ----------------------------------------------------------------------------------------------- |
| `None`         | No search.                                                                                      |
| `Before`       | Before the supplied token.                                                                      |
| `Function`     | The same before search.                                                                         |
| `Attribute`    | A before search starting two token indexes earlier, without parsing the attribute's syntax.     |
| `EnumCase`     | Before, then after with a comma as a possible stopping point; a closing brace stops the search. |
| `Param`        | Before, then after with a comma or closing parenthesis as a possible stopping point.            |
| `Variable`     | Before, then after with a semicolon as a possible stopping point.                               |
| `GenericParam` | Before, then after with a comma or greater-than token as a possible stopping point.             |

The scans can pass over other tokens and use one nesting count for opening and closing delimiters, with special handling for braces and the punctuation in the table.
They do not parse declaration extents or match delimiter kinds as a parser would.
When an after search stops at punctuation, the next retained token must itself be a suitable after comment.
The before scan stops at a semicolon, a comma at nesting depth zero, a closing brace, or an unmatched opening delimiter.
The first selected comment need not be on the declaration's line or the immediately preceding line; the implementation's initial line-proximity check is disabled.
If more than one search location has documentation, the first match wins and the duplicate-warning TODO emits no diagnostic.

`findMarkupType` recognizes the following markers independently of whether an ordinary-comment search is enabled:

| Comment Marker | Classification                                       |
| -------------- | ---------------------------------------------------- |
| `/**`, `/*!`   | Block documentation before a declaration.            |
| `/**<`, `/*!<` | Block documentation after a declaration.             |
| `///`, `//!`   | Line documentation before a declaration.             |
| `///<`, `//!<` | Line documentation after a declaration.              |
| Ordinary `/*`  | Ordinary block comment before a declaration.         |
| Ordinary `//`  | Ordinary line comment before or after a declaration. |

For ordinary line comments, `findMarkupType` selects before when `TokenFlag::AtStartOfLine` is set and after otherwise.
That lexer flag is preserved through leading whitespace and comments, so it does not require column zero.
The extractor disables ordinary block-before and line-before candidates by default; `setSearchInOrdinaryComments` controls those two categories.
Disabling that setting does not exclude ordinary line-after comments, which can still be extracted.
Block documentation occupies one token; a line group extends over consecutive physical lines only while the adjacent retained tokens have exactly the same comment type.

The location lookup requires an exact token-start match and currently fails to recognize token index zero.
Comment selection also rejects a candidate beginning at token index zero.
Consequently, a comment at the beginning of a file can be missed even when the declaration's location is otherwise found.
These are limitations of the existing searches, rather than additional restrictions on programs accepted by Slang.

### Extract Text and Whitespace

Consider this example, with a `Variable` search item at the `value` token:

```slang
int earlier;
/// First line.
///     Indented line.
int value;
```

The backward search selects the two `///` tokens.
The extractor removes the marker from each line, finds one common leading ASCII space, and returns `First line.\n    Indented line.\n`.
The declaration association remains the caller's responsibility.

For a line group, extraction drops a whitespace-only first or last candidate line and removes the minimum leading ASCII-space count from every surviving line.
An interior blank line participates in that minimum; it can prevent removal of spaces from the other lines.
Tabs do not count as indentation, trailing spaces are preserved, and each emitted line receives a final LF.
The whitespace-only endpoint test does not repeatedly discard all empty lines at each end.

For a block comment, extraction splits the token spelling into lines and removes the final `*/`.
If the first spelling slice still points into the original physical line, extraction also removes the opening marker and uses the comment's byte column as the maximum number of leading ASCII spaces to remove from later lines.
That pointer test can fail when lexing has allocated a spelling with escaped newlines removed; the opening marker is then left in the text and later lines have no indentation limit.
Whitespace-only first and last block lines are dropped, each remaining line receives LF, and leading `*` decoration is preserved.
Neither block extraction nor line extraction expands tabs or normalizes arbitrary decoration.
The public `removeStart` helper performs only matching opening-marker removal and returns a slice borrowing the supplied comment text.

### Assign Visibility and Report Failures

Each file starts with `Public` visibility.
The scan recognizes line comments beginning exactly `//@` and compares their trimmed suffix with `public:`, `internal:`, `hidden:`, or `private:`.
The last two spellings both select `Hidden`; unrecognized suffixes leave the current value unchanged.
A change applies from the directive's physical line onward in that file, without AST-scope nesting or preprocessing-condition interpretation.
The declaration location's physical line selects the result's visibility, independently of the selected comment's line.
The extractor returns all visibility values; filtering belongs to consumers.
For example, `DocMarkdownWriter::isVisible` currently excludes both `Internal` and `Hidden` entries.

An absent comment is a normal result and does not fail the batch.
Lexing can report diagnostics to the supplied sink while `extract` still returns `SLANG_OK`, because extraction does not inspect the sink's error count.
A failure returned by the selected search or text-extraction step propagates through the result code after any earlier changes to the view and result lists; the operation does not roll those changes back.
Malformed text has no complete validation guarantee: reaching end-of-file in a block comment currently has no lexer diagnostic, while extraction of a selected block assumes and asserts the closing delimiter.

## Existing Implementation Mapping

The assignments below describe ownership, not completed file or build changes.

- All of [slang-doc-extractor.h](../../../../old-source/compiler-core/slang-doc-extractor.h) belongs here: `MarkupVisibility`, `DocMarkupExtractor`, its search and comment classifications, input/output records, public search records, and public classification and prefix-removal helpers.
- All of [slang-doc-extractor.cpp](../../../../old-source/compiler-core/slang-doc-extractor.cpp) belongs here: batching and view selection, token lookup, lexical searches, text normalization, line visibility, and their private helpers.

[slang-doc-ast.h](../../../../old-source/slang/slang-doc-ast.h) and [slang-doc-ast.cpp](../../../../old-source/slang/slang-doc-ast.cpp) remain wholly Compiler-owned.
`ASTMarkupUtil` selects declarations and search styles, supplies their locations, and uses `inputIndex` to associate extraction results with `ASTMarkup` entries.
`ASTMarkup::attachToAST` and the `MarkupEntry` declaration in `slang-ast-support-types.h` likewise remain with Compiler, as do the Markdown writer and editor presentation.
These callers do not make AST declarations a dependency of extraction.

The current `compiler-core` static target selects `slang-doc-extractor.cpp` through the directory glob in `slang_add_target` and links Foundation's legacy `core` target.
Compiler variants link `compiler-core`; `ASTMarkupUtil::extract` is their direct extraction client.
Built-in compilation extracts and attaches documentation, and `Session::writeCoreModuleDoc` extracts documentation before invoking the Markdown writer.
`WorkspaceVersion::getOrCreateMarkupAST` enables ordinary-comment searching and caches AST markup used by the language server's documentation formatter.
The static `slang-cpp-parser` tool library also links `compiler-core` and uses `MarkupVisibility` in its node record; it does not call the extractor.
The extractor include in `slang-fiddle-main.cpp` is disabled by `#if 0` and does not establish an active extraction client.

The current target's `fast_float` and SPIR-V inputs serve other services; extraction uses neither directly.
The assigned files contain no platform-specific branch, runtime-loaded implementation, generated input, or product-export annotation, and their headers are not installed product headers.
Any later migration must preserve the existing consumers, declaration names, and behavior under the [root preservation contracts](../../slang.md#staged-migration-contracts).
This document selects no new library or namespace and makes no code changes to the search or normalization limitations described above.
