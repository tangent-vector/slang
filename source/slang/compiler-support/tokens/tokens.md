# Tokens

Tokens provides the shared representation of lexical items and readable descriptions of their categories.
Lexing, preprocessing, parsing, diagnostics, and repository tools exchange these values without assigning those operations to the representation itself.

This is a proposed public leaf of [Compiler Support](../compiler-support.md).
The implementation remains in `old-source/compiler-core/`.

## Concepts

Compiler Support defines a [token](../compiler-support.md#concepts).
[Source Text](../source-text/source-text.md#concepts) describes source locations and their owning managers; [Names](../names/names.md#concepts) describes interned spellings and their lifetimes.

- A **token category**, represented by `TokenType`, distinguishes identifiers, literals, punctuation, whitespace, newlines, comments, invalid characters, and the end of input.
  The category table also includes an unknown value and the compiler's completion marker.
  Keyword recognition belongs to clients that interpret identifier spellings.
- A token's **spelling** is the sequence of bytes returned by `getContent()`, rather than the decoded value of a literal.
  The token stores a borrowed character pointer or a borrowed `Name*`, with a length in either case.
  `TokenFlag::Name` selects that storage form independently of the token category; a name-backed token need not be an identifier.
  The stored length is a 32-bit byte count, not a Unicode character count or a guaranteed extent in the original text.
- **Trivia** means whitespace, newlines, and comments.
  They have token categories in the same representation as other lexical items; a token has no attached leading or trailing trivia collection.
  Clients decide whether to retain, skip, or group those items.
- **Token flags** record lexical context, escaped-newline processing, and the spelling-storage choice.
  In lexer output, `AtStartOfLine` means that only whitespace or comments separate the token from input start or the preceding newline token.
  The lexer counts comments as whitespace for `AfterWhitespace` and sets both context flags at input start.
  It sets `ScrubbingNeeded` when it encounters an escaped newline; that flag remains on the token after the lexer removes the escaped newline from its spelling.
  Preprocessing can adjust the line-start flag on tokens it produces, so the flags describe producer-supplied context rather than a fresh interpretation of the location.

A token neither retains its spelling storage nor owns the manager that interprets its location.
Copying a token copies those references without extending their lifetimes.
Clients keep the referenced characters or name alive while reading its spelling, and the relevant source managers alive without resetting them while interpreting its location.
Name-backed storage also relies on the unchanged spelling described by Names; changing a name's public text can invalidate the token's stored length.
A default token has the unknown category, empty spelling, and an invalid location.
Producers can supply a location independently of the spelling's storage.

## Dependencies

- [Source Text](../source-text/source-text.md) supplies the compact source location stored in each token.
  Location interpretation and the ownership of files and views remain with that service.
- [Names](../names/names.md) supplies the optional name-backed spelling and access to its retained text.
  Token construction and assignment borrow a supplied name; they do not intern characters or retain the name.
- Foundation's [Text](../../foundation/text/text.md) supplies borrowed string slices for spelling input and access.
- [Scalar Types](../../foundation/base-types/scalar-types/scalar-types.md) supplies the index type used for spelling lengths.
- [Failure Reporting](../../foundation/failure-reporting/failure-reporting.md) supplies assertions for category-description lookup and spelling-storage preconditions.
- The product header `include/slang.h` supplies the compiler-specific inline annotation; the host C++ environment supplies fixed-width integers.
  These are declaration inputs rather than a dependency on the compiler implementation.

The broad `slang-basic.h` include does not make all Foundation services dependencies of Tokens.
Tokens does not require a lexer, parser, diagnostic sink, collection, allocator, or literal decoder to represent or inspect one value.

## Service Areas

Tokens has no child modules.
Its services are:

- **Category vocabulary:** the category list and the corresponding human-readable descriptions, including quoted punctuation spellings.
  `TokenTypeToString` returns a category description, not the spelling of a particular token.
- **Token values:** construction, copying, and access to locations and spelling, including the stored length and optional name.
  `setContent` and `setName` update the storage choice and length; the caller supplies the category, location, and other contextual flags.

Lexing chooses categories from input bytes and prepares their spellings in storage supplied by callers.
For example, the lexer can borrow a view's text directly, allocate text with escaped newlines removed in a caller-supplied arena, or intern an identifier in a supplied name pool.
The lifetime of those allocations remains separate from the returned token's lifetime.
The preprocessor likewise supplies storage and locations when synthesizing or pasting tokens.
Neither spelling length nor the name-storage flag proves that a token still denotes one unchanged span of an input file.

[Diagnostics](../diagnostics/diagnostics.md) uses category descriptions, spelling access, and token locations to format messages.
Reporting a diagnostic, recognizing or expanding a directive, classifying an identifier as a keyword, and deciding a literal's value remain with the respective clients.

## Existing Implementation Mapping

The following paths are relative to `old-source/compiler-core/`.
All declarations and definitions in these three files belong to Tokens:

- `slang-token.h` contains `TokenType`, `TokenFlags`, `TokenFlag`, and `Token`, including its storage union, constructors, and inline access and assignment operations.
- `slang-token-defs.h` contains the shared category and description table included by the enumeration and description lookup.
  It is a repeated-include C++ input, not a generated file or a lexer implementation.
- `slang-token.cpp` implements `TokenTypeToString` from that same table.

The adjacent `slang-lexer.{h,cpp}` remain assigned to [Lexing](../lexing/lexing.md), including `TokenList`, `TokenSpan`, `TokenReader`, their end-of-input conventions, and literal decoding.
The token-formatting and diagnostic-position overloads in `slang-diagnostic-sink.{h,cpp}` belong to Diagnostics.
The AST token serialization customizations in `old-source/slang/slang-serialize-ast.cpp` belong to Compiler: they store spelling text rather than pointers, omit the name-storage flag when writing, and intern the text when reading.
They must continue to use the same token type as the AST declarations.
The separate `Slang::Misc` token types and reader in `old-source/core/slang-token-reader.{h,cpp}`, JSON's `JSONToken`, and fiddle's token-with-trivia and delimiter-grouping structures are outside this assignment.

The current `compiler-core` static target compiles `slang-token.cpp` through its directory source selection and links Foundation's legacy `core` target.
Compiler variants and existing tools, including `slang-fiddle` and `slang-cpp-parser`, consume the declarations through that target.
The assigned files contain no platform-specific branch, runtime-loaded implementation, or product-export annotation, and the headers are not installed product headers.
The target's `fast_float` and SPIR-V inputs serve other services; the token implementation does not use them.
Any later migration must preserve the category order, representation and client access under the [root preservation contracts](../../slang.md#staged-migration-contracts); this document selects no namespace, binary-interface change, or replacement token mechanism.
