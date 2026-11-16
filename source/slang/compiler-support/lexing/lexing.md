# Lexing

Lexing turns program text into tokens and provides token-sequence reading and literal decoding.
Clients can scan one item at a time, collect tokens with selected trivia, read a borrowed sequence with lookahead, or decode the spelling of a literal.
Preprocessing, grammar recognition, declaration lookup, and construction of typed expressions remain with callers.

This is a proposed public leaf of [Compiler Support](../compiler-support.md).
Its implementation remains in `old-source/compiler-core/`; no code has moved into this directory.

## Concepts

[Tokens](../tokens/tokens.md#concepts) defines tokens, spelling, trivia, and token flags.
[Source Text](../source-text/source-text.md#concepts) defines files, views, locations, and their owners; [Names](../names/names.md#concepts) defines interned spellings.
Lexing uses those representations without extending their lifetimes.

- A **lexer** (`Lexer`) reads the UTF-8 bytes of a supplied source view and maintains a byte cursor and the lexical-context flags for its next result.
  Locations use the view's starting location plus the original byte offset, even when a returned spelling has been changed.
  The lexer borrows its view, optional sink and name pool, and spelling arena; callers keep those inputs valid while scanning uses them.
- A **token list** (`TokenList`) owns a contiguous sequence of token records.
  A completed list is nonempty and stores a final end-of-file token.
  Its `end()` points at that stored token, rather than past the allocation; iteration from `begin()` to `end()` excludes the terminator.
  Owning the records does not retain their spelling storage or source managers.
- A **token span** (`TokenSpan`) borrows a begin/end pair within token storage.
  A **token reader** (`TokenReader`) adds a cursor and a mutable cached lookahead token to such a range.
  A non-null end pointer must identify a readable token, because the reader copies that token and changes its cached category to end-of-file when the cursor reaches the range end.
  An empty default reader instead synthesizes a locationless end-of-file token.
- A **parsing cursor** (`TokenReader::ParsingCursor`) saves a reader's current token pointer and cached token.
  Restoring it changes those two values without changing the reader's range end or restoring parser state, diagnostics, or mutations to the underlying storage.
  Saved cursors therefore require the same range and still-valid token storage.
  Cursor equality compares token pointers, without comparing the cached token.
- A **literal value** is the result of interpreting a literal token's spelling, rather than the spelling bytes themselves.
  The shared integer and floating-point storage types do not by themselves determine the type of an expression in the program.

## Dependencies

- [Source Text](../source-text/source-text.md) supplies the view's text and location range, and temporary files and views for the diagnostic-highlighting adapter.
- [Tokens](../tokens/tokens.md) supplies lexical categories, flags, and the token representation returned by scanning and sequence reading.
- [Names](../names/names.md) optionally interns identifier and completion-marker spellings into a caller-supplied pool.
- [Diagnostics](../diagnostics/diagnostics.md) supplies reporting and the shared lexer-message descriptions.
  Diagnostics accepts a highlighting callback; it does not call Lexing directly.
- Foundation's [Text](../../foundation/text/text.md) supplies strings, builders, slices, byte-sequence formatting, and Unicode decoding and encoding.
- [Memory](../../foundation/memory/memory.md) supplies the caller-owned arena used for spellings with escaped newlines removed; [Collections](../../foundation/collections/collections.md) supplies token-list storage.
- [Base Types](../../foundation/base-types/base-types.md) supplies scalar and portability conventions; [Failure Reporting](../../foundation/failure-reporting/failure-reporting.md) supplies assertions for sequence and decoding contracts.
- The external `fast_float` library converts decimal floating-point spellings.
  The host C++ environment supplies fixed-width integers, bit operations, floating-point limits, and arithmetic used for hexadecimal conversion and precision reduction.

The implementation does not require compiler AST declarations, a semantic visitor, a language-mode object, or generated compiler metadata.
The broad Foundation headers and unused string-escaping include do not establish additional service dependencies.

## Service Areas

Lexing has no child modules.
Scanning, token-sequence reading, and literal decoding share token and spelling contracts; their current size and clients do not require another level of module ownership.

### Scanning Text

Single-item scanning recognizes identifiers, numeric and quoted literals, punctuation, whitespace, comments, newlines, and invalid input.
It also recognizes the compiler's `#?` completion marker.
The scanner has one shared set of rules rather than a language-mode switch.
Identifier scanning accepts non-ASCII character-reader results without consulting declaration scopes or applying a language-specific identifier table.
The parser decides which identifier spellings denote keywords or declared syntax.

Scanning removes backslash-escaped newlines before interpreting the next character and records that processing in the token flags.
Consider this example:

```slang
int va\
lue = 1;
```

The identifier token has spelling `value`, but its location still identifies the `v` in the original view.
The removed newline occupies bytes in that view, so the stored spelling length is shorter than the original token extent.
The arena supplies the changed spelling; when a name pool is supplied, interning then supplies the identifier's name-backed spelling.
Without either change, the token borrows characters directly from the view.
The caller keeps the view's contents, any arena allocations, and any referenced names alive for as long as the corresponding tokens are read.

Collection helpers select which scanned records to keep.
`lexAllSemanticTokens` skips all trivia, `lexAllMarkupTokens` keeps comments but skips whitespace and newlines, and `lexAllTokens` keeps every scanned record.
Each helper includes a final end-of-file token.
Even the last form contains processed spellings and lexical recovery results rather than a lossless copy of the input bytes.

The scanner reports lexical failures and continues with tokens where its existing recovery permits.
For example, an unterminated ordinary string produces a literal token that ends before a closing quote, whereas an unrecognized character produces an invalid token.
When a recognized leading byte starts a sequence that the UTF-8 decoder rejects, lookahead treats the rejected sequence as a space.
Consuming that sequence uses the stored sink for the invalid-sequence diagnostic.
Other malformed input can follow a different path: an isolated `0x80` byte followed by an ASCII space can be consumed as an identifier without that diagnostic.
Encountering a zero byte or a truncated multibyte sequence can move the byte cursor to the end of the input.
The implementation has no complete malformed-input validation guarantee: reaching end-of-file inside a block comment currently has no diagnostic.

The preprocessor chooses when to suppress lexical diagnostics while skipping conditional branches.
The suppression flag affects reporting sites that call `getDiagnosticSink`; the invalid-sequence report in `_advance` uses the stored sink directly and bypasses that flag.
The local reporting helper also limits reporting according to the supplied sink's accumulated error count.
These existing behaviors do not implement preprocessor conditionals or determine whether compilation succeeds.

The line-end search helper reads physical newline sequences in the same view without advancing the lexer cursor.
Its caller supplies a location in that view and a positive count of line endings to find; the count is reduced as endings are encountered.
The helper returns an invalid location if it cannot find the requested ending.
The preprocessor combines that operation with its own input-stream ordering.

The diagnostic-highlighting adapter constructs temporary lexer storage, scans the first item, and returns a borrowed slice of the supplied input.
It uses the scanned token's stored spelling length to size that slice and returns an empty slice for invalid or out-of-range results.
The slice does not refer to the adapter's temporary arena or name pool.
Because spelling length can differ from physical extent, the adapter does not establish the original extent of a token containing escaped newlines.

### Reading Token Sequences

The reader exposes one-token lookahead, advancement, location access, and cursor save/restore without recognizing grammar or skipping trivia.
Advancement returns the cached token and stops moving once the cursor reaches the range end.
For a subrange, the readable token at its end need not itself be an end-of-file token: only the cached category is changed, and the other token fields remain available.
Preprocessor macro-argument reading uses this convention to read part of a larger allocation.
Clients must preserve the token allocation and range while a reader or saved cursor refers to it.

The mutable cached token also allows parser policy without changing the sequence storage.
For example, the compiler parser handles a scanned `>>` when closing nested generic arguments by changing its cached lookahead to `>` and adjusting its location.
Choosing that interpretation, matching delimiters, backtracking over grammar, and recovering from syntax errors belong to the parser rather than this reader.

### Decoding Literals

Literal decoding is explicit and separate from scanning.
For example, the token for `"a\n"` still contains quotes and a backslash followed by `n`; string decoding produces the bytes for `a` followed by a newline.
String decoding also handles raw-string delimiters and returns a retained string.
Character decoding interprets one code point or escape, returns `-1` for failure, and uses the optional diagnostic sink for reporting.
Filename decoding only strips the first and last delimiters; it preserves backslashes for paths such as Windows include filenames.

Integer decoding returns the accumulated value, with optional borrowed suffix, decimal-base classification, and overflow result.
The compiler parser interprets integer suffixes and selects an expression type.
Floating-point decoding instead classifies the supported half/float/double suffix spellings itself and returns a value held in a `double`, together with malformed-significand, malformed-suffix, range, and precision information.
Decimal conversion uses `fast_float`; hexadecimal conversion and precision reduction are implemented locally.
The existing decimal and hexadecimal paths differ in their rounding and truncation behavior; migration must preserve those numerical choices.
The compiler parser translates those results into expression types and its floating-point diagnostics.
The returned suffix and error slices borrow token spelling storage.
These helpers serve existing literal-token spellings and recovery paths; they are not interchangeable validators for arbitrary byte slices.

## Existing Implementation Mapping

All declarations and definitions in `old-source/compiler-core/slang-lexer.h` and `slang-lexer.cpp` belong to Lexing.
They include token lists, spans, readers and saved cursors; lexer state, character reading, scanning and collection; line-end search and diagnostic highlighting; literal storage aliases, floating-point result categories, and all literal-decoding helpers.
The raw-string value helper defined only in the implementation file belongs to the same decoding service.

Token categories and individual token values remain with [Tokens](../tokens/tokens.md).
The lexer diagnostic catalog and its core lookup assembly remain with [Diagnostics](../diagnostics/diagnostics.md).
The separate `Slang::Misc` tokenizer in `old-source/core/slang-token-reader.{h,cpp}`, JSON's lexer, and fiddle's trivia grouping and delimiter matching are outside this assignment.
Compiler owns preprocessing streams, macro expansion and token pasting, parser grammar and recovery, language-version decisions, AST construction, and semantic lookup.
For pasted tokens, the preprocessor creates the text and its source view, supplies the initiating location, invokes Lexing, and decides whether the result is a valid paste.
Lexing does not infer macro provenance from a spelling.

The current `compiler-core` static target compiles `slang-lexer.cpp` through directory source selection and links Foundation's legacy `core` target and `fast_float`.
Compiler variants, documentation extraction, and repository tools consume these services through that target.
`slang-fiddle` uses all tokens before performing its own trivia grouping; `slang-capability-generator` uses comment-preserving tokens; `slang-cpp-parser` uses tokens without trivia.
Lookup and SPIR-V embedding generators also supply the highlighting adapter to diagnostics.
Those generators and the compiler's bootstrap consume Lexing; their generated compiler outputs are not inputs required to compile this lexer.
This leaf does not resolve the root's [compiler/bootstrap construction questions](../../slang.md#unresolved-architectural-dependencies).

## Input-Storage Question Before Migration

The escaped-newline lookahead in `_peek` and `_advance` assumes readable null-terminated storage beyond the view's logical content endpoint.
Source Text can retain an arbitrary UTF-8 blob directly, and its encoding-conversion path does not explicitly append a terminator.
A source view alone therefore does not establish the available readable tail required by those paths.
Compiler Support and Source Text need an explicit disposition of that existing input-storage contract before Lexing migration; this document does not silently strengthen Source Text's guarantee or change the scanner.

Any later migration must preserve existing consumers, spelling and location lifetimes, token-range conventions, and numerical behavior under the [root preservation contracts](../../slang.md#staged-migration-contracts).
Phase-one refinement does not establish migration readiness or select a replacement parsing architecture.
