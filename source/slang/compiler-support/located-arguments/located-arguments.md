# Located Arguments

Located Arguments stores command arguments with locations for diagnostic presentation and provides sequential reading of those arguments.
Clients interpret option spellings, validate values, and decide how arguments change their requests.

This is a proposed public leaf of [Compiler Support](../compiler-support.md).
The implementation remains in `old-source/compiler-core/`; no implementation has moved into this directory.

## Concepts

[Source Text](../source-text/source-text.md#concepts) defines source files, views, locations, and their manager lifetimes.
[Foundation](../../foundation/foundation.md#concepts) defines strings and intrusive references.

- A **located argument** (`CommandLineArg`) pairs a retained `String` value with a `SourceLoc` for diagnostic presentation.
  The value is the argument supplied by the caller, rather than its quoted display spelling.
  Copying an argument retains its string storage but does not retain the manager needed to interpret its location.
- An **argument context** (`CommandLineContext`) owns the `SourceManager` used to represent command arguments as text.
  Argument lists can share a context, allowing copied arguments to keep their original locations without rebuilding the displayed text.
- An **argument list** (`CommandLineArgs`) owns an ordered, mutable sequence of located arguments and retains an optional argument context.
  A list created without a context can hold supplied arguments or deserialized values; constructing diagnostic text from an argument array requires a context.
- An **argument reader** (`CommandLineReader`) borrows a mutable argument list and an optional diagnostic sink and records the index at which reading continues.
  The sink is required when the reader must diagnose a missing argument.

## Dependencies

- [Source Text](../source-text/source-text.md) supplies compact locations, command-line path information, source files and views, and the manager that owns and interprets them.
- [Diagnostics](../diagnostics/diagnostics.md) supplies the sink and the shared missing-option-argument description used by required reads.
- Foundation's [Strings](../../foundation/text/strings/strings.md) supplies retained values, character views, comparison, and builders.
  [Text Operations](../../foundation/text/text-operations/text-operations.md) supplies newline splitting, and [Escaping](../../foundation/text/escaping/escaping.md) supplies quoted display construction.
- [Host I/O](../../foundation/host-services/host-io/host-io.md) supplies `Process::getEscapeHandler`, which selects the process-argument display convention.
  Using that selector does not launch a process.
- [Memory](../../foundation/memory/memory.md) supplies intrusive retention for the context, and [Collections](../../foundation/collections/collections.md) supplies argument and split-result lists.
  [Base Types](../../foundation/base-types/base-types.md) supplies count, index, and portability conventions; [Failure Reporting](../../foundation/failure-reporting/failure-reporting.md) supplies assertions for reader and location preconditions.
- Product headers under `include/` supply result codes and the optional `ISlangFileSystemExt` contract passed to the context's manager.
  Using that contract does not assign the product file-system interface or a concrete file-system implementation to this module.

The `slang-basic.h` aggregate does not establish dependencies on every Foundation service.
The `slang-type-text-util.h` include serves the separate downstream-tool policy in the same implementation file.
[Command Options](../../foundation/command-options/command-options.md) supplies catalogs, lookup, and help composition to clients; the located-argument services do not use those catalogs.

## Service Areas

Located Arguments has no child modules.
Its principal services share the same argument and context contracts:

- **Argument storage and diagnostic text:** `CommandLineArgs::setArgs` replaces the list from an argument array that excludes the executable name.
  It copies the argument values and creates a source file and view from their display rendering, with a space after each argument.
  Each argument's location identifies the beginning of its displayed spelling.
- **Sequential reading and mutation:** the reader peeks, consumes, repositions, or removes the current argument without interpreting option syntax.
  A required read consumes the next argument when present; otherwise it reports the missing value using the preceding argument as the option name and returns failure.
- **Value comparison and interchange:** ordered comparison checks argument values, and the existing serialization operations write and read newline-separated values without locations.
  Compiler clients use that representation for stored downstream-tool arguments; selecting the tool and forwarding its arguments remain client policy.

## Storage and Reading Contracts

The context's manager owns the files and views created by `setArgs`.
Replacing or editing a list does not update or discard previously constructed diagnostic text.
The context constructor skips a large initial location range to reduce accidental confusion with program-text locations, but that convention does not make locations globally unique across managers.
The relevant context must remain alive, without resetting its manager, while its locations are interpreted.
Retaining a list's context does not retain a different context from which a caller copied an argument through `add` or direct list access.

Diagnostic display construction uses the process-argument escaping handler without propagating its failure result.
The current Windows and Unix providers select the space-quoting handler, which rejects embedded double quotes when quoting a value containing a space.
For example, `setArgs` keeps the value `a "b"` but displays `""` for that argument.
The displayed text therefore does not promise a lossless rendering of every argument value.

The reader retains neither its list nor its sink.
Its references to arguments and values additionally depend on the list's storage and mutations.
Removing the current argument leaves the index unchanged so that the following argument becomes current.
Peeking a value, consuming, or removing requires a current argument; repositioning permits the end index.
`peekLoc` returns an invalid location at the end, including when a required read diagnoses a missing value.
That failure path requires a preceding option and a usable sink; it does not recover the preceding option's location.

Serialization appends a newline after every value without escaping embedded newlines.
Deserialization appends to the existing list and assigns invalid locations; it neither clears the list nor creates diagnostic text.
The format therefore preserves argument boundaries only for values without embedded newlines and does not preserve location context.

## Existing Implementation Mapping

The paths below are relative to `old-source/compiler-core/` and are partial-file assignments.
The complete header and implementation also contain the separate `DownstreamArgs` service.

| Existing Implementation       | Assigned Responsibility                                                                                                                                                                                                                |
| ----------------------------- | -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| `slang-command-line-args.h`   | The complete `CommandLineArg`, `CommandLineContext`, `CommandLineArgs`, and `CommandLineReader` declarations: their data, constructors, accessors, iteration, reader mutation and required reads, comparison, and value serialization. |
| `slang-command-line-args.cpp` | All definitions of `CommandLineArgs` and `CommandLineReader`, including argument display construction, value comparison, newline serialization and deserialization, previous-value lookup, and both required-read overloads.           |

`DownstreamArgs`, including its flags, entries, context retention, name registration and lookup, and `stripDownstreamArgs`, belongs to [Compiler's Downstream Tools](../../compiler/downstream-tools/downstream-tools.md) under [Compiler Support's assignment](../compiler-support.md#existing-implementation-mapping).
That service interprets the `-X` forwarding syntax and groups arguments by tool name while using this module's values and reader.
Its use of compilation-tool names does not introduce a Compilation Vocabulary dependency into Located Arguments.

`OptionsParser` in `old-source/slang/slang-options.cpp` uses a context retained by `Linkage`, constructs a diagnostic sink for that context, and applies compiler option policy above these services.
`CompilerOptionSet::getDownstreamArgs` in `old-source/slang/slang-compiler-options.cpp` deserializes stored values without a context.
`tools/render-test/options.cpp` separately uses the same argument and reader services for its own options.
`tools/slang-unit-test/unit-test-command-line-args.cpp` exercises argument construction, comparison, and downstream grouping; those clients and their option policies remain outside this leaf.

The current `compiler-core` static target discovers and compiles the complete implementation file and links legacy `core`.
Compiler variants, render-test, and the unit-test module obtain these internal definitions through that build.
The four assigned types have no export annotations of their own, but their members and signatures use existing Foundation, Source Text, and Diagnostics types.
Any later migration must preserve client access, retained-context behavior, and the [root's binary-interface contracts](../../slang.md#staged-migration-contracts).
The shared file still needs separation from downstream-tool policy before it can follow the proposed ownership, and Foundation's recorded dependency cycles remain unresolved.
