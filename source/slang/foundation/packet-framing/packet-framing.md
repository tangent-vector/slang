<!--
SPDX-FileCopyrightText: The Khronos Group, Inc.
SPDX-License-Identifier: CC-BY-4.0
-->

# Packet Framing

Packet Framing reads and writes length-delimited byte packets over streams supplied by its clients.
The existing format uses HTTP-style header fields followed by uninterpreted content bytes.
It serves JSON RPC and process communication without interpreting JSON, selecting a peer, or opening a network connection.
The implementation remains in `old-source/core/slang-http.{h,cpp}`.

Consider a client sending the three bytes `abc`.
`HTTPPacketConnection::write` first writes `Content-Length: 3\r\n\r\n`, then writes those three content bytes.
The receiver may obtain the header and content through several reads, or receive more than one packet in one read.
The framing service finds the header terminator, parses the content length, and exposes one complete content range at a time.
After the client consumes that range, the connection can interpret the next packet's bytes.
The client determines what the content means and whether an error calls for another peer or another request.

## Concepts

[Foundation](../foundation.md#concepts) defines streams, borrowed views, intrusive references, and string slices.
Packet Framing adds the following concepts:

- A **packet** consists of a header ending in the four bytes `\r\n\r\n` and a content range whose byte count comes from the parsed header.
  The content need not be text or end in a zero byte.
- A **header field** is a key/value pair separated by the first colon in a line.
  The parser trims horizontal whitespace from the key and value and retains every parsed pair in order.
  `Content-Length` and `Content-Type` have additional interpretations described below.
- The **read state** identifies whether a connection is waiting for a header, waiting for content, presenting a complete packet, closed between packets, or unable to read another packet after an error.
  `HTTPPacketConnection::ReadState` names those cases `Header`, `Content`, `Done`, `Closed`, and `Error`.
- A **read error** distinguishes a failed stream read, a complete header that failed parsing, and a stream ending inside a packet.
  `HTTPPacketConnection::ReadError` names those causes `Stream`, `InvalidHeader`, and `UnexpectedEnd`; `None` records no such cause.
  A wait timeout is separate from those causes.

The name `HTTPPacketConnection` denotes this stream-framing service rather than a complete HTTP client or server.
Request lines, status lines, HTTP methods, transfer encodings, and network connection management are outside the supplied service.

## Dependencies

- [Host I/O](../host-services/host-io/host-io.md) supplies `Stream`, `BufferedReadStream`, and `Process` timing operations.
  The connection reads buffered bytes, consumes completed ranges, and delegates writes to the supplied stream.
  Header waiting yields the current thread; packet waiting uses the host clock and a bounded sleep interval between polls.
  These timing calls do not create or retain a `Process` object.
- [Strings](../text/strings/strings.md) supplies borrowed character slices, comparison, trimming, substring search, and `StringBuilder` construction.
  [Text Operations](../text/text-operations/text-operations.md) supplies line traversal, content-type splitting, and integer parsing.
  Packet Framing assigns header meaning to the results of those character operations.
- [Memory](../memory/memory.md) supplies the header arena, the connection's `RefObject` base, and intrusive retention of its read and write streams.
  [Collections](../collections/collections.md) supplies the ordered header-field list, temporary split results, and the borrowed content array view.
- [Scalar Types](../base-types/scalar-types/scalar-types.md) supplies byte, count, index, and integer types.
  [Numeric Operations](../base-types/numeric-operations/numeric-operations.md) supplies `Math::Min` for the polling sleep interval.
  [Failure Reporting](../failure-reporting/failure-reporting.md) supplies assertions on accessor and state preconditions.
- The product support headers in `include/` supply `SlangResult`, result constants and tests, and `SLANG_RETURN_ON_FAIL` from `slang-com-helper.h`.
  The existing `slang-com-ptr.h` include does not establish use of COM object retention or a product object interface in this service.

JSON RPC is a client of Packet Framing rather than a dependency.
The lower-level text, collection, ownership, and failure services retain the [cycles recorded by Foundation](../foundation.md#unresolved-boundaries).
This leaf does not resolve those ancestor holds or establish that the entire dependency graph is ready for migration.

## Principal Service Areas

Packet Framing is a public ordinary leaf module within Foundation and introduces no child modules or new build artifact.
The complete `HTTPHeader` and `HTTPPacketConnection` declarations stay together because parsing the header, locating the content range, and consuming one packet form one framing task.
The standalone header operations remain available without constructing a packet connection.

### Header Parsing and Rendering

`HTTPHeader` provides terminator search, waiting for a complete header, parsing into fields, key lookup, and rendering into a string builder.
`parse` resets its output and copies the header text into that output's arena before creating slices into the copy.
It removes a final `\r\n\r\n` when present, but direct parsing does not require that terminator; the stream-reading paths locate the terminator before parsing.
Each parsed line must contain a colon.
A failed parse may leave the output with the copied text and the fields processed before failure; parsing is not transactional.

Header names and the `charset` parameter name are compared case-sensitively.
A recognized `Content-Length` is parsed through `StringUtil::parseInt` into a signed `Index` and rejected if parsing fails or the result is negative.
That parser accepts an optional sign and decimal or `0x`/`0X` hexadecimal digits, rejects overflow, and requires the full trimmed value to be consumed.
Missing `Content-Length` leaves the reset length at zero; repeated recognized lengths are allowed and the last successfully parsed value determines the content length.
`indexOfKey` instead finds the first exactly matching pair in the retained list.
No independent header-size, field-count, or content-size limit is enforced by Packet Framing.

For a recognized `Content-Type`, the parser takes the first semicolon-separated part as the MIME type and scans later parts for `charset=value`.
The parser records those slices without decoding the content or validating an encoding.
`append` always renders `Content-Length` first.
When either the MIME type or encoding is nonempty, it also renders `Content-Type`, supplying `text/plain` or `UTF-8` for the missing counterpart.
It then appends retained pairs other than exactly named `Content-Length` and `Content-Type`, followed by the empty line that ends the header.
The renderer therefore does not reproduce the original header bytes or every repeated recognized field.

The arena-backed slices produced by `parse` remain valid until the header is reset, parsed again, or destroyed.
The public fields also permit manually constructed headers whose slices borrow other storage; the header does not retain that storage.
The constructor initializes the arena but does not initialize the scalar content length; manual rendering requires a reset or an explicitly assigned length.
`read` consumes header bytes only after parsing succeeds, and `readHeaderText` waits without a timeout until it finds a terminator, encounters a stream failure, or sees the buffered stream end.
`BufferedReadStream::isEnd` remains false while buffered bytes remain, so standalone header waiting can keep polling after the underlying stream ends with an incomplete header.
The packet connection's separate handling of the backing stream's end does not apply to that standalone helper.

### Incremental Packet Reading

`HTTPPacketConnection` retains the supplied buffered read stream and write stream and starts in `Header`.
`update` returns immediately in `Closed` or `Error`; otherwise, it asks the buffered stream for more bytes.
In `Header`, it searches for a complete header and consumes the header only after parsing succeeds.
The connection then enters `Content` and enters `Done` once at least the declared number of content bytes is buffered.
Surplus bytes remain buffered for subsequent packets.
A complete final packet reaches `Done` before the implementation considers end-of-stream.
If the backing stream has ended with no bytes pending in `Header`, the connection enters `Closed`; an incomplete header or content range instead produces `UnexpectedEnd` in `Error`.
A failed buffered-stream update produces `Stream`, and a failed complete-header parse produces `InvalidHeader`.

`getReadHeader` requires `Content` or `Done`, and `getContent` requires `Done`.
The content view borrows exactly the declared range from the buffered stream.
`consumeContent` consumes that range and returns the connection to `Header`; clients can no longer use the previous packet through those accessors.
A later buffered-stream update can move or reallocate bytes, including when `update` is called while the connection is already `Done`.
Clients therefore finish using a content view before further updates or consumption and coordinate access to the supplied streams themselves.
The connection provides no synchronization or independent copy of received content.

`waitForResult` polls the same state machine and returns success only when the read state is `Done`.
A nonnegative timeout limits polling using the host clock; a negative timeout selects no deadline.
The wait does not make a blocking underlying read interruptible.
Timeout leaves the partial packet and read state available for another update or wait.
Closure and errors also return failure, so callers use the read state and error cause to distinguish the outcomes.
The current implementation resets its stored `SlangResult` at the start of every wait even when already in `Error`; the retained read state and error cause remain separate from that result.
Nonblocking updates require a nonblocking underlying read stream.

### Packet Writing

`write` constructs a length-only header and makes one stream write for the rendered header followed by one for the supplied content.
It stops at the first failed write and propagates that result without changing the read state.
The stream interface reports a result rather than a partial-write count; the framing layer provides no continuation buffer, rollback, retry, flush, or atomic-packet guarantee.
The current file and process-pipe providers report short writes as failure, potentially after transferring a prefix.
The caller supplies valid content storage for the duration of the call and chooses streams whose blocking behavior is suitable.

## Existing Implementation Mapping

| Existing Location                | Assigned Responsibility                                                                                                                                                                         |
| -------------------------------- | ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| `old-source/core/slang-http.h`   | Complete `HTTPHeader`, nested `Pair`, inline key lookup, and complete `HTTPPacketConnection`, including its nested state/error enums, accessors, retained streams, and protected state helpers. |
| `old-source/core/slang-http.cpp` | Header constants and all header operations; packet construction, header/content progression, reads, consumption, writes, and the private `SleepState` polling helper.                           |

No declaration in either assigned file belongs to JSON RPC or network establishment.
The JSON RPC parser, native-value conversion, error-message production, process lifetime management, and retry policy remain with their existing clients.
For example, `JSONRPCConnection::sendRPC` serializes JSON before calling `write`, and `tryReadMessage` parses the borrowed content before consuming the packet.
`JSONRPCConnection::_updateReadErrorFromTransport` assigns its own RPC-level meaning to the framing errors.
`TestContext` and the process tests obtain child-process or standard streams before constructing a packet connection; Packet Framing does not perform those caller operations.

`old-source/core/CMakeLists.txt` includes the implementation in static `core` through `slang_glob_sources`.
Enabled `slang-rt` separately compiles the same core implementation directory through `EXTRA_SOURCE_DIRS` in `old-source/slang-rt/CMakeLists.txt`.
A future port preserves both consumers and their export settings, the existing declaration names and signatures, and the [root's staged identity contracts](../../slang.md#staged-migration-contracts).
The existing `HTTPPacketConnectionReflect`, `HTTPPacketConnectionPeerCrash`, `HTTPHeaderContentLengthValidation`, and `JSONRPCReadErrorClassification` tests identify relevant preservation coverage; this architectural refinement does not execute or extend them.

## Preservation Constraints

The case-sensitive field recognition, permissive missing and repeated length fields, numeric syntax, unbounded buffering, partial parse mutation, standalone header waiting at end-of-stream, wait-result reset, and partial-write behavior above describe the current implementation.
They are not stricter HTTP-validation guarantees or behavior changes selected by this decomposition.
The existing header comments also refer to `waitForContent` and `getHeader`, while the actual API provides `waitForResult` and `getReadHeader`.
Inherited behavior, declaration names, and missing or inconsistent implementation explanations remain deferred under the trial's phase-two `TODO(tfoley-refactor)` policy.
No code port, protocol repair, new transport abstraction, or change to caller retry policy is selected here.
