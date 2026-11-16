#ifndef SLANG_FOUNDATION_TOOL_STATUS_CODES_H
#define SLANG_FOUNDATION_TOOL_STATUS_CODES_H

// Tool Status Codes declares the outcome vocabulary shared by tool invocation paths.
// We define the enum in `Slang` so existing tool signatures and the module interface use the
// same type, then reexport that definition in the leaf namespace below.

namespace Slang
{

/// A status reported for a Slang executable tool or its in-process or test-server invocation.
///
/// A caller can report a status without executing the tool or receiving its result. These statuses
/// are distinct from `SlangResult` values and test verdicts. Clients provide result conversion,
/// integer validation, and test-result classification.
///
/// Negative enum integers are not universally portable native process exit values. Clients supply
/// platform-specific encoding and decoding of normal process exit values and handle termination by
/// a signal separately.
enum class ToolReturnCode
{
    // TODO(tfoley-refactor): Give `InternalCompilerError` a name that also covers diagnosed
    // compilation aborts reported by `handleSignal` with exceptions disabled.
    /// The client reported an internal compiler error.
    ///
    /// Internal errors, assertion failures, and caught exceptions can produce this status.
    /// With exceptions disabled, an ordinary diagnosed compilation abort can also produce it.
    /// This status alone does not mean that a compiler bug caused the failure.
    InternalCompilerError = -2,

    /// The client reported a compilation failure.
    ///
    /// The tool may still have produced output. A test expecting a compilation failure can
    /// still pass.
    CompilationFailed = -1,

    /// The client reported success for the requested operation.
    Success = 0,

    /// The client reported failure for the requested operation.
    Failed,

    /// The client reported an ignored invocation, for example because an optional feature
    /// was unavailable.
    Ignored,

    // TODO(tfoley-refactor): Give `FailedToRun` a name that also covers a missing invocation
    // result after execution may have started.
    /// The caller could not run the tool or obtain its result.
    ///
    /// This status does not establish whether the tool started or completed execution.
    FailedToRun,
};

} // namespace Slang

namespace slang::foundation::tool_status_codes
{
using ::Slang::ToolReturnCode;
} // namespace slang::foundation::tool_status_codes

#endif // SLANG_FOUNDATION_TOOL_STATUS_CODES_H
