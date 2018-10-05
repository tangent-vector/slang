// ir-definite-initialization.h
#pragma once

namespace Slang
{
    class DiagnosticSink;
    struct IRModule;

        /// Check that functions in `module` objey the definitive initialization rules.
        ///
        /// Diagnostics related to possible use of initialized values will be emitted to `sink`.
    void checkDefiniteInitialization(
        IRModule*       module,
        DiagnosticSink* sink);
}
