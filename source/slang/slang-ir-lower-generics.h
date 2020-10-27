// slang-ir-lower-generics.h
#pragma once

#include "slang-ir.h"

namespace Slang
{
    struct IRModule;
    class DiagnosticSink;
    class TargetRequest;

    typedef uint32_t LowerGenericsOptions;
    enum
    {
        kLowerGeneicsOptions_None = 0,
        kLowerGenericsOption_AllowDynamicDispatch               = 1 << 0,
        kLowerGenericsOption_SpecializeDispatchToKnownTargets   = 1 << 1,
    };

    /// Lower generic and interface-based code to ordinary types and functions using
    /// dynamic dispatch mechanisms.
    void lowerGenerics(
        TargetRequest*          targetReq,
        IRModule*               module,
        DiagnosticSink*         sink,
        LowerGenericsOptions    options);

}
