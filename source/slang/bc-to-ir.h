// bc-to-ir.h
#pragma once

#include "compiler.h"
#include "syntax.h"

namespace Slang
{
    RefPtr<IRModule> loadBinaryModuleIR(
        CompileRequest* compileRequest,
        ISlangBlob*     blob);
}
