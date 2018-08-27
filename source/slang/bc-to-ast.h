// bc-to-ast.h
#pragma once

#include "compiler.h"
#include "syntax.h"

namespace Slang
{
    RefPtr<ModuleDecl> loadBinaryModuleAST(
        CompileRequest* compileRequest,
        ISlangBlob*     blob);
}
