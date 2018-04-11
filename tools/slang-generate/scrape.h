// scrape.h
#pragma once

// This file defines the interface for "scraping"
// source files to gather information on the types,
// functions, etc. declared within them.
//
// This information can be used to feed various
// code generation tasks.

#include "../../source/core/list.h"

namespace slang_generate
{
    using namespace Slang;

    struct Syntax
    {};

    struct Decl : Syntax
    {};

    struct Stmt : Syntax
    {};

    struct Expr : Syntax
    {};

    struct Type : Syntax
    {};

    struct Declarator : Syntax
    {};

    struct ContainerDecl : Decl
    {
        List<Decl*> decls;
    };

    struct FileDecl : ContainerDecl
    {};

    struct AggTypeDecl : ContainerDecl
    {};

    struct StructDecl : AggTypeDecl
    {};

    struct ClassDecl : AggTypeDecl
    {};

    FileDecl* scrapeFile(char const* path);
}
