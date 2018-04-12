// scrape.h
#pragma once

// This file defines the interface for "scraping"
// source files to gather information on the types,
// functions, etc. declared within them.
//
// This information can be used to feed various
// code generation tasks.

#include "../../source/core/list.h"
#include "../../source/core/slang-string.h"

namespace slang_generate
{
    using Slang::List;
    using Slang::UnownedStringSlice;
    using Slang::UnownedTerminatedStringSlice;

    struct Name
    {
        UnownedTerminatedStringSlice text;
    };

    struct SourceLoc
    {
        char const* path    = nullptr;
        int         line    = 0;
        int         column  = 0;
    };

    struct Syntax
    {
        SourceLoc loc;
    };

    struct Decl : Syntax
    {};

    struct Stmt : Syntax
    {};

    struct TypeRepr : Syntax
    {};

    struct Expr : Syntax
    {};

    // Expressions

    struct NameExpr : Expr
    {
        Name* name;
    };

    struct LiteralExpr : Expr
    {
        UnownedStringSlice text;
    };

    struct IntegerLiteralExpr : LiteralExpr
    {};

    struct FloatingPointLiteralExpr : LiteralExpr
    {};

    struct StringLiteralExpr : LiteralExpr
    {};

    struct CharacterLiteralExpr : LiteralExpr
    {};

    struct NamedTypeRepr : TypeRepr
    {
        Name* name;
    };

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
