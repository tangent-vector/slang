// bc-to-ast.cpp
#include "bc-to-ast.h"

#include "bytecode.h"

namespace Slang
{
    struct BCToASTContext
    {
        CompileRequest*         compileRequest;
        ISlangBlob*             blob;

        SlangBCReflectionSectonHeader* bcAST;
    };

    DiagnosticSink* getSink(
        BCToASTContext*  context)
    {
        return &context->compileRequest->mSink;
    }

    RefPtr<Decl> createDecl(
        BCToASTContext*     context,
        BCReflectionNode*   bcNode)
    {
        switch(bcNode->tag)
        {
        case SLANG_BC_REFLECTION_TAG_MODULE:
            return new ModuleDecl();

        case SLANG_BC_REFLECTION_TAG_FUNC:
            return new FuncDecl();

        case SLANG_BC_REFLECTION_TAG_PARAM:
            return new ParamDecl();

        default:
            getSink(context)->diagnose(SourceLoc(), Diagnostics::unexpected, "invalid Slang bytecode file");
            return nullptr;
        }
    }

    void fillInDeclCommon(
        BCToASTContext*         context,
        SlangBCReflectionDecl*  bcDecl,
        Decl*                   decl)
    {
        (void)context;
        (void)bcDecl;
        (void)decl;
        // TODO: name
        // TODO: modifiers
        // TODO: parent
    }

    void fillInContainerDeclCommon(
        BCToASTContext*         context,
        SlangBCReflectionDecl*  bcDecl,
        ContainerDecl*          decl)
    {
        fillInDeclCommon(context, bcDecl, decl);
    }

    void fillInModuleDecl(
        BCToASTContext*         context,
        SlangBCReflectionDecl*  bcDecl,
        ModuleDecl*             decl)
    {
        fillInContainerDeclCommon(context, bcDecl, decl);
    }

    void fillInDecl(
        BCToASTContext*     context,
        BCReflectionNode*   bcNode,
        Decl*               decl)
    {
        switch(bcNode->tag)
        {
        case SLANG_BC_REFLECTION_TAG_MODULE:
            {
                fillInModuleDecl(context, (SlangBCReflectionDecl*)bcNode, (ModuleDecl*)decl);
            }
            break;

        default:
            break;
        }
    }

    RefPtr<ModuleDecl> loadBinaryModuleAST(
        CompileRequest* compileRequest,
        ISlangBlob*     blob)
    {
        auto bcFileHeader = (SlangBCFileHeader*) blob->getBufferPointer();

        BCToASTContext contextStorage;
        auto context = &contextStorage;

        context->compileRequest = compileRequest;
        context->blob = blob;

        UInt sectionCount = bcFileHeader->sectionTableEntryCount;
        SlangBCReflectionSectonHeader* bcAST = nullptr;
        for(UInt ii = 0; ii < sectionCount; ++ii)
        {
            auto sectionTableEntry = getSectionTableEntry(bcFileHeader, ii);
            if(sectionTableEntry.type != SLANG_BC_SECTION_TYPE_REFLECTION)
                continue;

            // Okay, we have an IR section, and we should try to load it.

            bcAST = (SlangBCReflectionSectonHeader*) getSectionData(bcFileHeader, sectionTableEntry);

            // We are breaking out of our search as soon as we find an IR
            // section, even if it isn't the only one. Is this reasonable?
            break;
        }
        if(!bcAST)
        {
            getSink(context)->diagnose(SourceLoc(), Diagnostics::unexpected, "invalid Slang bytecode file");
            return nullptr;
        }
        context->bcAST = bcAST;

        // Assume that entry #0 in the reflection section represents the whole module

        // TODO: should scan through all nodes in a first pass to create decl nodes
        // before they can be referenced by other nodes, then fill in in a second pass.
        //
        // TODO: even better than that would be to only populate thigns on-demand,
        // as lookup operations get performed.
        //
        auto bcModule = getNode(bcAST, 0);
        auto moduleDecl = createDecl(context, bcModule).As<ModuleDecl>();
        fillInDecl(context, bcModule, moduleDecl);

        return moduleDecl;
    }
}
