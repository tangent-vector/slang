// bc-to-ast.cpp
#include "bc-to-ast.h"

#include "bytecode.h"

namespace Slang
{
    struct BCToASTContext
    {
        CompileRequest*         compileRequest;
        ISlangBlob*             blob;

        SlangBCReflectionSectonHeader*      bcAST;
        SlangBCStringTableSectionHeader*    bcSymbolNameTable;
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

    void fillInDecl(
        BCToASTContext*     context,
        BCReflectionNode*   bcNode,
        Decl*               decl);

    void fillInDeclCommon(
        BCToASTContext*         context,
        SlangBCReflectionDecl*  bcDecl,
        Decl*                   decl)
    {
        decl->nameAndLoc.name = context->compileRequest->getNamePool()->getName(
            getString(context->bcSymbolNameTable, bcDecl->name));

        // TODO: modifiers
        // TODO: parent
    }

    void fillInContainerDeclCommon(
        BCToASTContext*                 context,
        SlangBCReflectionContainerNode* bcDecl,
        ContainerDecl*                  decl)
    {
        fillInDeclCommon(context, &bcDecl->asDecl, decl);

        uint32_t* memberIndices = (uint32_t*)((char*)bcDecl
            + bcDecl->memberIndicesOffset);

        UInt memberCount = bcDecl->memberCount;
        for(UInt mm = 0; mm < memberCount; ++mm)
        {
            UInt memberIndex = memberIndices[mm];
            BCReflectionNode* bcMemberNode = getNode(
                context->bcAST,
                memberIndex);

            auto memberDecl = createDecl(context, bcMemberNode);
            fillInDecl(context, bcMemberNode, memberDecl);

            decl->Members.Add(memberDecl);
            memberDecl->ParentDecl = decl;
        }
    }

    void fillInModuleDecl(
        BCToASTContext*                 context,
        SlangBCReflectionContainerNode* bcDecl,
        ModuleDecl*                     decl)
    {
        fillInContainerDeclCommon(context, bcDecl, decl);
    }

    void fillInVarDecl(
        BCToASTContext*             context,
        SlangBCReflectionVarNode*   bcDecl,
        VarDeclBase*                decl)
    {
        fillInDeclCommon(context, &bcDecl->asDecl, decl);

        // TODO: type, etc.
    }

    void fillInFuncDecl(
        BCToASTContext*             context,
        SlangBCReflectionFuncNode*  bcDecl,
        FuncDecl*                   decl)
    {
        fillInContainerDeclCommon(context, &bcDecl->asContainer, decl);

        // TODO: handle result type here
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
                fillInModuleDecl(context, (SlangBCReflectionContainerNode*)bcNode, (ModuleDecl*)decl);
            }
            break;

        case SLANG_BC_REFLECTION_TAG_PARAM:
            {
                fillInVarDecl(context, (SlangBCReflectionVarNode*)bcNode, (ParamDecl*)decl);
            }
            break;

        case SLANG_BC_REFLECTION_TAG_FUNC:
            {
                fillInFuncDecl(context, (SlangBCReflectionFuncNode*)bcNode, (FuncDecl*)decl);
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


        int32_t bcASTSectionIndex = findSection(bcFileHeader, SLANG_BC_SECTION_TYPE_REFLECTION);
        if(bcASTSectionIndex < 0)
        {
            getSink(context)->diagnose(SourceLoc(), Diagnostics::unexpected, "invalid Slang bytecode file");
            return nullptr;
        }
        auto bcAST = (SlangBCReflectionSectonHeader*) getSectionData(bcFileHeader, bcASTSectionIndex);

        auto bcSymbolNameTableIndex = findChildSection(bcFileHeader, bcASTSectionIndex, SLANG_BC_SECTION_TYPE_STRING_TABLE);
        if(bcSymbolNameTableIndex < 0)
        {
            getSink(context)->diagnose(SourceLoc(), Diagnostics::unexpected, "invalid Slang bytecode file");
            return nullptr;
        }
        auto bcSymbolNameTable = (SlangBCStringTableSectionHeader*)getSectionData(bcFileHeader, bcSymbolNameTableIndex);

        context->bcAST = bcAST;
        context->bcSymbolNameTable = bcSymbolNameTable;


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
