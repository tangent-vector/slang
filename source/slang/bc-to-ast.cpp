// bc-to-ast.cpp
#include "bc-to-ast.h"

#include "bytecode.h"
#include "bc-impl.h"

namespace Slang
{
    struct BCToASTContext
    {
        CompileRequest*         compileRequest;
        ISlangBlob*             blob;

        SlangBCReflectionSectonHeader*      bcAST;
        SlangBCStringTableSectionHeader*    bcSymbolNameTable;

        Dictionary<Int, RefPtr<Decl>> mapNodeIDToDecl;
        Dictionary<Int, RefPtr<Type>> mapNodeIDToType;

        List<RefPtr<DeferredAction>>        deferredActions;
    };

    DiagnosticSink* getSink(
        BCToASTContext*  context)
    {
        return &context->compileRequest->mSink;
    }

    Session* getSession(
        BCToASTContext* context)
    {
        return context->compileRequest->mSession;
    }

    void addDeferredAction(BCToASTContext* context, RefPtr<DeferredAction> const& action)
    {
        context->deferredActions.Add(action);
    }

    template<typename T>
    void addDeferredAction(BCToASTContext* context, T const& action)
    {
        RefPtr<DeferredAction> actionObj = new DeferredActionImpl<T>(action);
        addDeferredAction(context, actionObj);
    }

//

    RefPtr<Decl> getDecl(
        BCToASTContext* context,
        Int             nodeID);

    // Types

    RefPtr<Type> createTypeForNode(
        BCToASTContext* context,
        Int            nodeID)
    {
        auto node = getNode(context->bcAST, nodeID);

        auto tag = node->tag;

        if( (tag >= SLANG_BC_REFLECTION_TAG_DECL_BEGIN) && (tag < SLANG_BC_REFLECTION_TAG_DECL_END) )
        {
            auto decl = getDecl(context, nodeID);
            return DeclRefType::Create(getSession(context), makeDeclRef(decl.Ptr()));
        }

        getSink(context)->diagnose(SourceLoc(), Diagnostics::unexpected, "unhandled type case");
        return nullptr;
    }

    RefPtr<Type> getType(
        BCToASTContext* context,
        Int             nodeID)
    {
        RefPtr<Type> type;
        if(context->mapNodeIDToType.TryGetValue(nodeID, type))
            return type;

        type = createTypeForNode(context, nodeID);
        context->mapNodeIDToType.Add(nodeID, type);
        return type;
    }

    // Declarations

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

        int32_t* memberIndices = (int32_t*)((char*)bcDecl
            + bcDecl->memberIndicesOffset);

        UInt memberCount = bcDecl->memberCount;
        for(UInt mm = 0; mm < memberCount; ++mm)
        {
            Int memberIndex = memberIndices[mm];

            auto memberDecl = getDecl(context, memberIndex);

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

        decl->type.type = getType(context, bcDecl->typeID);
    }

    void fillInFuncDecl(
        BCToASTContext*             context,
        SlangBCReflectionFuncNode*  bcDecl,
        FuncDecl*                   decl)
    {
        fillInContainerDeclCommon(context, &bcDecl->asContainer, decl);

        decl->ReturnType.type = getType(context, bcDecl->resultTypeID);
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

    RefPtr<VarDeclBase> createVarDecl(
        BCToASTContext*     context,
        BCReflectionNode*   bcNode,
        RefPtr<VarDeclBase> decl)
    {
        addDeferredAction(context, [=]{ fillInVarDecl(context, (SlangBCReflectionVarNode*)bcNode, decl);});
        return decl;
    }

    RefPtr<AggTypeDeclBase> createAggTypeDecl(
        BCToASTContext*                 context,
        SlangBCReflectionContainerNode* bcNode,
        RefPtr<AggTypeDeclBase>         decl)
    {
        addDeferredAction(context, [=]{ fillInContainerDeclCommon(context, bcNode, decl);});
        return decl;
    }

    RefPtr<Decl> importDecl(
        BCToASTContext*         context,
        Int                     nodeID,
        SlangBCReflectionDecl*  bcDecl)
    {
        // We are expected to find a matching declaration via import
        switch( bcDecl->asNode.tag )
        {
        case SLANG_BC_REFLECTION_TAG_MODULE:
            break;

        default:
            {
                // TODO: need to grab a reference to
                // the parent of this node, so that we
                // can use it as a container decl for lookup...
            }
            break;
        }
    }

    RefPtr<Decl> createDecl(
        BCToASTContext* context,
        Int             nodeID)
    {
        auto bcNode = (SlangBCReflectionDecl*) getNode(context->bcAST, nodeID);

        if( nodeID < 0 )
        {
            return importDecl(context, nodeID, bcNode);
        }

        switch(bcNode->asNode.tag)
        {
        case SLANG_BC_REFLECTION_TAG_MODULE:
            {
                RefPtr<ModuleDecl> decl = new ModuleDecl();
                addDeferredAction(context, [=]{ fillInModuleDecl(context, (SlangBCReflectionContainerNode*)bcNode, decl);});
                return decl;
            }

        case SLANG_BC_REFLECTION_TAG_FUNC:
            {
                RefPtr<FuncDecl> decl = new FuncDecl();
                addDeferredAction(context, [=]{ fillInFuncDecl(context, (SlangBCReflectionFuncNode*)bcNode, decl);});
                return decl;
            }

        case SLANG_BC_REFLECTION_TAG_PARAM:
            return createVarDecl(context, bcNode, new ParamDecl());

        case SLANG_BC_REFLECTION_TAG_STRUCT:
            return createAggTypeDecl(context, (SlangBCReflectionContainerNode*)bcNode, new StructDecl());

        default:
            getSink(context)->diagnose(SourceLoc(), Diagnostics::unexpected, "invalid Slang bytecode file");
            return nullptr;
        }
    }

    RefPtr<Decl> getDecl(
        BCToASTContext* context,
        Int             nodeID)
    {
        RefPtr<Decl> decl;
        if(context->mapNodeIDToDecl.TryGetValue(nodeID, decl))
            return decl;

        decl = createDecl(context, nodeID);
        context->mapNodeIDToDecl.Add(nodeID, decl);
        return decl;
    }

    //

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
        auto moduleDecl = createDecl(context, 0).As<ModuleDecl>();

        while( context->deferredActions.Count() )
        {
            List<RefPtr<DeferredAction>> deferredActions;
            deferredActions.SwapWith(context->deferredActions);

            for( auto action : deferredActions )
            {
                action->execute();
            }
        }

        return moduleDecl;
    }
}
