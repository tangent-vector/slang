// ast-to-bc.cpp
#include "bc-impl.h"

#include "compiler.h"
#include "visitor.h"

namespace Slang
{

struct ASTSection;

struct ASTNodeBuilder : RefObject
{
    uint32_t tag;
    Int nodeIndex;
    RefPtr<ASTSection> section;

    virtual BCWriteOffsetBase writeData(BCWriter& writer) = 0;

    bool isImported() const { return nodeIndex < 0; }
};

struct ASTSection : BCSectionBuilder
{
    ASTSection()
    {
        this->type = SLANG_BC_SECTION_TYPE_REFLECTION;
    }

    List<RefPtr<ASTNodeBuilder>> nodes;
    List<RefPtr<ASTNodeBuilder>> importedNodes;

    RefPtr<BCStringTableBuilder> stringTable;

    void writeData(BCWriter& writer)
    {
        auto nodeCount = nodes.Count();
        auto importCount = importedNodes.Count();

        typedef uint32_t NodeTableEntry;

        auto bcSectionHeader = writer.reserve<SlangBCReflectionSectonHeader>();
        auto bcNodeOffsets = writer.reserve<NodeTableEntry>(nodeCount);
        auto bcImportOffsets = writer.reserve<NodeTableEntry>(importCount);

        bcSectionHeader->entryCount = nodeCount;
        bcSectionHeader->entrySize = sizeof(NodeTableEntry);
        bcSectionHeader->entryTableOffset = bcNodeOffsets.offset;

        bcSectionHeader->importCount = importCount;
        bcSectionHeader->importTableOffset = bcImportOffsets.offset;

        for(UInt nn = 0; nn < nodeCount; ++nn)
        {
            BCWriter subWriter;
            subWriter.baseOffset = writer.baseOffset;
            subWriter.outData = writer.outData;

            auto nodeOffset = nodes[nn]->writeData(subWriter);
            bcNodeOffsets[nn] = nodeOffset - bcSectionHeader;
        }

        for(UInt ii = 0; ii < importCount; ++ii)
        {
            BCWriter subWriter;
            subWriter.baseOffset = writer.baseOffset;
            subWriter.outData = writer.outData;

            auto importedNodeOffset = importedNodes[ii]->writeData(subWriter);
            bcImportOffsets[ii] = importedNodeOffset - bcSectionHeader;
        }
    }
};

struct SharedASTToBCContext
{
    CompileRequest*                     compileRequest;
    RefPtr<ASTSection>                  astSection;
    List<RefPtr<DeferredAction>>        deferredActions;
    Dictionary<Decl*, ASTNodeBuilder*>  mapDeclToNode;
};

struct ASTToBCContext
{
    SharedASTToBCContext* shared;
};

void addDeferredAction(ASTToBCContext* context, RefPtr<DeferredAction> const& action)
{
    context->shared->deferredActions.Add(action);
}

template<typename T>
void addDeferredAction(ASTToBCContext* context, T const& action)
{
    RefPtr<DeferredAction> actionObj = new DeferredActionImpl<T>(action);
    addDeferredAction(context, actionObj);
}

//

struct DeclNodeBuilder : ASTNodeBuilder
{
    RefPtr<Decl> decl;

    void writeDeclNode(BCWriter& writer, BCWriteOffset<SlangBCReflectionDecl> const& bcNode)
    {
        (void)writer;
        bcNode->asNode.tag = tag;
        bcNode->name = section->stringTable->addString(decl->getName());
    }
};


struct VarDeclNodeBuilder : DeclNodeBuilder
{
    ASTNodeBuilder* typeNode;

    void writeVarNode(BCWriter& writer, BCWriteOffset<SlangBCReflectionVarNode> const& bcNode)
    {
        writeDeclNode(writer, bcNode.as<SlangBCReflectionDecl>());
        bcNode->typeID = typeNode->nodeIndex;
    }

    BCWriteOffsetBase writeData(BCWriter& writer)
    {
        auto bcNode = writer.reserveBase<SlangBCReflectionVarNode>();
        writeVarNode(writer, bcNode);
        return bcNode;
    }
};

struct ContainerDeclNodeBuilder : DeclNodeBuilder
{
    List<ASTNodeBuilder*> memberNodes;

    void writeContainerNode(
        BCWriter& writer,
        BCWriteOffset<SlangBCReflectionContainerNode> const& bcNode)
    {
        writeDeclNode(writer, bcNode.as<SlangBCReflectionDecl>());

        auto memberCount = memberNodes.Count();
        auto bcMemberIndices = writer.reserve<uint32_t>(memberCount);

        bcNode->memberCount = memberCount;
        bcNode->memberIndicesOffset = bcMemberIndices.offset;

        for(UInt mm = 0; mm < memberCount; ++mm)
        {
            bcMemberIndices[mm] = memberNodes[mm]->nodeIndex;
        }
    }

    BCWriteOffsetBase writeData(BCWriter& writer)
    {
        auto bcNode = writer.reserveBase<SlangBCReflectionContainerNode>();
        writeContainerNode(writer, bcNode);
        return bcNode;
    }
};

struct FuncDeclNodeBuilder : ContainerDeclNodeBuilder
{
    RefPtr<ASTNodeBuilder> resultTypeNode;

    BCWriteOffsetBase writeData(BCWriter& writer)
    {
        auto bcNode = writer.reserveBase<SlangBCReflectionFuncNode>();
        writeContainerNode(writer, bcNode.as<SlangBCReflectionContainerNode>());
        bcNode->resultTypeID = resultTypeNode->nodeIndex;
        return bcNode;
    }
};

struct StructDeclNodeBuilder : ContainerDeclNodeBuilder
{
};

struct ModuleDeclNodeBuilder : ContainerDeclNodeBuilder
{
};

ASTNodeBuilder* getNodeForDecl(
    ASTToBCContext* context,
    RefPtr<Decl>    decl);

ASTNodeBuilder* getNodeForDeclRef(
    ASTToBCContext*     context,
    DeclRefBase const&  declRef);

ASTNodeBuilder* getNodeForType(
    ASTToBCContext* context,
    Type*           type);

void addNode(
    ASTToBCContext* context,
    ASTNodeBuilder* node,
    bool            isImport)
{
    auto section = context->shared->astSection;
    node->section = section;

    if( isImport )
    {
        UInt nodeIndex = section->importedNodes.Count();
        node->nodeIndex = ~nodeIndex;
        section->importedNodes.Add(node);
    }
    else
    {
        UInt nodeIndex = section->nodes.Count();
        node->nodeIndex = nodeIndex;
        section->nodes.Add(node);
    }
}

template<typename T>
T* addNode(ASTToBCContext* context, Decl* decl, uint32_t tag, bool isImport)
{
    auto node = new T();
    node->tag = tag;
    node->decl = decl;
    addNode(context, node, isImport);
    return node;
}

//

void populateDeclNode(
    ASTToBCContext*     context,
    Decl*               decl,
    DeclNodeBuilder*    node)
{
    (void)context;
    (void)decl;
    (void)node;
    // TODO: deal with name, etc.
}

void populateVarDeclNode(
    ASTToBCContext*     context,
    VarDeclBase*        decl,
    VarDeclNodeBuilder* node)
{
    populateDeclNode(context, decl, node);

    node->typeNode = getNodeForType(context, decl->type);

    // TODO: type
    //
    // TODO: We need to decide if and how initializers
    // make it through serialization. In general, we
    // don't want to be encoding expressions in the
    // reflection data, because all the actual "code"
    // should live in the IR representation instead.
    // The challenge in this case is that things like
    // default parameter values may be something we
    // want to inline in at call sites...
    //
}

void populateContainerDeclNode(
    ASTToBCContext*             context,
    ContainerDecl*              decl,
    ContainerDeclNodeBuilder*   node)
{
    populateDeclNode(context, decl, node);

//    for(auto memberDecl : decl->Members)
//    {
//        auto memberNode = getNodeForDecl(context, memberDecl);
//        node->memberNodes.Add(memberNode);
//    }
}

void populateCallableDeclNode(
    ASTToBCContext*         context,
    CallableDecl*           decl,
    FuncDeclNodeBuilder*    node)
{
    populateContainerDeclNode(context, decl, node);

    node->resultTypeNode = getNodeForType(context, decl->ReturnType);
}

void populateImportedModuleNode(
    ASTToBCContext*             context,
    ModuleDecl*                 decl,
    ContainerDeclNodeBuilder*   node)
{
    populateDeclNode(context, decl, node);
}

void populateAggTypeDeclNode(
    ASTToBCContext*         context,
    AggTypeDeclBase*        decl,
    StructDeclNodeBuilder*  node)
{
    populateContainerDeclNode(context, decl, node);
}

//

struct ASTToBCDeclVisitor : DeclVisitor<ASTToBCDeclVisitor, DeclNodeBuilder*>
{
    ASTToBCContext* context;
    bool            isImport;

    DiagnosticSink* getSink()
    {
        return &context->shared->compileRequest->mSink;
    }

#define UNUSED(NAME) \
    DeclNodeBuilder* visit##NAME(NAME*) { return nullptr; }

    UNUSED(DeclGroup)
    UNUSED(EmptyDecl)
    UNUSED(ScopeDecl)

    // Note: `import` declarations can largely be ignored for our
    // purposes, because we can build up a list of the imported
    // declarations we require by other means.
    //
    // TODO: THe one big exception to the above is any `__exported`
    // import declarations, because we need these to be taken
    // into account in any module that imports this code.
    //
    UNUSED(ImportDecl)

    // TODO: Attribute and syntax declarations need to be preserved in the output
    // before we switch to embedding the stdlib as bytecode, because we obviously
    // need downstream modules to see these declarations on import.
    UNUSED(AttributeDecl)
    UNUSED(SyntaxDecl)

#undef UNUSED

#define CASE(NAME, TAG, NODECLASS, POPULATEFUNC)    \
    DeclNodeBuilder* visit##NAME(NAME* decl) {      \
        auto node = addNode<NODECLASS>(context,     \
            decl, SLANG_BC_REFLECTION_TAG_##TAG, isImport);   \
        auto ctxt = context;                        \
        addDeferredAction(context, [=]{POPULATEFUNC(ctxt, decl, node);});   \
        return node; }

    CASE(ModuleDecl,        MODULE,         ModuleDeclNodeBuilder,  populateContainerDeclNode)
    CASE(ConstructorDecl,   CONSTRUCTOR,    FuncDeclNodeBuilder,    populateCallableDeclNode)

    CASE(StructField,           FIELD,                  VarDeclNodeBuilder,     populateVarDeclNode)
    CASE(ParamDecl,             PARAM,                  VarDeclNodeBuilder,     populateVarDeclNode)
    CASE(Variable,              VAR,                    VarDeclNodeBuilder,     populateVarDeclNode)
    CASE(GenericValueParamDecl, GENERIC_VALUE_PARAM,    VarDeclNodeBuilder,     populateVarDeclNode)

    // TODO: for function nodes we need to skip anything that isn't a "primary" declaration,
    // so that we only emit one copy of each declared function to the output.
    CASE(FuncDecl,          FUNC,           FuncDeclNodeBuilder,    populateCallableDeclNode)


    CASE(StructDecl,    STRUCT, StructDeclNodeBuilder, populateAggTypeDeclNode)

#undef CASE

#define UNIMPLEMENTED(NAME) \
    DeclNodeBuilder* visit##NAME(NAME* decl) { SLANG_UNIMPLEMENTED(getSink(), decl, "AST->BC"); return nullptr; }


    UNIMPLEMENTED(AccessorDecl)
    UNIMPLEMENTED(AggTypeDeclBase)
    UNIMPLEMENTED(EnumCaseDecl)
    UNIMPLEMENTED(GenericDecl)
    UNIMPLEMENTED(GenericTypeParamDecl)
    UNIMPLEMENTED(SubscriptDecl)
    UNIMPLEMENTED(TypeConstraintDecl)
    UNIMPLEMENTED(TypeDefDecl)


#undef UNIMPLEMENTED

};

ASTNodeBuilder* createNodeForDeclImpl(
    ASTToBCContext*     context,
    RefPtr<Decl>        decl,
    bool                isImport)
{
    ASTToBCDeclVisitor visitor;
    visitor.context = context;
    visitor.isImport = isImport;
    return visitor.dispatch(decl);
}

ASTNodeBuilder* createNodeForDecl(
    ASTToBCContext* context,
    RefPtr<Decl>    decl)
{
    bool isImported = false;

    auto parentDecl = decl->ParentDecl;
    ContainerDeclNodeBuilder* parentNode = nullptr;
    if( !parentDecl )
    {
        // Expectation: this should always be an imported module, or something
        // has gone horribly wrong.
        if( auto moduleDecl = decl.As<ModuleDecl>() )
        {
            // Okay, we are importing a module, and we should handle that
            isImported = true;
        }
        else
        {
            SLANG_UNEXPECTED("decl had no parent!");
            return nullptr;
        }
    }
    else
    {
        // Othwerwise we must have had a parent declaration, and it
        // should get translated into a node as well.
        parentNode = (ContainerDeclNodeBuilder*) getNodeForDecl(context, parentDecl);
        isImported = parentNode->isImported();
    }

    ASTNodeBuilder* declNode = createNodeForDeclImpl(context, decl, isImported);
    if( parentNode )
    {
        parentNode->memberNodes.Add(declNode);
    }
    return declNode;
}

void registerNodeForDecl(
    ASTToBCContext* context,
    Decl*           decl,
    ASTNodeBuilder* node)
{
    context->shared->mapDeclToNode.Add(decl, node);
}

ASTNodeBuilder* getNodeForDecl(
    ASTToBCContext* context,
    RefPtr<Decl>    decl)
{
    ASTNodeBuilder* declNode = nullptr;
    if(context->shared->mapDeclToNode.TryGetValue(decl.Ptr(), declNode))
        return declNode;

    declNode = createNodeForDecl(context, decl);
    registerNodeForDecl(context, decl, declNode);
    return declNode;
}

ASTNodeBuilder* getNodeForDeclRef(
    ASTToBCContext*     context,
    DeclRefBase const&  declRef)
{
    // Always start with a node for the underlying declaration
    auto declNode = getNodeForDecl(context, declRef.getDecl());

    // If this decl-ref doesn't apply any substitutions, then we are
    // already done: the declaration node will represent a reference to itself
    if(!declRef.substitutions )
    {
        return declNode;
    }

    // Otherwise, we need to construct zero or more specialization
    // nodes, based on what is present along the chain.
    //
    // TODO: we should cache these and no re-create nodes we already have...
    SLANG_UNEXPECTED("specialized decl ref");
    UNREACHABLE_RETURN(nullptr);
}

//

struct ASTToBCTypeVisitor : TypeVisitor<ASTToBCTypeVisitor, ASTNodeBuilder*>
{
    ASTToBCContext* context;

    DiagnosticSink* getSink()
    {
        return &context->shared->compileRequest->mSink;
    }

#define UNUSED(NAME) \
    ASTNodeBuilder* visit##NAME(NAME*) { return nullptr; }

    UNUSED(TypeType)
    UNUSED(ErrorType)
    UNUSED(InitializerListType)
    UNUSED(OverloadGroupType)

#undef UNUSED

#define CASE(NAME)  \
    ASTNodeBuilder* visit##NAME(NAME* type) { return getNodeForDeclRef(context, type->declRef); }

    CASE(DeclRefType)
    CASE(GenericDeclRefType)
    CASE(NamedExpressionType)

#undef CASE

#define UNIMPLEMENTED(NAME) \
    ASTNodeBuilder* visit##NAME(NAME*) { SLANG_UNIMPLEMENTED_X("AST->BC"); UNREACHABLE_RETURN(nullptr); }

    UNIMPLEMENTED(ArrayExpressionType)
    UNIMPLEMENTED(FuncType)

#undef UNIMPLEMENTED

};


ASTNodeBuilder* getNodeForType(
    ASTToBCContext* context,
    Type*           type)
{
    ASTToBCTypeVisitor visitor;
    visitor.context = context;
    return visitor.dispatch(type);
}


//

void walkDeclsRec(
    ASTToBCContext* context,
    RefPtr<Decl>    decl)
{
    if(decl.As<ScopeDecl>())
        return;

    getNodeForDecl(context, decl);

    if( auto containerDecl = decl.As<ContainerDecl>() )
    {
        for( auto memberDecl : containerDecl->Members )
        {
            walkDeclsRec(context, memberDecl);
        }
    }
}

void generateBytecodeSectionsForAST(
    CompileRequest* compileRequest,
    BCFileBuilder*  fileBuilder)
{
    auto astSection = fileBuilder->addSection<ASTSection>(".ast");
    astSection->stringTable = astSection->addChildSection<BCStringTableBuilder>(".strings");

    SharedASTToBCContext sharedContextStorage;
    auto sharedContext = &sharedContextStorage;
    sharedContext->compileRequest = compileRequest;
    sharedContext->astSection = astSection;

    ASTToBCContext contextStorage;
    auto context = &contextStorage;
    context->shared = sharedContext;

    // Need to translate the module(s) in the compile request
    // into node structures in the BC file.
    //
    // TODO: the multiple-translation-unit case should really
    // be handled by creating an "umbrella" node to collect
    // all the separate TUs, and then having shared resource
    // parameters live at the umbrella scope, instead of attached
    // to invidiual TUs.
    //
    for(auto translationUnit : compileRequest->translationUnits)
    {
        auto moduleDecl = translationUnit->SyntaxNode;
        auto moduleNode = createNodeForDeclImpl(context, moduleDecl, false);
        registerNodeForDecl(context, moduleDecl, moduleNode);
    }

    // Now, recursively walk the structure of all the declarations
    // we need to export and ensure that we are exporting everything
    // we need to.
    for(auto translationUnit : compileRequest->translationUnits)
    {
        walkDeclsRec(context, translationUnit->SyntaxNode);
    }

    while( sharedContext->deferredActions.Count() )
    {
        List<RefPtr<DeferredAction>> deferredActions;
        deferredActions.SwapWith(sharedContext->deferredActions);

        for( auto action : deferredActions )
        {
            action->execute();
        }
    }
}

} // namespace Slang
