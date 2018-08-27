// ast-to-bc.cpp
#include "bc-impl.h"

#include "compiler.h"
#include "visitor.h"

namespace Slang
{

struct ASTNodeBuilder : RefObject
{
    uint32_t tag;

    virtual void writeData(BCWriter& writer) = 0;
};

struct ASTSection : BCSectionBuilder
{
    ASTSection()
    {
        this->type = SLANG_BC_SECTION_TYPE_REFLECTION;
    }

    List<RefPtr<ASTNodeBuilder>> nodes;

    void writeData(BCWriter& writer)
    {
        auto nodeCount = nodes.Count();

        typedef uint32_t NodeTableEntry;

        auto bcSectionHeader = writer.reserve<SlangBCReflectionSectonHeader>();
        auto bcNodeOffsets = writer.reserve<NodeTableEntry>(nodeCount);

        bcSectionHeader->entryCount = nodeCount;
        bcSectionHeader->entrySize = sizeof(NodeTableEntry);
        bcSectionHeader->entryTableOffset = bcNodeOffsets.offset;

        for(UInt nn = 0; nn < nodeCount; ++nn)
        {
            auto nodeOffset = writer.tell();
            bcNodeOffsets[nn] = nodeOffset;

            nodes[nn]->writeData(writer);
        }
    }
};

struct SharedASTToBCContext
{
    CompileRequest*     compileRequest;
    RefPtr<ASTSection>  astSection;
};

struct ASTToBCContext
{
    SharedASTToBCContext* shared;
};

//

struct DeclNodeBuilder : ASTNodeBuilder
{
    void writeDeclNode(BCWriter& writer, BCWriteOffset<SlangBCReflectionDecl> const& bcNode)
    {
        (void)writer;
        bcNode->asNode.tag = tag;
    }
};


struct VarDeclNodeBuilder : DeclNodeBuilder
{
    void writeVarNode(BCWriter& writer, BCWriteOffset<SlangBCReflectionVarNode> const& bcNode)
    {
        writeDeclNode(writer, bcNode.as<SlangBCReflectionDecl>());
    }

    void writeData(BCWriter& writer)
    {
        auto bcNode = writer.reserve<SlangBCReflectionVarNode>();
        writeVarNode(writer, bcNode);
    }
};

struct ContainerDeclNodeBuilder : DeclNodeBuilder
{
    List<DeclNodeBuilder*> memberNodes;

    void writeContainerNode(
        BCWriter& writer,
        BCWriteOffset<SlangBCReflectionContainerNode> const& bcNode)
    {
        writeDeclNode(writer, bcNode.as<SlangBCReflectionDecl>());
    }

    void writeData(BCWriter& writer)
    {
        auto bcNode = writer.reserve<SlangBCReflectionContainerNode>();
        writeContainerNode(writer, bcNode);
    }
};

struct FuncDeclNodeBuilder : ContainerDeclNodeBuilder
{
};

struct ModuleDeclNodeBuilder : ContainerDeclNodeBuilder
{
};

DeclNodeBuilder* generateBCNodeForDecl(
    ASTToBCContext* context,
    RefPtr<Decl>    decl);

//struct ASTToBCVisitor : TypeVisitor<int32_t>
//{
//};

void addNode(
    ASTToBCContext* context,
    ASTNodeBuilder* node)
{
    context->shared->astSection->nodes.Add(node);
}

template<typename T>
T* addNode(ASTToBCContext* context, uint32_t tag)
{
    auto node = new T();
    node->tag = tag;
    addNode(context, node);
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

    for(auto memberDecl : decl->Members)
    {
        auto memberNode = generateBCNodeForDecl(context, memberDecl);
        node->memberNodes.Add(memberNode);
    }
}

void populateCallableDeclNode(
    ASTToBCContext*         context,
    CallableDecl*           decl,
    FuncDeclNodeBuilder*    node)
{
    populateContainerDeclNode(context, decl, node);

    // TODO: result type, etc.
}


//

struct ASTToBCDeclVisitor : DeclVisitor<ASTToBCDeclVisitor, DeclNodeBuilder*>
{
    ASTToBCContext* context;

    DiagnosticSink* getSink()
    {
        return &context->shared->compileRequest->mSink;
    }

#define UNUSED(NAME) \
    DeclNodeBuilder* visit##NAME(NAME*) { return nullptr; }

    UNUSED(AttributeDecl)
        UNUSED(DeclGroup)
        UNUSED(EmptyDecl)
        UNUSED(ImportDecl)
        UNUSED(ScopeDecl)
        UNUSED(SyntaxDecl)

#undef UNUSED

#define CASE(NAME, TAG, NODECLASS, POPULATEFUNC)    \
    DeclNodeBuilder* visit##NAME(NAME* decl) {      \
        auto node = addNode<NODECLASS>(context,     \
            SLANG_BC_REFLECTION_TAG_##TAG);         \
        POPULATEFUNC(context, decl, node);          \
        return node; }

    CASE(ModuleDecl,        MODULE,         ModuleDeclNodeBuilder,  populateContainerDeclNode)
    CASE(FuncDecl,          FUNC,           FuncDeclNodeBuilder,    populateCallableDeclNode)
    CASE(ConstructorDecl,   CONSTRUCTOR,    FuncDeclNodeBuilder,    populateCallableDeclNode)

    CASE(StructField,           FIELD,                  VarDeclNodeBuilder,     populateVarDeclNode)
    CASE(ParamDecl,             PARAM,                  VarDeclNodeBuilder,     populateVarDeclNode)
    CASE(Variable,              VAR,                    VarDeclNodeBuilder,     populateVarDeclNode)
    CASE(GenericValueParamDecl, GENERIC_VALUE_PARAM,    VarDeclNodeBuilder,     populateVarDeclNode)

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


#undef UNUSED

};

DeclNodeBuilder* generateBCNodeForDecl(
    ASTToBCContext* context,
    RefPtr<Decl>    decl)
{
    ASTToBCDeclVisitor visitor;
    visitor.context = context;
    return visitor.dispatch(decl);
}

//

void generateBytecodeSectionsForAST(
    CompileRequest* compileRequest,
    BCFileBuilder*  fileBuilder)
{
    auto astSection = fileBuilder->addSection<ASTSection>(".ast");

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
        generateBCNodeForDecl(context, translationUnit->SyntaxNode);
    }
}

} // namespace Slang
