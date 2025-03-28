// ast-generator-main.cpp

#include "../../source/compiler-core/slang-lexer.h"
#include "../../source/compiler-core/slang-perfect-hash-codegen.h"
#include "../../source/core/slang-file-system.h"
#include "../../source/core/slang-io.h"
#include "../../source/core/slang-secure-crt.h"
#include "../../source/core/slang-string-util.h"
#include "../../source/core/slang-uint-set.h"

#include <stdio.h>

using namespace Slang;

namespace Diagnostics
{
#define DIAGNOSTIC(id, severity, name, messageFormat) \
    const DiagnosticInfo name = {id, Severity::severity, #name, messageFormat};
#include "slang-ast-diagnostic-defs.h"
#undef DIAGNOSTIC
} // namespace Diagnostics


void fatal(DiagnosticSink* sink, char const* message)
{
    sink->diagnose(SourceLoc(), Diagnostics::unexpected, message);
}

enum class SyntaxAttributeParseModeBit
{
    Optional,
    OneOrMore,
    CommaSeparated,
};

enum class SyntaxAttributeParseMode
{
    Default = 0,
    Optional,
    OneOrMore,
    ZeroOrMore,

    CommaSeparated,
    OptionalCommaSeparated,
    OneOrMoreCommaSeparated,
    ZeroOrMoreCommaSeparated,

    Count,
};

struct GenNode : public RefObject
{
    SourceLoc loc;
};

struct GenExpr : public GenNode
{};
typedef GenExpr SyntaxExpr;
typedef GenNode GenType;

struct GenModifier : public GenNode
{
public:
};
struct GenAbstractModifier : public GenModifier
{};
struct GenHiddenModifier : public GenModifier
{};

struct GenDecl : public GenNode
{
public:
    String name;
    List<RefPtr<GenModifier>> modifiers;

    template<typename T>
    T* findModifier()
    {
        for (auto m : modifiers)
        {
            if (auto found = as<T>(m))
                return found;
        }
        return nullptr;
    }
};

struct GenContainerDecl : public GenDecl
{
public:
    List<RefPtr<GenDecl>> directMembers;

    template<typename T>
    T* findFirstMemberOfType()
    {
        for (auto m : directMembers)
        {
            if (auto found = as<T>(m))
                return found;
        }
        return nullptr;
    }
};

struct GenRawDecl : public GenDecl
{
public:
    GenRawDecl(
        List<Token> tokens)
        : tokens(tokens)
    {}

    List<Token> tokens;
};

struct GenModuleDecl : public GenContainerDecl
{};

struct GenAggTypeDecl : public GenContainerDecl
{
public:
    RefPtr<GenAggTypeDecl> directBase;

    unsigned neededParseModeBitSet = 1;
};
struct GenClassDecl : public GenAggTypeDecl
{};
struct GenStructDecl : public GenAggTypeDecl
{};

struct GenTypeRef : public GenType
{
public:
    GenTypeRef(RefPtr<GenDecl> decl)
        : decl(decl)
    {}

    RefPtr<GenDecl> decl;
};

typedef GenAggTypeDecl SyntaxClassDef;

struct GenPatternPiece : public GenNode
{};

struct GenPatternLeaf : public GenPatternPiece
{
public:
};

struct GenPatternNamedLeaf : public GenPatternLeaf
{
public:
    GenPatternNamedLeaf(
        String name,
        RefPtr<GenType> type)
        : name(name)
        , type(type)
    {}

    String name;
    RefPtr<GenType> type;
};

struct GenPatternUnnamedLeaf : public GenPatternLeaf
{
public:
    GenPatternUnnamedLeaf(
        RefPtr<GenType> type)
        : type(type)
    {}

    RefPtr<GenType> type;
};

struct GenPatternTerminal : public GenType
{
    String value;
};

struct GenPatternSimpleTerminal : public GenPatternTerminal
{};

struct GenPatternKeyword : public GenPatternTerminal
{};

struct GenPatternNonterminal : public GenType
{
public:
    GenPatternNonterminal(
        String name)
        : name(name)
    {}

    String name;
};


struct GenPatternDecl : public GenDecl
{
    RefPtr<GenPatternPiece> expr;
};

struct GenFieldDecl : public GenDecl
{
public:
    GenFieldDecl(
        String name,
        RefPtr<GenType> type)
        : name(name)
        , type(type)
    {}

    String name;
    RefPtr<GenType> type;
};

struct GenPatternSeq : public GenPatternPiece
{
public:
    GenPatternSeq(
        List<RefPtr<GenPatternPiece>> exprs)
        : exprs(exprs)
    {}

    List<RefPtr<GenPatternPiece>> exprs;
};

struct GenPatternEnclosed : public GenPatternPiece
{
public:
    GenPatternEnclosed(
        RefPtr<GenPatternLeaf> open,
        RefPtr<GenPatternPiece> inner,
        RefPtr<GenPatternLeaf> close)
        : open(open)
        , inner(inner)
        , close(close)
    {}

    RefPtr<GenPatternLeaf> open;
    RefPtr<GenPatternPiece> inner;
    RefPtr<GenPatternLeaf> close;
};

struct OptionalType : public GenType
{
public:
    OptionalType(
        RefPtr<GenType> base)
        : base(base)
    {}

    RefPtr<GenType> base;
};

struct CommaSeparatedType : public GenType
{
public:
    CommaSeparatedType(
        RefPtr<GenType> base)
        : base(base)
    {}

    RefPtr<GenType> base;
};

struct ZeroOrMoreType : public GenType
{
public:
    ZeroOrMoreType(
        RefPtr<GenType> base)
        : base(base)
    {}

    RefPtr<GenType> base;
};

struct OneOrMoreType : public GenType
{
public:
    OneOrMoreType(
        RefPtr<GenType> base)
        : base(base)
    {}

    RefPtr<GenType> base;
};

void addMember(GenContainerDecl* parentDecl, GenDecl* memberDecl)
{
    if (!memberDecl) return;

    parentDecl->directMembers.add(memberDecl);
}

struct SyntaxSharedContext : public RefObject
{
    SyntaxSharedContext()
    {
        namePool.setRootNamePool(&rootPool);
    }

    NamePool namePool;
    RootNamePool rootPool;

    RefPtr<GenModuleDecl> moduleDecl;
//    Dictionary<String, RefPtr<SyntaxClassDef>> m_mapNameToSyntaxClass;
//    List<RefPtr<SyntaxClassDef>> m_defs;

#if 0
    RefPtr<SyntaxClassDef> findSyntaxClass(String const& name)
    {
        if (auto found = m_mapNameToSyntaxClass.tryGetValue(name))
        {
            return *found;
        }

        RefPtr<SyntaxClassDef> def = new SyntaxClassDef();
        def->name = name;

        m_mapNameToSyntaxClass.add(name, def);

        return def;
    }
#endif
};

struct OptionalToken : Token
{
    OptionalToken()
        : present(false)
    {}

    OptionalToken(Token const& token)
        : Token(token)
        , present(true)
    {}

    bool present = true;

    operator bool()
    {
        return present;
    }
};

bool isLexeme(GenAggTypeDecl* def)
{
    for (auto d = def; d; d = d->directBase)
    {
        if (d->name == "Token")
        {
            return true;
        }
    }
    return false;
}

String getLowerName(
    String name)
{
    return String(name.subString(0, 1)).toLower() + name.subString(1, name.getLength() - 1);
}

String getLowerName(
    SyntaxClassDef* def)
{
    return getLowerName(def->name);
}

struct SyntaxDefParser
{
    SyntaxDefParser(Lexer* lexer, DiagnosticSink* sink, SyntaxSharedContext& sharedContext)
        : m_lexer(lexer), m_sink(sink), m_sharedContext(sharedContext)
    {
    }

    Lexer* m_lexer;
    DiagnosticSink* m_sink;
    SyntaxSharedContext& m_sharedContext;


    TokenReader m_tokenReader;

    TokenType peekTokenType()
    {
        return m_tokenReader.peekTokenType();
    }

    OptionalToken advanceIf(TokenType type)
    {
        auto peekToken = m_tokenReader.peekToken();
        if (peekToken.type == type)
        {
            m_tokenReader.advanceToken();
            return peekToken;
        }
        return OptionalToken();
    }

    OptionalToken advanceIf(char const* name)
    {
        auto peekToken = m_tokenReader.peekToken();
        if (peekToken.type == TokenType::Identifier
            && peekToken.getContent() == UnownedTerminatedStringSlice(name))
        {
            m_tokenReader.advanceToken();
            return peekToken;
        }
        return OptionalToken();

    }

    Token readToken()
    {
        return m_tokenReader.advanceToken();
    }

    Token readToken(TokenType type)
    {
        Token token = m_tokenReader.advanceToken();
        if (token.type != type)
        {
            m_sink->diagnose(
                token.loc,
                Diagnostics::unexpectedTokenExpectedTokenType,
                token,
                type);
            fprintf(stderr, "throwing\n");
            throw 99;
            return token;
        }
        return token;
    }

    bool isLineSuccessive(HumaneSourceLoc above, HumaneSourceLoc below)
    {
        return above.line + 1 == below.line;
    }

    RefPtr<SyntaxClassDef> findSyntaxClass(String const& name);
#if 0
    {
        return m_sharedContext.findSyntaxClass(name);
    }
#endif

    RefPtr<GenPatternTerminal> createTerminal(Token token)
    {
        String value;
        switch (token.type)
        {
        case TokenType::StringLiteral:
            value = getStringLiteralTokenValue(token);
            break;

        default:
            value = token.getContent();
            break;
        }

        bool isIdentifier = token.type == TokenType::Identifier;
        if (!isIdentifier && value.getLength() > 0)
        {
            isIdentifier = true;
            for (auto c : value.getUnownedSlice())
            {
                if ((('a' <= c) && (c <= 'z'))
                    || (('A' <= c) && (c <= 'Z'))
                    || (('0' <= c) && (c <= '9'))
                    || (c == '_'))
                {
                    continue;
                }
                else
                {
                    isIdentifier = false;
                    break;
                }
            }
        }

        if (isIdentifier)
        {
            RefPtr<GenPatternKeyword> terminal = new GenPatternKeyword();
            terminal->value = value;
            return terminal;
        }
        else
        {
            RefPtr<GenPatternSimpleTerminal> terminal = new GenPatternSimpleTerminal();
            terminal->value = value;
            return terminal;
        }
    }

    RefPtr<GenPatternTerminal> parseTerminal()
    {
        Token token = readToken();
        return createTerminal(token);
    }

    RefPtr<GenType> parseTypeSuffix(RefPtr<GenType> inType)
    {
        RefPtr<GenType> type = inType;

        if (advanceIf(TokenType::QuestionMark))
        {
            type = new OptionalType(type);
        }
        else
        {
            if (advanceIf(TokenType::Comma))
            {
                type = new CommaSeparatedType(type);
            }

            if (advanceIf(TokenType::OpMul))
            {
                type = new ZeroOrMoreType(type);
            }
            else if (advanceIf(TokenType::OpAdd))
            {
                type = new OneOrMoreType(type);
            }
        }

        return type;
    }

    RefPtr<GenType> parseType()
    {
        RefPtr<GenType> leafType;
        switch (peekTokenType())
        {
        case TokenType::StringLiteral:
            leafType = parseTerminal();
            break;

        default:
            {
                auto nameToken = readToken(TokenType::Identifier);
                auto name = nameToken.getContent();
                leafType = new GenPatternNonterminal(name);
            }
            break;
        }

        return parseTypeSuffix(leafType);
    }

    RefPtr<GenPatternLeaf> parseAttribute()
    {
        Token nameToken = readToken(TokenType::Identifier);
        String name = nameToken.getContent();

        String attributeName;
        RefPtr<GenType> type;
        if (advanceIf(TokenType::Colon))
        {
            attributeName = name;
            type = parseType();
        }
        else
        {
            attributeName = getLowerName(name);

            type = new GenPatternNonterminal(name);
            type = parseTypeSuffix(type);
        }

        RefPtr<GenPatternNamedLeaf> attribute = new GenPatternNamedLeaf(
            attributeName, type);

        return attribute;
    }

    RefPtr<GenPatternPiece> parsePatternItem()
    {
        switch (peekTokenType())
        {
        default:
            {
                auto terminal = parseTerminal();
                return new GenPatternUnnamedLeaf(terminal);
            }

        case TokenType::Dollar:
            readToken();
            return parseAttribute();

        case TokenType::LBrace:
        {
            auto openToken = readToken();

            auto inner = parseSequencePattern();

            auto closeToken = readToken(TokenType::RBrace);

            auto open = createTerminal(openToken);
            auto close = createTerminal(closeToken);

            return new GenPatternEnclosed(
                new GenPatternUnnamedLeaf(open),
                inner,
                new GenPatternUnnamedLeaf(close));
        }
        break;
        }
    }

    RefPtr<GenPatternSeq> parseSequencePattern()
    {
        List<RefPtr<GenPatternPiece>> exprs;

        for (;;)
        {
            switch (peekTokenType())
            {
            case TokenType::EndOfFile:
            case TokenType::Semicolon:
            case TokenType::RBrace:
                return RefPtr<GenPatternSeq>(new GenPatternSeq(exprs));
            }

            auto expr = parsePatternItem();
            exprs.add(expr);
        }


    }

    RefPtr<GenPatternDecl> parsePattern()
    {
        RefPtr<GenPatternPiece> expr;
        switch (peekTokenType())
        {
        case TokenType::LBrace:
            readToken(TokenType::LBrace);
            expr = parseSequencePattern();
            readToken(TokenType::RBrace);
            break;

        default:
            expr = parsePatternItem();
            advanceIf(TokenType::Semicolon);
        }

        RefPtr<GenPatternDecl> result = new GenPatternDecl();
        result->expr = expr;
        return result;
    }

    RefPtr<GenDecl> parseField()
    {
        auto nameToken = readToken(TokenType::Identifier);
        auto name = nameToken.getContent();

        String fieldName;
        RefPtr<GenType> fieldType;
        if (advanceIf(TokenType::Colon))
        {
            fieldName = name;
            fieldType = parseType();
        }
        else
        {
            fieldName = getLowerName(name);

            fieldType = new GenPatternNonterminal(name);
            fieldType = parseTypeSuffix(fieldType);
        }


        RefPtr<GenFieldDecl> field = new GenFieldDecl(
            fieldName, fieldType);

        return field;
    }

    void parseFields(GenContainerDecl* parentDecl)
    {
        List<RefPtr<SyntaxExpr>> exprs;

        for (;;)
        {
            switch (peekTokenType())
            {
            case TokenType::EndOfFile:
            case TokenType::RBrace:
                return;

            case TokenType::Semicolon:
                readToken();
                return;
            }

            auto field = parseField();
            if (field)
                parentDecl->directMembers.add(field);
        }
    }

    RefPtr<GenDecl> lookUp(String name)
    {
        for (auto def : m_sharedContext.moduleDecl->directMembers)
        {
            if (def->name == name)
                return def;
        }
        return nullptr;
    }

    void parseMembers(
        RefPtr<GenContainerDecl> decl)
    {
        for (;;)
        {
            switch (peekTokenType())
            {
            case TokenType::EndOfFile:
            case TokenType::RBrace:
                return;
            }

            auto memberDecl = parseDecl(decl);
            if (memberDecl)
                decl->directMembers.add(memberDecl);
        }
    }

    void parseAggTypeDecl(
        RefPtr<GenAggTypeDecl> decl)
    {
        // read the name
        Token nameToken = readToken(TokenType::Identifier);
        String name = nameToken.getContent();
        decl->name = name;

        // read the (optional) base class
        if (advanceIf(TokenType::Colon))
        {
            Token baseNameToken = readToken(TokenType::Identifier);
            String baseName = baseNameToken.getContent();

            auto base = lookUp(baseName);
            if (auto baseDef = as<GenAggTypeDecl>(base))
            {
                decl->directBase = baseDef;
            }
            else
            {
                fatal(m_sink, "base not right");
            }
        }

        //  read a possible body
        RefPtr<SyntaxExpr> expr;
        RefPtr<SyntaxExpr> fields;
        if (advanceIf(TokenType::LBrace))
        {
            parseMembers(decl);

            readToken(TokenType::RBrace);
        }
        else
        {
            readToken(TokenType::Semicolon);
        }

        // end of definition

        // If this is defining a lexeme, and it has a pattern that
        // consists of a single simple nonterminal, then a
        // definition will have been registered under the text of
        // that nonterminal, and we should re-use it.
        //
#if 0
        RefPtr<SyntaxClassDef> def;
        if (isLexeme(baseDef))
        {
            auto e = expr;
            if (auto seq = as<SequenceExpr>(e))
            {
                if (seq->exprs.getCount() == 1)
                {
                    e = seq->exprs[0];
                }
            }
            if (auto attr = as<AttributeExpr>(e))
            {
                e = attr->expr;
            }
            if (auto terminal = as<SimpleTerminalExpr>(e))
            {
                fprintf(stderr, "found lexeme: %s\n", nameToken.getContent().begin());
                def = terminal->def;

                m_sharedContext.m_mapNameToSyntaxClass.add(
                    nameToken.getContent(), def);
            }
        }

        if (!def)
        {
            def = findSyntaxClass(nameToken.getContent());
        }

        if (def->kind != Def::Kind::Unknown)
        {
            fprintf(stderr, "already seen definition of %s\n", nameToken.getContent().begin());
            throw 99;
        }
#endif

        //
    }

    void readBalancedTokens(List<Token>& tokens)
    {
        for (;;)
        {
            switch (peekTokenType())
            {
            case TokenType::EndOfFile:
            case TokenType::RBrace:
            case TokenType::RBracket:
            case TokenType::RParent:
                return;

            default:
                tokens.add(readToken());
                break;

            case TokenType::LBrace:
                tokens.add(readToken());
                readBalancedTokens(tokens);
                tokens.add(readToken(TokenType::RBrace));
                break;

            case TokenType::LBracket:
                tokens.add(readToken());
                readBalancedTokens(tokens);
                tokens.add(readToken(TokenType::RBracket));
                break;

            case TokenType::LParent:
                tokens.add(readToken());
                readBalancedTokens(tokens);
                tokens.add(readToken(TokenType::RParent));
                break;
            }
        }
    }

    List<Token> readBalancedTokens()
    {
        List<Token> tokens;
        readBalancedTokens(tokens);
        return tokens;
    }

    RefPtr<GenRawDecl> parseRawDecl()
    {

        readToken(TokenType::LBrace);
        auto tokens = readBalancedTokens();
        readToken(TokenType::RBrace);

        RefPtr<GenRawDecl> decl = new GenRawDecl(tokens);

        return decl;
    }

    RefPtr<GenDecl> parseDeclInner(GenContainerDecl* parentDecl)
    {
        if (advanceIf("syntax_class"))
        {
            RefPtr<GenClassDecl> decl = new GenClassDecl();
            parseAggTypeDecl(decl);
            return decl;
        }
        else if (advanceIf("struct"))
        {
            RefPtr<GenStructDecl> decl = new GenStructDecl();
            parseAggTypeDecl(decl);
            return decl;
        }
        else if (advanceIf("pattern"))
        {
            return parsePattern();
        }
        else if (advanceIf("FIELDS"))
        {
            parseFields(parentDecl);
            return nullptr;
        }
        else if (advanceIf("RAW"))
        {
            auto decl = parseRawDecl();
            return decl;
        }
        else
        {
            m_sink->diagnose(m_tokenReader.peekLoc(), Diagnostics::unexpectedToken, m_tokenReader.peekToken());
            return nullptr;
        }
    }

    RefPtr<GenDecl> parseDecl(GenContainerDecl* parentDecl)
    {
        List<RefPtr<GenModifier>> modifiers;

        for (;;)
        {
            if (advanceIf("abstract"))
            {
                modifiers.add(new GenAbstractModifier());
                continue;
            }

            if (advanceIf("hidden"))
            {
                modifiers.add(new GenHiddenModifier());
                continue;
            }

            break;
        }

        auto decl = parseDeclInner(parentDecl);
        if (!decl)
            return nullptr;

        decl->modifiers = modifiers;
        return decl;
    }

    SlangResult parseDefs()
    {
        auto tokens = m_lexer->lexAllSemanticTokens();
        m_tokenReader = TokenReader(tokens);

        RefPtr<GenModuleDecl> moduleDecl = new GenModuleDecl();
        m_sharedContext.moduleDecl = moduleDecl;

        parseMembers(moduleDecl);

        return SLANG_OK;
    }
};

struct GenerationContext
{
public:
    GenerationContext(
        DiagnosticSink* sink,
        SyntaxSharedContext& sharedContext,
        StringBuilder& sbHeader,
        StringBuilder& sbCpp)
        : sink(sink)
        , sharedContext(sharedContext)
        , sbHeader(sbHeader)
        , sbCpp(sbCpp)
    {}

    DiagnosticSink* sink = nullptr;
    SyntaxSharedContext& sharedContext;
    StringBuilder& sbHeader;
    StringBuilder& sbCpp;

    String calculateTypeName(GenType* type)
    {
        while (auto ref = as<GenTypeRef>(type))
            type = ref->decl;

        if (auto classDecl = as<GenClassDecl>(type))
        {
            return classDecl->name + "*";
        }
        else if (auto decl = as<GenAggTypeDecl>(type))
        {
            return decl->name;
        }
        else if (auto keyword = as<GenPatternKeyword>(type))
        {
            return "Identifier";
        }

#if 0
        if (auto terminal = as<SimpleTerminalExpr>(expr))
        {
            return terminal->def->name;
        }
        else if (auto nonterminal = as<NonterminalExpr>(expr))
        {
            return nonterminal->def->name + "*";
        }
#endif
        if (auto optionalExpr = as<OptionalType>(type))
        {
            auto baseName = calculateTypeName(optionalExpr->base);
            return baseName;
        }

        if (auto zeroOrMoreExpr = as<ZeroOrMoreType>(type))
        {
            auto baseName = calculateTypeName(zeroOrMoreExpr->base);
            return "List<" + baseName + ">";
        }

        if (auto oneOrMoreExpr = as<OneOrMoreType>(type))
        {
            auto baseName = calculateTypeName(oneOrMoreExpr->base);
            return "List<" + baseName + ">";
        }

        if (auto commaSeparatedExpr = as<CommaSeparatedType>(type))
        {
            auto baseName = calculateTypeName(commaSeparatedExpr->base);
            return baseName;
        }

        fatal(sink, "calculateTypeName");
    }

    void generateField(
        String attributeName,
        String typeName)
    {
        sbHeader << "    " << typeName << " " << attributeName << ";\n";
    }

    void generateField(
        String attributeName,
        SyntaxExpr* expr)
    {
        String typeName = calculateTypeName(expr);
        generateField(attributeName, typeName);
    }

#if 0
    void generateExprFields(
        SyntaxExpr* expr)
    {
        if (auto seq = as<SequenceExpr>(expr))
        {
            for (auto e : seq->exprs)
            {
                generateExprFields(e);
            }
            return;
        }

        if (auto attr = as<AttributeExpr>(expr))
        {
            generateField(attr->name, attr->expr);
        }
        else if (auto simpleTerminal = as<SimpleTerminalExpr>(expr))
        {
            auto def = simpleTerminal->def;
            generateField(
                getLowerName(def->name),
                def->name);
        }
        else if (auto identTerminal = as<IdentifierTerminalExpr>(expr))
        {
            auto name = identTerminal->value;
            generateField(
                name + "Keyword",
                "Identifier");
        }
        else if (auto enclosed = as<CurlyBraceEnclosedExpr>(expr))
        {
            generateExprFields(enclosed->open);
            generateExprFields(enclosed->inner);
            generateExprFields(enclosed->close);
        }
        else
        {
            sink->diagnose(SourceLoc(), Diagnostics::cannotFindFile, "generateExprFields");
            throw 99;
        }
    }
#endif

#if 0
    SyntaxExpr* getFirstSet(
        SyntaxExpr* expr)
    {
        if (auto terminal = as<TerminalExpr>(expr))
        {
            return terminal;
        }
        else if (auto nonterminal = as<NonterminalExpr>(expr))
        {
            if (auto defExpr = nonterminal->def->expr)
            {
                return getFirstSet(defExpr);
            }
            else
            {
                // this is a hard case!!!
                return nullptr;
            }
        }
        else if (auto sequence = as<SequenceExpr>(expr))
        {
            if (sequence->exprs.getCount() > 0)
            {
                return getFirstSet(sequence->exprs[0]);
            }
        }
        else
        {
            return nullptr;
        }
    }
#endif

    void generateLeafPatternParsingLogic(
        String attributeName,
        GenType* type)
    {
        sbCpp << "    result->" << attributeName << " = ";

#if 0
        if (auto simpleTerminal = as<SimpleTerminalExpr>(type))
        {
            auto def = simpleTerminal->def;
            auto tokenName = def->name;

            sbCpp << "parser->ReadToken(TokenType::" << tokenName << ")";
        }
        else if (auto identTerminal = as<IdentifierTerminalExpr>(type))
        {
            auto name = identTerminal->value;
            sbCpp << "parser->ReadToken(\"" << name << "\")";
        }
        else if (auto nonterminal = as<NonterminalExpr>(type))
        {
            auto name = nonterminal->def->name;
            sbCpp << "parse" << name << "(parser)";
        }
        else
#endif
        {
            // The fallback case is that we will need a subroutine
            // to handle this case...

            bool optional = false;
            bool commaSeparated = false;
            bool plural = false;

            if (auto optionalExpr = as<OptionalType>(type))
            {
                optional = true;
                type = optionalExpr->base;
            }
            else if (auto oneOrMoreExpr = as<OneOrMoreType>(type))
            {
                plural = true;
                type = oneOrMoreExpr->base;
            }
            else if (auto zeroOrMoreExpr = as<ZeroOrMoreType>(type))
            {
                optional = true;
                plural = true;
                type = zeroOrMoreExpr->base;
            }

            if (auto commaSeparatedExpr = as<CommaSeparatedType>(type))
            {
                commaSeparated = true;
                type = commaSeparatedExpr->base;
            }

            auto typeName = calculateTypeName(type);

            sbCpp << "parse";
            if (optional) sbCpp << "Optional";
            if (commaSeparated) sbCpp << "CommaSeparated";
            sbCpp << typeName;
            if (plural) sbCpp << "s";
            sbCpp << "(parser)";
        }

        sbCpp << ";\n";
    }

    void generatePatternParsingLogic(
        GenPatternPiece* piece)
    {
        if (auto seq = as<GenPatternSeq>(piece))
        {
            for (auto e : seq->exprs)
            {
                generatePatternParsingLogic(e);
            }
            return;
        }


        if (auto attr = as<GenPatternNamedLeaf>(piece))
        {
            generateLeafPatternParsingLogic(attr->name, attr->type);
        }
#if 0
        else if (auto simpleTerminal = as<SimpleTerminalExpr>(expr))
        {
            auto def = simpleTerminal->def;
            auto attrName = getLowerName(def->name);
            generateFieldParsingLogic(
                attrName,
                expr);
        }
        else if (auto identTerminal = as<IdentifierTerminalExpr>(expr))
        {
            auto name = identTerminal->value;
            auto attrName = name + "Keyword";
            generateFieldParsingLogic(
                attrName,
                expr);
        }
#endif
        else if (auto enclosed = as<GenPatternEnclosed>(piece))
        {
            // TODO: need to deal with the fact that this is an
            // open/close pair...

            generatePatternParsingLogic(enclosed->open);
            generatePatternParsingLogic(enclosed->inner);
            generatePatternParsingLogic(enclosed->close);
        }
        else
        {
            fatal(sink, "generateFieldParsingLogic");
            throw 99;
        }


#if 0
        if (auto attr = as<AttributeExpr>(expr))
        {
        }


        if (auto terminal = as<TerminalExpr>(expr))
        {
            auto def = sharedContext.findSyntaxClass(terminal->value);
            sbCpp << "contents.xxx = readToken<" << def->name << ">();\n";
        }
        else if (auto nonterminal = as<NonterminalExpr>(expr))
        {
            auto def = nonterminal->def;
            sbCpp << "contents.yyy = parse" << def->name << "();\n";
        }

        // TODO: other cases require observing lookaheads...

        else if (auto optional = as<OptionalExpr>(expr))
        {
            // TODO: get the "first set" of the base of `optional`,
            // and if the lookahead is anything in that set, then
            // we parse the thing.

            auto firstSet = getFirstSet(optional->base);
            if (auto firstSetTerminal = as<TerminalExpr>(firstSet))
            {
                // We have a single nonterminal that can always tell us what
                // to expect next...
                //
                auto firstSetDef = sharedContext.findSyntaxClass(firstSetTerminal->value);
                sbCpp << "if(advanceIf<" << firstSetDef->name << ">())\n";
                sbCpp << "{\n";
                generateFieldParsingLogic(optional->base);
                sbCpp << "}\n";
            }
        }
#endif
    }

    struct FirstSet
    {
        HashSet<SyntaxClassDef*> terminalClasses;
        HashSet<String> keywords;
    };

    bool collectFirstSet(GenPatternPiece* piece, FirstSet& ioFirstSet)
    {
        if (auto seqExpr = as<GenPatternSeq>(piece))
        {
            for (auto e : seqExpr->exprs)
            {
                bool couldBeEmpty = collectFirstSet(e, ioFirstSet);
                if (!couldBeEmpty)
                    return false;
            }
            return true;
        }

#if 0
        if (auto identTerminal = as<IdentifierTerminalExpr>(expr))
        {
            ioFirstSet.keywords.add(identTerminal->value);
            return false;
        }
#endif

        throw 99;
    }

    bool collectFirstSet(GenAggTypeDecl* def, FirstSet& ioFirstSet)
    {
        bool couldBeEmpty = true;

        if (auto pattern = def->findFirstMemberOfType<GenPatternDecl>())
        {
            couldBeEmpty = collectFirstSet(pattern->expr, ioFirstSet);
        }

        // TODO: if there are syntax classes that
        // inherit from this one, we should try to descend
        // into those...

        return couldBeEmpty;
    }

    void generateGuardLogicForOptional(
        SyntaxClassDef* def)
    {
        // Collect all the terminals that could
        // come first...
        //
        FirstSet firstSet;
        bool couldBeEmpty = collectFirstSet(def, firstSet);
        assert(!couldBeEmpty);

        // Now we need to generate the testing logic
        // based on the first set we have...
        auto keywordCount = firstSet.keywords.getCount();
        auto terminalCount = firstSet.terminalClasses.getCount();

        assert(keywordCount || terminalCount);

        if (terminalCount)
        {
            sbCpp << "    switch(peekTokenCode(parser))\n";
            sbCpp << "    {\n";
            sbCpp << "    default:\n";
            sbCpp << "        break;\n\n";

            for (auto t : firstSet.terminalClasses)
            {
                sbCpp << "    case TokenCode::" << t->name << ":\n";
            }

            sbCpp << "        return parse" << def->name << "(parser);\n";
            sbCpp << "    }\n";
        }

        for (auto k : firstSet.keywords)
        {
            sbCpp << "    if(peek(\"" << k << "\")) ";
            sbCpp << "return parse" << def->name << "(parser);\n";
        }

        sbCpp << "\n    return nullptr;\n";
    }

    void generateParsingLogic(
        SyntaxClassDef* def,
        SyntaxAttributeParseMode mode)
    {
        unsigned m = unsigned(mode);

        bool isOptional = (m & unsigned(SyntaxAttributeParseMode::Optional)) != 0;
        bool isPlural = (m & unsigned(SyntaxAttributeParseMode::OneOrMore)) != 0;
        bool isCommaSeparated = (m & unsigned(SyntaxAttributeParseMode::CommaSeparated)) != 0;

        String typeName = def->name + "*";
        if (isCommaSeparated) typeName = "CommaSeparated<" + typeName + ">";
        if (isPlural) typeName = "List<" + typeName + ">";

        sbCpp << "static " << typeName << " parse";
        if (isOptional) sbCpp << "Optional";
        if (isCommaSeparated) sbCpp << "CommaSeparated";
        sbCpp << def->name;
        if (isPlural) sbCpp << "s";
        sbCpp << "(Parser * parser)\n";
        sbCpp << "{\n";

        // need to handle this by cases, because some are way easier than others...

        if (mode == SyntaxAttributeParseMode::Optional)
        {
            generateGuardLogicForOptional(def);
        }
        else
        {
            sbCpp << "    List<" << def->name << "*> result;\n\n";

            if (!isOptional || isCommaSeparated)
            {
                sbCpp << "    auto first = parse" << def->name << "(parser);\n";
                if (isOptional)
                {
                    sbCpp << "    if(!first) return result;\n";
                }
                sbCpp << "    result.add(first);\n\n";
            }

            if (!isCommaSeparated)
            {
                // keep on attempting to parse optionals until we fail
                sbCpp << "    while(auto item = parseOptional" << def->name << "(parser))\n";
                sbCpp << "    {\n";
                sbCpp << "        result.add(item);\n";
                sbCpp << "    }\n";
            }
            else
            {
                sbCpp << "    while(advanceIf(parser, TokenCode::Comma))\n";
                sbCpp << "    {\n";
                sbCpp << "        auto item = parseOptional" << def->name << "(parser);\n";
                sbCpp << "        if(!item) break;\n";
                sbCpp << "    }\n";
            }
            sbCpp << "    return result\n";
        }

        sbCpp << "}\n\n";
    }

    void generateForwardDecl(GenDecl* decl)
    {
        sbHeader << "class " << decl->name << ";\n";
    }

    void generateMemberDecl(
        GenDecl* decl)
    {
        if (auto field = as<GenFieldDecl>(decl))
        {
            auto typeName = calculateTypeName(field->type);
            sbHeader << "    " << typeName << " " << field->name << ";\n";
        }
        else if (auto pattern = as<GenPatternDecl>(decl))
        {
            // TODO: a pattern should produce fields, but
            // that should probably be its own pass, so
            // that we can re-use or refine fields
        }
        else if (auto raw = as<GenRawDecl>(decl))
        {
            for (auto token : raw->tokens)
            {
                if (token.flags & TokenFlag::AtStartOfLine)
                    sbHeader << "\n";
                else if (token.flags & TokenFlag::AfterWhitespace)
                    sbHeader << " ";

                sbHeader << token.getContent();
            }
        }
        else
        {
            fatal(sink, "generateMemberDecl");
        }
    }

    void generateAggTypeDefinition(
        char const* keyword,
        GenAggTypeDecl* def)
    {
        sbHeader << keyword << " " << def->name << "\n";

        if (auto base = def->directBase)
        {
            sbHeader << "    : public " << base->name << "\n";
        }

        if (isLexeme(def))
        {
            sbHeader << "{};\n";
        }
        else
        {
//            auto expr = def->expr;
            sbHeader << "{\n";
            sbHeader << "public:\n";

            for (auto memberDecl : def->directMembers)
                generateMemberDecl(memberDecl);

//            generateExprFields(expr);

            sbHeader << "};\n";
        }
    }

    void generateDefinition(GenDecl* def)
    {
        if (auto classDecl = as<GenClassDecl>(def))
        {
            generateAggTypeDefinition("class", classDecl);
        }
        else
        {
            throw 99;
        }
    }

    void generateParsingLogic(GenDecl* decl)
    {
        auto def = as<GenAggTypeDecl>(decl);
        if (!def)
            return;

        if (isLexeme(def))
            return;

        auto pattern = def->findFirstMemberOfType<GenPatternDecl>();
        if (!pattern)
            return;

        auto expr = pattern->expr;

        // TODO: should skip non-parseable "intermediate" nodes

        sbCpp << "static NodeBase* parse" << def->name
            << "(Parser * parser, void* /*userData*/)\n";
        sbCpp << "{\n";
        sbCpp << "    auto result = parser->astBuilder->create<"
            << def->name << ">();\n";

        generatePatternParsingLogic(expr);

        sbCpp << "    return result;\n";
        sbCpp << "}\n\n";

        unsigned modeBits = def->neededParseModeBitSet >> 1;
        for (unsigned mode = 1; modeBits; ++mode, modeBits >>= 1)
        {
            if (modeBits & 1 == 0)
                continue;

            // need to generate parsing logic for this mode

            generateParsingLogic(def, SyntaxAttributeParseMode(mode));
        }
    }

    void generateDefinitions()
    {
        sbHeader << "// generated code; do not edit\n";
        sbCpp << "// generated code; do not edit\n";

        // start with forward declarations
        for (auto def : sharedContext.moduleDecl->directMembers)
        {
            if (def->findModifier<GenHiddenModifier>())
                continue;

            generateForwardDecl(def);
        }

        // now proper definitions for each type
        for (auto def : sharedContext.moduleDecl->directMembers)
        {
            if (def->findModifier<GenHiddenModifier>())
                continue;

            generateDefinition(def);

        }

        // generate parsing logic
        for (auto def : sharedContext.moduleDecl->directMembers)
        {
            if (def->findModifier<GenHiddenModifier>())
                continue;

            generateParsingLogic(def);
        }
    }
};

SlangResult generateDefinitions(
    DiagnosticSink* sink,
    SyntaxSharedContext& sharedContext,
    StringBuilder& sbHeader,
    StringBuilder& sbCpp)
{
    GenerationContext context(sink, sharedContext, sbHeader, sbCpp);
    context.generateDefinitions();

#if 0
    sbHeader << "enum class CapabilityAtom\n{\n";
    sbHeader << "    Invalid,\n";
    for (auto def : defs)
    {
        if (def->flavor == CapabilityFlavor::Normal)
        {
            sbHeader << "    " << def->name << ",\n";
        }
    }
    sbHeader << "    Count\n";
    sbHeader << "};\n";

    CapabilityDef* firstAbstractDef = nullptr;
    CapabilityDef* firstAliasDef = nullptr;
    sbHeader << "enum class CapabilityName\n{\n";
    sbHeader << "    Invalid,\n";
    Index enumValueCounter = 1;
    List<CapabilityDef*> mapEnumValueToDef;
    mapEnumValueToDef.add(nullptr); // For Invalid.
    for (auto def : defs)
    {
        if (def->flavor == CapabilityFlavor::Normal)
        {
            def->enumValue = enumValueCounter;
            ++enumValueCounter;
            mapEnumValueToDef.add(def);
            sbHeader << "    " << def->name << " = (int)CapabilityAtom::" << def->name << ",\n";
        }
    }
    for (auto def : defs)
    {
        if (def->flavor == CapabilityFlavor::Abstract)
        {
            if (firstAbstractDef == nullptr)
                firstAbstractDef = def;
            def->enumValue = enumValueCounter;
            ++enumValueCounter;
            mapEnumValueToDef.add(def);
            sbHeader << "    " << def->name << ",\n";
        }
    }
    for (auto def : defs)
    {
        if (def->flavor == CapabilityFlavor::Alias)
        {
            if (firstAliasDef == nullptr)
                firstAliasDef = def;
            def->enumValue = enumValueCounter;
            ++enumValueCounter;
            mapEnumValueToDef.add(def);
            sbHeader << "    " << def->name << ",\n";
        }
    }
    sbHeader << "    Count\n";
    sbHeader << "};\n";

    Index targetCount = 0;
    Index stageCount = 0;

    UIntSet anyTargetAtomSet{};
    UIntSet anyStageAtomSet{};
    StringBuilder anyTargetUIntSetHash;
    StringBuilder anyStageUIntSetHash;

    for (auto def : defs)
    {
        if (def->getAbstractBase() == def->sharedContext->ptrOfTarget)
        {
            targetCount++;
            anyTargetAtomSet.add(def->enumValue);
        }
        else if (def->getAbstractBase() == def->sharedContext->ptrOfStage)
        {
            stageCount++;
            anyStageAtomSet.add(def->enumValue);
        }
    }
    outputUIntSetGenerator(
        "generatorOf_kAnyTargetUIntSetBuffer",
        anyTargetUIntSetHash,
        anyTargetAtomSet);
    anyTargetUIntSetHash << "static CapabilityAtomSet kAnyTargetUIntSetBuffer = "
                            "generatorOf_kAnyTargetUIntSetBuffer();\n";
    sbCpp << anyTargetUIntSetHash;

    outputUIntSetGenerator(
        "generatorOf_kAnyStageUIntSetBuffer",
        anyStageUIntSetHash,
        anyStageAtomSet);
    anyStageUIntSetHash << "static CapabilityAtomSet kAnyStageUIntSetBuffer = "
                           "generatorOf_kAnyStageUIntSetBuffer();\n";
    sbCpp << anyStageUIntSetHash;

    sbHeader << "\nenum {\n";
    sbHeader << "    kCapabilityTargetCount = " << targetCount << ",\n";
    sbHeader << "    kCapabilityStageCount = " << stageCount << ",\n";
    sbHeader << "};\n\n";

    calcCanonicalRepresentations(sink, defs, mapEnumValueToDef);

    struct SerializedConjunction
    {
        SerializedConjunction() {}
        SerializedConjunction(const String& initFunctionName, UIntSet& data)
            : m_initFunctionName(initFunctionName), m_data(data)
        {
        }
        String m_initFunctionName;
        UIntSet m_data;
    };
    List<SerializedConjunction> serializedCapabilitesCache;

    List<Index> serializedAtomDisjunctions;
    auto serializeConjunction = [&](const List<CapabilityDef*>& capabilities,
                                    CapabilityDef* parentDef,
                                    Index conjunctionNumber) -> Index
    {
        auto capabilitiesAsUIntSet = atomSetToUIntSet(capabilities);
        // Do we already have a serialized capability array that is the same the one we are trying
        // to serialize?
        for (Index i = 0; i < serializedCapabilitesCache.getCount(); i++)
        {
            auto& existingSet = serializedCapabilitesCache[i].m_data;
            if (existingSet == capabilitiesAsUIntSet)
            {
                return i;
            }
        }
        auto initName =
            "generatorOf_" + parentDef->name + "_conjunction" + String(conjunctionNumber);
        outputUIntSetGenerator(initName, sbCpp, capabilitiesAsUIntSet);

        auto result = serializedCapabilitesCache.getCount();
        serializedCapabilitesCache.add(
            SerializedConjunction(initName + "()", capabilitiesAsUIntSet));
        return result;
    };
    auto serializeDisjunction = [&](const List<Index>& conjunctions) -> SerializedArrayView
    {
        SerializedArrayView result;
        result.first = serializedAtomDisjunctions.getCount();
        for (auto c : conjunctions)
        {
            serializedAtomDisjunctions.add(c);
        }
        result.count = conjunctions.getCount();
        return result;
    };
    for (auto def : defs)
    {
        List<Index> conjunctions;
        for (auto& c : def->canonicalRepresentation)
            conjunctions.add(serializeConjunction(c, def, conjunctions.getCount()));
        def->serializedCanonicalRepresentation = serializeDisjunction(conjunctions);
    }

    sbCpp << "static CapabilityAtomSet kCapabilityArray[] = {\n";
    Index arrayIndex = 0;
    for (Index i = 0; i < serializedCapabilitesCache.getCount(); ++i)
    {
        sbCpp << "    " << serializedCapabilitesCache[i].m_initFunctionName << ",\n";
    }
    sbCpp << "};\n";
    sbCpp << "static CapabilityAtomSet* kCapabilityConjunctions[] = {\n";
    for (auto c : serializedAtomDisjunctions)
    {
        sbCpp << "    kCapabilityArray + " << c << ", \n";
    }
    sbCpp << "};\n";

    sbCpp
        << "static const CapabilityAtomInfo kCapabilityNameInfos[int(CapabilityName::Count)] = {\n";
    for (auto* def : mapEnumValueToDef)
    {
        if (!def)
        {
            sbCpp
                << R"(    { UnownedStringSlice::fromLiteral("Invalid"), CapabilityNameFlavor::Concrete, CapabilityName::Invalid, 0, {nullptr, 0} },)"
                << "\n";
            continue;
        }

        // name.
        sbCpp << "    { UnownedStringSlice::fromLiteral(\"" << def->name << "\"), ";

        // flavor.
        switch (def->flavor)
        {
        case CapabilityFlavor::Normal:
            sbCpp << "CapabilityNameFlavor::Concrete";
            break;
        case CapabilityFlavor::Abstract:
            sbCpp << "CapabilityNameFlavor::Abstract";
            break;
        case CapabilityFlavor::Alias:
            sbCpp << "CapabilityNameFlavor::Alias";
            break;
        }
        sbCpp << ", ";

        // abstract base.
        auto abstractBase = def->getAbstractBase();
        if (abstractBase)
        {
            sbCpp << "CapabilityName::" << abstractBase->name;
        }
        else
        {
            sbCpp << "CapabilityName::Invalid";
        }
        sbCpp << ", ";

        // rank
        sbCpp << def->rank;
        sbCpp << ", ";

        // canonnical representation.
        sbCpp << "{ kCapabilityConjunctions + " << def->serializedCanonicalRepresentation.first
              << ", " << def->serializedCanonicalRepresentation.count << "} },\n";
    }

    sbCpp << "};\n";

    sbCpp << "void freeCapabilityDefs()\n"
          << "{\n"
          << "    for (auto& cap : kCapabilityArray) { cap = CapabilityAtomSet(); }\n"
          << "    kAnyTargetUIntSetBuffer = CapabilityAtomSet();\n"
          << "    kAnyStageUIntSetBuffer = CapabilityAtomSet();\n"
          << "}\n";
#endif
    return SLANG_OK;
}

struct CheckScope
{
public:
    CheckScope(
        GenContainerDecl* container,
        CheckScope* parent)
        : container(container)
        , parent(parent)
    {}

    GenContainerDecl* container = nullptr;
    CheckScope* parent = nullptr;
};

struct CheckContext
{
    CheckContext(
        DiagnosticSink* sink,
        SyntaxSharedContext& shared)
        : sink(sink)
        , shared(shared)
    {}

    DiagnosticSink* sink = nullptr;
    SyntaxSharedContext& shared;

    CheckScope* currentScope = nullptr;

    struct WithScope : public CheckScope
    {
    public:
        WithScope(CheckContext* context, GenContainerDecl* container)
            : CheckScope(container, context->currentScope)
            , _context(context)
        {
            context->currentScope = this;
        }

        ~WithScope()
        {
            _context->currentScope = this->parent;
        }

    private:
        CheckContext* _context = nullptr;
    };

    String deriveFieldNameFromTypeName(String typeName)
    {
        return getLowerName(typeName);
    }

    String deriveFieldName(GenType* type)
    {
        if (!type)
        {
            return "UNKNOWN";
        }
        else if (auto keywordType = as<GenPatternKeyword>(type))
        {
            return keywordType->value + "Keyword";
        }
        else if (auto typeDecl = as<GenAggTypeDecl>(type))
        {
            return deriveFieldNameFromTypeName(typeDecl->name);
        }
        else
        {
            throw 99;
        }
    }

    RefPtr<GenDecl> lookUp(String name)
    {
        for (auto scope = currentScope; scope; scope = scope->parent)
        {
            for (auto d : scope->container->directMembers)
            {
                if (d->name == name)
                    return d;
            }
        }

        sink->diagnose(SourceLoc(), Diagnostics::undefinedIdentifier, name);

        return nullptr;
    }

    RefPtr<GenType> lookUpType(String name)
    {
        auto foundDecl = lookUp(name);
        if (!foundDecl)
            return nullptr;

        if (auto aggTypeDecl = as<GenAggTypeDecl>(foundDecl))
        {
            return new GenTypeRef(aggTypeDecl);
        }
        else
        {
            throw 99;
        }
    }

    RefPtr<GenType> checkType(GenType* type)
    {
        if (auto simpleTerminal = as<GenPatternSimpleTerminal>(type))
        {
            if (auto found = _mapTerminalToType.tryGetValue(simpleTerminal->value))
                return *found;

            fatal(sink, "couldn't find terminal");
        }
        else if (auto nonterminal = as<GenPatternNonterminal>(type))
        {
            auto type = lookUpType(nonterminal->name);
            return type;
        }
        else if (auto zeroOrMoreType = as<ZeroOrMoreType>(type))
        {
            zeroOrMoreType->base = checkType(zeroOrMoreType->base);
            return type;
        }
        else if (auto oneOrMoreType = as<OneOrMoreType>(type))
        {
            oneOrMoreType->base = checkType(oneOrMoreType->base);
            return type;
        }
        else if (auto commaSeparatedType = as<CommaSeparatedType>(type))
        {
            commaSeparatedType->base = checkType(commaSeparatedType->base);
            return type;
        }
        else if (auto optionalType = as<OptionalType>(type))
        {
            optionalType->base = checkType(optionalType->base);
            return type;
        }
        else if (auto keyword = as<GenPatternKeyword>(type))
        {
            return keyword;
        }
        else
        {
            throw 99;
        }
    }

    RefPtr<GenPatternLeaf> checkPatternLeaf(RefPtr<GenPatternLeaf> leaf)
    {
        if (auto unnamed = as<GenPatternUnnamedLeaf>(leaf))
        {
            // we should be able to resolve the type and synthesize
            // a name for it...
            //
            auto type = checkType(unnamed->type);
            auto name = deriveFieldName(type);

            return new GenPatternNamedLeaf(
                name, type);
        }
        else if (auto named = as<GenPatternNamedLeaf>(leaf))
        {
            named->type = checkType(named->type);
            return named;
        }
        else
        {
            throw 99;
        }
    }

    RefPtr<GenPatternPiece> checkPatternPiece(RefPtr<GenPatternPiece> piece)
    {
        if (auto leaf = as<GenPatternLeaf>(piece))
        {
            return checkPatternLeaf(leaf);
        }
        if (auto seq = as<GenPatternSeq>(piece))
        {
            for (auto& item : seq->exprs)
            {
                item = checkPatternPiece(item);
            }
            return seq;
        }
        else if (auto enclosed = as<GenPatternEnclosed>(piece))
        {
            enclosed->open = checkPatternLeaf(enclosed->open);
            enclosed->inner = checkPatternPiece(enclosed->inner);
            enclosed->close = checkPatternLeaf(enclosed->close);
            return enclosed;
        }
        else
        {
            throw 99;
        }
    }

    Dictionary<String, RefPtr<GenType>> _mapTerminalToType;

    void check(GenDecl* decl)
    {
        if (auto aggTypeDecl = as<GenAggTypeDecl>(decl))
        {
            // As a very special case, we need to register
            // all the terminals for lookup...

            if (isLexeme(aggTypeDecl))
            {
//                fprintf(stderr, "found: %s\n", decl->name.getBuffer());

                if (auto pattern = aggTypeDecl->findFirstMemberOfType<GenPatternDecl>())
                {
                    auto p = pattern->expr;
                    if (auto seq = as<GenPatternSeq>(p))
                        if (seq->exprs.getCount() == 1)
                            p = seq->exprs[0];

                    if (auto leaf = as<GenPatternUnnamedLeaf>(p))
                    {
                        if (auto term = as<GenPatternSimpleTerminal>(leaf->type))
                        {
                            //

                            _mapTerminalToType.add(term->value, aggTypeDecl);
                        }
                    }
                }
            }

            WithScope typeScope(this, aggTypeDecl);
            checkMembers(aggTypeDecl);
        }
        else if (auto patternDecl = as<GenPatternDecl>(decl))
        {
            patternDecl->expr = checkPatternPiece(patternDecl->expr);
        }
        else if (auto field = as<GenFieldDecl>(decl))
        {
            field->type = checkType(field->type);
        }
        else if (as<GenRawDecl>(decl))
        {}
        else
        {
            throw 99;
        }
    }

    void checkMembers(GenContainerDecl* decl)
    {
        for (auto memberDecl : decl->directMembers)
            check(memberDecl);
    }

    void check(GenModuleDecl* moduleDecl)
    {
        WithScope moduleScope(this, moduleDecl);
        checkMembers(moduleDecl);
    }
};

void check(
    DiagnosticSink* sink,
    SyntaxSharedContext& sharedContext)
{
    CheckContext context(sink, sharedContext);
    context.check(sharedContext.moduleDecl);
}

SlangResult parseDefFile(
    DiagnosticSink* sink,
    String inputPath,
    SyntaxSharedContext& sharedContext)
{
    auto sourceManager = sink->getSourceManager();

    String contents;
    SLANG_RETURN_ON_FAIL(File::readAllText(inputPath, contents));
    PathInfo pathInfo = PathInfo::makeFromString(inputPath);
    SourceFile* sourceFile = sourceManager->createSourceFileWithString(pathInfo, contents);
    SourceView* sourceView = sourceManager->createSourceView(sourceFile, nullptr, SourceLoc());
    Lexer lexer;
    lexer.initialize(sourceView, sink, &sharedContext.namePool, sourceManager->getMemoryArena());

    SyntaxDefParser parser(&lexer, sink, sharedContext);

    SLANG_RETURN_ON_FAIL(parser.parseDefs());
    return SLANG_OK;
}

void printDiagnostics(DiagnosticSink* sink)
{
    ComPtr<ISlangBlob> blob;
    sink->getBlobIfNeeded(blob.writeRef());
    if (blob)
    {
        fprintf(stderr, "%s", (const char*)blob->getBufferPointer());
    }
}

void writeIfChanged(String fileName, String content)
{
    if (File::exists(fileName))
    {
        String existingContent;
        File::readAllText(fileName, existingContent);
        if (existingContent.getUnownedSlice().trim() == content.getUnownedSlice().trim())
            return;
    }
    File::writeAllText(fileName, content);
}

int main(int argc, const char* const* argv)
{
    if (argc < 1)
    {
        fprintf(stderr, "Usage: %s\n", argc >= 1 ? argv[0] : "slang-ast-generator");
        return 1;
    }
    String targetDir;
    for (int i = 0; i < argc - 1; i++)
    {
        if (strcmp(argv[i], "--target-directory") == 0)
            targetDir = argv[i + 1];
    }

    String inPath = argv[1];
    if (targetDir.getLength() == 0)
        targetDir = Path::getParentDirectory(inPath);

    auto outCppPath = Path::combine(targetDir, "slang-generated-ast-defs-impl.h");
    auto outHeaderPath = Path::combine(targetDir, "slang-generated-ast-defs.h");
    auto outLookupPath = Path::combine(targetDir, "slang-lookup-ast-defs.cpp");
    SourceManager sourceManager;
    sourceManager.initialize(nullptr, OSFileSystem::getExtSingleton());
    DiagnosticSink sink(&sourceManager, nullptr);
    SyntaxSharedContext syntaxSharedContext;

    StringBuilder sbHeader, sbCpp;
    try
    {
        if (SLANG_FAILED(parseDefFile(&sink, inPath, syntaxSharedContext)))
        {
            fprintf(stderr, "failure\n");
            printDiagnostics(&sink);
            return 1;
        }

        check(&sink, syntaxSharedContext);
        if (sink.getErrorCount() > 0)
        {
            printDiagnostics(&sink);
            return 1;
        }

        if (SLANG_FAILED(generateDefinitions(&sink, syntaxSharedContext, sbHeader, sbCpp)))
        {
            printDiagnostics(&sink);
            return 1;
        }
    }
    catch(...)
    {
        fprintf(stderr, "caught\n");
    }


    fprintf(stderr, "GENERATED: %s \n %s\n", sbHeader.produceString().getBuffer(), sbCpp.produceString().getBuffer());

    writeIfChanged(outHeaderPath, sbHeader.produceString());
    writeIfChanged(outCppPath, sbCpp.produceString());

#if 0
    List<String> opnames;
    for (auto def : syntaxSharedContext.m_defs)
    {
        opnames.add(def->name);
    }

    if (SLANG_FAILED(writePerfectHashLookupCppFile(
            outLookupPath,
            opnames,
            "CapabilityName",
            "CapabilityName::",
            "slang-ast.h",
            &sink)))
    {
        printDiagnostics(&sink);
        return 1;
    }
#endif
    printDiagnostics(&sink);
    return 0;
}
