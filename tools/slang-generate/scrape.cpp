// scrape.cpp
#include "scrape.h"

namespace slang_generate
{

    void diagnoseError(SourceLoc loc, char const* message, ...)
    {
        throw 99;
    }

    // Token

    #define FOREACH_KEYWORD(X)  \
        X(AlignAs,          alignas)     \
        X(AlignOf,          alignof)\
        X(Asm,              asm)\
        X(Auto,             auto)\
        X(Bool,             bool)\
        X(Break,            break)\
        X(Case,             case)\
        X(Catch,            catch)\
        X(Char,             char)\
        X(Char16T,          char16_t)\
        X(Char32T,          char32_t)\
        X(Class,            class)\
        X(Const,            const)\
        X(ConstExpr,        constexpr)\
        X(ConstCast,        const_cast)\
        X(Continue,         continue)\
        X(DeclType,         decltype)\
        X(Default,          default)\
        X(Delete,           delete)\
        X(Do,               do)\
        X(Double,           double)\
        X(DynamicCast,      dynamic_cast)\
        X(Else,             else)\
        X(Enum,             enum)\
        X(Explicit,         explicit)\
        X(Export,           export)\
        X(Extern,           extern)\
        X(False,            false)\
        X(Float,            float)\
        X(For,              for)\
        X(Friend,           friend)\
        X(Goto,             goto)\
        X(If,               if)\
        X(Inline,           inline)\
        X(Int,              int)\
        X(Long,             long)\
        X(Mutable,          mutable)\
        X(Namespace,        namespace)\
        X(New,              new)\
        X(NoExcept,         noexcept)\
        X(NullPtr,          nullptr)\
        X(Operator,         operator)\
        X(Private,          private)\
        X(Protected,        protected)\
        X(Public,           public)\
        X(Register,         register)\
        X(ReinterpretCast,  reinterpret_cast)   \
        X(Return,           return)             \
        X(Short,            short)              \
        X(Signed,           signed)             \
        X(SizeOf,           sizeof)             \
        X(Static,           static)             \
        X(StaticAssert,     static_assert)      \
        X(StaticCast,       static_cast)        \
        X(Struct,           struct)             \
        X(Switch,           switch)             \
        X(Template,         template)           \
        X(This,             this)               \
        X(ThreadLocal,      thread_local)       \
        X(Throw,            throw)              \
        X(True,             true)               \
        X(Try,              try)                \
        X(TypeDef,          typedef)            \
        X(TypeID,           typeid)             \
        X(TypeName,         typename)           \
        X(Union,            union)              \
        X(Unsigned,         unsigned)           \
        X(Using,            using)              \
        X(Virtual,          virtual)            \
        X(Void,             void)               \
        X(Volatile,         volatile)           \
        X(WCharT,           wchar_t)            \
        X(While,            while)              \
        /* end */


    #define FOREACH_TOKEN(X)    \
        X(Invalid,              "invalid token")    \
        X(EndOfFile,            "end of file")      \
        X(LineComment,          "line comment")     \
        X(BlockComment,         "block comment")    \
        X(HorizontalWhitespace, "whitespace")       \
        X(Newline,              "newline")          \
        X(Identifier,           "identifier")       \
        X(IntegerLiteral,       "integer literal")  \
        X(FloatingPointLiteral, "floating-point literal")   \
        X(StringLiteral,        "string literal")           \
        X(CharacterLiteral,     "character literal")        \
        X(LCurly,               "'{'")                      \
        X(RCurly,               "'}'")                      \
        X(LParen,               "'('")                      \
        X(RParen,               "')'")                      \
        X(LSquare,              "'['")                      \
        X(RSquare,              "']'")                      \
        X(Semi,                 "';'")                      \
        X(Colon,                "':'")                      \
        X(ColonColon,           "'::'")                     \
        X(Dot,                  "'.'")                      \
        X(Question,             "'?'")                      \
        /* end */

    enum class TokenCode
    {
    #define TOKEN(NAME, DESC) \
        NAME,
    FOREACH_TOKEN(TOKEN)
    #undef TOKEN

    #define KEYWORD(NAME, TEXT) \
        NAME,
    FOREACH_KEYWORD(KEYWORD)
    #undef KEYWORD
    };

    struct Token
    {
        TokenCode code = TokenCode::Invalid;
        SourceLoc loc;
        UnownedStringSlice text;

        operator TokenCode() { return code; }
    };

    //

    struct NamePool
    {
        struct Entry
        {
            Entry* next;
            Name name;
        };
        Entry* entries = nullptr;
    };

    Name* getName(NamePool* namePool, UnownedStringSlice const& text)
    {
        for (auto entry = namePool->entries; entry; entry = entry->next)
        {
            if (entry->name.text == text)
                return &entry->name;
        }

        size_t textSize = text.size();
        NamePool::Entry* entry = (NamePool::Entry*)malloc(sizeof(NamePool::Entry) + textSize + 1);
        Name* name = &entry->name;
        char* buffer = (char*)(entry + 1);

        memcpy(buffer, text.begin(), textSize);
        buffer[textSize] = 0;

        name->text = UnownedTerminatedStringSlice(buffer, buffer + textSize);
        entry->next = namePool->entries;

        namePool->entries = entry;

        return name;
    }

    // Lexer

    struct Lexer
    {
        char const* cursor;
        char const* end;

        SourceLoc loc;
    };

    int peek(Lexer* lexer)
    {
        if (lexer->cursor == lexer->end)
            return -1;

        return *lexer->cursor;
    }

    int get(Lexer* lexer)
    {
        if (lexer->cursor == lexer->end)
            return -1;

        return *lexer->cursor++;
    }

    bool isDigit(int c)
    {
        return (c >= '0') && (c <= '9');
    }

    bool isIdentifierStartChar(int c)
    {
        return ((c >= 'a') && (c <= 'z'))
            || ((c >= 'A') && (c <= 'Z'))
            || (c == '_');
    }

    bool isIdentifierChar(int c)
    {
        return isIdentifierStartChar(c) || isDigit(c);
    }

    TokenCode lexIdentifier(Lexer* lexer)
    {
        for (;;)
        {
            int c = peek(lexer);
            if (!isIdentifierChar(c))
                break;

            get(lexer);
        }
        return TokenCode::Identifier;
    }

    void lexDigits(Lexer* lexer, int radix)
    {
        for (;;)
        {
            int c = peek(lexer);
            int digitVal = -1;
            switch (c)
            {
            case '_':
                get(lexer);
                continue;

            case '0': case '1': case '2': case '3': case '4':
            case '5': case '6': case '7': case '8': case '9':
                digitVal = c - '0';
                break;

            case 'A': case 'B': case 'C': case 'D': case 'E': case 'F':
                if (radix > 10)
                {
                    digitVal = 10 + (c - 'A');
                }
                break;

            case 'a': case 'b': case 'c': case 'd': case 'e': case 'f':
                if (radix > 10)
                {
                    digitVal = 10 + (c - 'a');
                }
                break;

            default:
                break;
            }

            if (digitVal < 0)
                break;

            if (digitVal >= radix)
            {
                diagnoseError(lexer->loc, "invalid digit in base-%d number", radix);
            }

            get(lexer);
        }
    }

    TokenCode lexNumberLiteral(Lexer* lexer, int radix)
    {
        TokenCode code = TokenCode::IntegerLiteral;

        // Leading digits
        lexDigits(lexer, radix);

        // TODO: need to handle floating-point cases

        // TODO: need to handle suffixes

        return code;

    }

    TokenCode lexTokenImpl(Lexer* lexer)
    {
        int c = get(lexer);
        switch (c)
        {
        case -1:
            return TokenCode::EndOfFile;

        case '0':
            switch(int d = peek(lexer))
            {
            case 'x': case 'X':
                return lexNumberLiteral(lexer, 16);

            case 'b': case 'B':
                return lexNumberLiteral(lexer, 2);

            case '0': case '1': case '2': case '3': case '4':
            case '5': case '6': case '7': case '8': case '9':
            case '_':
                return lexNumberLiteral(lexer, 8);

            default:
                return lexNumberLiteral(lexer, 10);
            }

        default:
            break;
        }

        if (isIdentifierStartChar(c))
        {
            return lexIdentifier(lexer);
        }

        diagnoseError(lexer->loc, "invalid byte 0x%02x", c);
        return TokenCode::Invalid;
    }

    Token lexToken(Lexer* lexer)
    {
        for (;;)
        {
            Token token;
            token.loc = lexer->loc;

            char const* begin = lexer->cursor;
            token.code = lexTokenImpl(lexer);
            char const* end = lexer->cursor;
            token.text = UnownedStringSlice(begin, end);

            switch (token.code)
            {
            case TokenCode::Invalid:
            case TokenCode::HorizontalWhitespace:
            case TokenCode::Newline:
                continue;

            default:
                break;
            }

            return token;
        }
    }

    // Parser

    struct Parser
    {
        NamePool*   namePool;
        Lexer*      lexer;
        Token       token;
    };

    Token peek(Parser* parser)
    {
        return parser->token;
    }

    bool peek(Parser* parser, TokenCode code)
    {
        return parser->token.code == code;
    }

    SourceLoc peekLoc(Parser* parser)
    {
        return parser->token.loc;
    }

    Token advance(Parser* parser)
    {
        Token result = parser->token;
        parser->token = lexToken(parser->lexer);
        return result;
    }

    void unexpected(Parser* parser, char const* expected)
    {
        diagnoseError(peekLoc(parser), "unexpected token, expected %s", expected);
    }

    void unexpected(Parser* parser, TokenCode expected)
    {
        diagnoseError(peekLoc(parser), "unexpected token");
    }

    Token expect(Parser* parser, TokenCode code)
    {
        if (peek(parser) == code)
        {
            return advance(parser);
        }
        unexpected(parser, code);
        return Token();
    }

    Name* expectIdentifier(Parser* parser)
    {
        if (peek(parser) == TokenCode::Identifier)
        {
            Token token = advance(parser);

            return getName(parser->namePool, token.text);
        }
        expect(parser, TokenCode::Identifier);
        return nullptr;
    }

    Expr* parsePrimaryExpr(Parser* parser)
    {
        switch (peek(parser))
        {
        case TokenCode::Identifier:
            {
                NameExpr* expr = new NameExpr();
                expr->loc = peekLoc(parser);
                expr->name = expectIdentifier(parser);
                return expr;
            }
            break;

        case TokenCode::IntegerLiteral:
            {
                IntegerLiteralExpr* expr = new IntegerLiteralExpr();
                expr->loc = peekLoc(parser);
                expr->text = advance(parser).text;
                return expr;
            }

        case TokenCode::FloatingPointLiteral:
            {
                FloatingPointLiteralExpr* expr = new FloatingPointLiteralExpr();
                expr->loc = peekLoc(parser);
                expr->text = advance(parser).text;
                return expr;
            }

        case TokenCode::StringLiteral:
            {
                StringLiteralExpr* expr = new StringLiteralExpr();
                expr->loc = peekLoc(parser);
                expr->text = advance(parser).text;
                return expr;
            }

        case TokenCode::CharacterLiteral:
            {
                CharacterLiteralExpr* expr = new CharacterLiteralExpr();
                expr->loc = peekLoc(parser);
                expr->text = advance(parser).text;
                return expr;
            }

        case TokenCode::LParen:
            {
                expect(parser, TokenCode::LParen);
                // TODO: need to disambiguate cast here
                expect(parser, TokenCode::RParen);
        }
            break;

        default:
            unexpected(parser, "an expression");
            return nullptr;
        }
    }


    TypeRepr* parseTypeSpec(Parser* parser)
    {
        switch (peek(parser))
        {
        case TokenCode::Identifier:
            {
                NamedTypeRepr* namedType = new NamedTypeRepr();
                namedType->name = expectIdentifier(parser);
                return namedType;
            }
            break;

        default:
            unexpected(parser, "a type");
            return nullptr;
        }
    }

    Declarator* parseDeclarator(Parser* parser)
    {

    }

    Decl* parseDeclaratorDecl(Parser* parser)
    {
        TypeRepr* typeSpec = parseTypeSpec(parser);

        Declarator* declator = parseDeclarator(parser);

        // parse declarator

        // parse more...
    }

    Decl* parseDecl(Parser* parser)
    {
        switch (peek(parser))
        {
        case TokenCode::EndOfFile:
        case TokenCode::RCurly:
        case TokenCode::RParen:
        case TokenCode::RSquare:
            unexpected(parser, "a declaration");
            break;

        default:
            return parseDeclaratorDecl(parser);
        }
    }

    FileDecl* parseFileDecl(Parser* parser)
    {
        FileDecl* fileDecl = new FileDecl();
        fileDecl->loc = peekLoc(parser);
        for (;;)
        {
            if (peek(parser, TokenCode::EndOfFile))
                break;

            Decl* decl = parseDecl(parser);
            fileDecl->decls.Add(decl);
        }
        return fileDecl;
    }

    //

    struct ScrapeContext
    {

    };

    FileDecl* scrapeFile(
        ScrapeContext*  context,
        char const*     path)
    {
        // First, figure out what kind of file we are dealing with,
        // by looking at its extension.

        // Slurp the content of the file into a buffer

        Lexer lexer;

        Parser parser;
        parser.lexer = &lexer;

        FileDecl* fileDecl = parseFileDecl(&parser);
        expect(&parser, TokenCode::EndOfFile);

        return fileDecl;
    }

    FileDecl* scrapeFile(char const* path)
    {
        ScrapeContext context;
        return scrapeFile(&context, path);
    }
}
