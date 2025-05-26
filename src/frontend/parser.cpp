module lpc.frontend.parser;

import std;
import lpc.logging;
import lpc.frontend.token;
import lpc.frontend.ast;

namespace lpc::frontend {

// clang-format off
namespace rules {

#define DEF_RULE_BEGIN(R)                                                      \
    constexpr auto R::rule() noexcept {                                        \
        return make_node<NodeType::R>(

#define DEF_RULE_END(R)                                                        \
            );                                                                 \
    }

#define DEFTOKEN(T) constexpr const OneToken<TokenType::T>(T)

DEFTOKEN(LPAREN);
DEFTOKEN(RPAREN);

template<ParserRule R>
constexpr auto List = [] () noexcept {
    return make_node<NodeType::List>(
        chain(
            !LPAREN
          , R() 
          , Many(R())
          , OneToken<TokenType::DOT>()
          , R()
          , !RPAREN
        )
    );
};


template<ParserRule R>
constexpr auto Vector = [] () noexcept {
    return make_node<NodeType::Vector>(
        chain(
            !OneToken<TokenType::SHELL_LPAREN>()
          , R()
          , !RPAREN
        )
    );
};

constexpr const auto GetIdentifier
    = []() noexcept { return any(GetVariable(), GetKeyword()); };

constexpr const auto Ellipsis = []() noexcept {
    return chain(
        When<OneVariable<hash_string("...")>>(),
        GetVariable() // TODO Bad solution
    );
};
// 5.1 Programs
// A program is a sequence of expressions, definitions,
// and syntax definitions.
DEF_RULE_BEGIN(Program)
Many(~Def<ExprOrDef>())
DEF_RULE_END(Program)

DEF_RULE_BEGIN(ExprOrDef)
any(
    ~Def<Definition>()
  , Def<SyntaxDefinition>()
  , Def<Expression>()
  , chain(
        !LPAREN
      , !OneKeyword<Keyword::BEGIN>()
      , ~Def<ExprOrDef>()
      , Many(~Def<ExprOrDef>())
      , !RPAREN
    )
)
DEF_RULE_END(ExprOrDef)

// (4.) (7.1.3.) Expressions
DEF_RULE_BEGIN(Expression)
any(
    GetVariable()
  , ~Def<Literal>()
  , Def<ProcedureCall>()
  , Def<Lambda>()
  , Def<If>()
  , Def<Assignment>()
  // TODO Quasiquote
  , Def<MacroUse>()
  , Def<MacroBlock>()
)
DEF_RULE_END(Expression)

// (4.1.2) a literal
DEF_RULE_BEGIN(Literal)
any(
    Def<Quotation>()
  , GetConstant()
)
DEF_RULE_END(Literal)

DEF_RULE_BEGIN(Quotation)
any(
    chain(
        !OneToken<TokenType::APOSTROPHE>(),
        Def<Datum>()
    )
  , chain(
        !LPAREN
      , !OneKeyword<Keyword::QUOTE>(),
        Def<Datum>()
      , !RPAREN
    )
)
DEF_RULE_END(Quotation)

// (4.1.3) Procedure Call
DEF_RULE_BEGIN(ProcedureCall)
chain(
    !LPAREN
  , Def<Expression>()
  , Many(Def<Expression>())
  , !RPAREN
)
DEF_RULE_END(ProcedureCall)

// (4.1.4) Procedures
DEF_RULE_BEGIN(Lambda)
chain(
    !LPAREN
  , !OneKeyword<Keyword::LAMBDA>()
  , Def<Formals>()
  , Def<Body>()
  , !RPAREN
)
DEF_RULE_END(Lambda)

DEF_RULE_BEGIN(Formals)
any(
    GetVariable()
  , chain(
        !LPAREN
      , Many(GetVariable())
      , !RPAREN
    )
  , List<GetVariable>()
)
DEF_RULE_END(Formals)

DEF_RULE_BEGIN(Body)
chain(
    Many(Def<Definition>())
  , Def<Sequence>()
)
DEF_RULE_END(Body)

DEF_RULE_BEGIN(Sequence)
chain(
    Def<Expression>()
  , Many(Def<Expression>()) 
)
DEF_RULE_END(Sequence)

// Conditional
DEF_RULE_BEGIN(If)
chain(
    !LPAREN
  , !OneKeyword<Keyword::IF>()
  , Def<Expression>()         // test
  , Def<Expression>()         // consequent
  , Maybe(Def<Expression>())  // alternate
  , !RPAREN
)
DEF_RULE_END(If)

// Assignment
DEF_RULE_BEGIN(Assignment)
chain(
    !LPAREN
  , !OneKeyword<Keyword::SET>()
  , GetVariable()           // variable
  , Def<Expression>()         // value
  , !RPAREN
)
DEF_RULE_END(Assignment)

DEF_RULE_BEGIN(MacroUse)
chain(
    !LPAREN
  , GetIdentifier()
  , Many(Def<Expression>())   // arguments
  , !RPAREN
)
DEF_RULE_END(MacroUse)

DEF_RULE_BEGIN(MacroBlock)
chain(
    !LPAREN
  , any(
        Def<LetSyntax>()
      , Def<LetRecSyntax>()
    )
  , !LPAREN
  , Many(Def<SyntaxSpec>())
  , !RPAREN
  , Def<Body>()
  , !RPAREN
)
DEF_RULE_END(MacroBlock)

DEF_RULE_BEGIN(LetSyntax)
!OneVariable<hash_string("let-syntax")>()
DEF_RULE_END(LetSyntax)

DEF_RULE_BEGIN(LetRecSyntax)
!OneVariable<hash_string("letrec-syntax")>()
DEF_RULE_END(LetRecSyntax)

// Syntax Specification
DEF_RULE_BEGIN(SyntaxSpec)
chain(
    !LPAREN
  , GetVariable()
  , Def<TransformerSpec>()
  , Many(Def<Expression>())
  , !RPAREN
)
DEF_RULE_END(SyntaxSpec)


// 5.2 Definitions
DEF_RULE_BEGIN(Definition)
any(
    Def<Define>()
  , chain(
        !LPAREN
      , !OneKeyword<Keyword::BEGIN>()
      , Many(Def<Define>())
      , !RPAREN
    )
)
DEF_RULE_END(Definition)

DEF_RULE_BEGIN(Define)
chain(
    !LPAREN
  , !OneKeyword<Keyword::DEFINE>()
  , any(
        chain(
            GetVariable()
          , Def<Expression>()
        )
      , chain(
            any(
                chain(
                    !LPAREN
                    , GetVariable()
                    , !RPAREN
                ),
                List<GetVariable>()
            )
          , Def<Body>()
        )
    )
  , !RPAREN
)
DEF_RULE_END(Define)

// 5.3 Syntax Definitions
DEF_RULE_BEGIN(SyntaxDefinition)
chain(
    !LPAREN
  , !OneVariable<hash_string("define-syntax")>()
  , GetIdentifier()
  , Def<TransformerSpec>()
  , !RPAREN
)
DEF_RULE_END(SyntaxDefinition)

DEF_RULE_BEGIN(Datum)
any(
    GetConstant()
  , GetIdentifier()
  , chain(
        !LPAREN
      , Many(~Def<Datum>())
      , !RPAREN
    )
  , List<Flatten<Def<Datum>>>()
  , chain(
        any(
            OneToken<TokenType::APOSTROPHE>()  // TODO Won't work
          , OneToken<TokenType::BACKTICK>()
          , OneToken<TokenType::COMMA>()
          , OneToken<TokenType::COMMA_AT>()
        )
      , ~Def<Datum>()
    )
  , chain(
        OneToken<TokenType::SHELL_LPAREN>()
      , Many(~Def<Datum>())
      , RPAREN
    )
)
DEF_RULE_END(Datum)

DEF_RULE_BEGIN(TransformerSpec)
chain(
    !LPAREN
  , !OneVariable<hash_string("syntax-rules")>()
  , !LPAREN
  , Many(GetIdentifier())
  , !RPAREN
  , Many(Def<SyntaxRule>())
  , !RPAREN
)
DEF_RULE_END(TransformerSpec)

DEF_RULE_BEGIN(SyntaxRule)
chain(
    !LPAREN
  , Def<Pattern>()
  , Def<Template>()
  , !RPAREN
)
DEF_RULE_END(SyntaxRule)

DEF_RULE_BEGIN(Pattern)
any(
    chain(
        Not<OneVariable<hash_string("...")>>()
      , GetVariable()
    )
  , chain(
        !LPAREN
      , Many(~Def<Pattern>())
      , !RPAREN
    )
  , List<Flatten<Def<Pattern>>>()
  , chain(
        !LPAREN
      , ~Def<Pattern>()
      , Many(~Def<Pattern>())
      , Ellipsis()
      , !RPAREN
    )
  , Vector<Many<Flatten<Def<Pattern>>>>()
  , Vector<decltype(
        chain(
          Many(~Def<Pattern>())
          , Ellipsis()
        )
    )>()
  , GetConstant()
)
DEF_RULE_END(Pattern)

constexpr const auto TemplateElement = []() noexcept {
    return chain(
        ~Def<Template>()
      , Maybe(Ellipsis())
    );
};

DEF_RULE_BEGIN(Template)
any(
    chain(
        Not<OneVariable<hash_string("...")>>()
      , GetVariable()
    )
  , chain(
        !LPAREN
      , Many(TemplateElement())
      , !RPAREN
    )
  , make_node<NodeType::List>(
        chain(
            !LPAREN
          , TemplateElement()
          , Many(TemplateElement())
          , OneToken<TokenType::DOT>()
          , ~Def<Template>()
          , !RPAREN
        )
    )
  , Vector<Many<Flatten<Def<Template>>>>()
  , GetConstant()
)
DEF_RULE_END(Template)

} // namespace rules
// clang-format on

void Parser::parse() noexcept {
    auto rule = rules::Program::rule();
    _root = std::move(rule(_cursor).value()[0]);
    if (!_cursor.is_eof()) {
        Error("Unexpected tokens after parsing the root node");
        _cursor.fail();
        return;
    }
}

template <TokenType T>
constexpr bool Cursor::is() const noexcept {
    return _token->type() == T;
}

template <Keyword K>
constexpr bool Cursor::is() const noexcept {
    return _token->type() == TokenType::KEYWORD
        && std::get<Keyword>(_token->value()) == K;
}

template <std::size_t Hash>
constexpr bool Cursor::is() const noexcept {
    if (_token->type() != TokenType::IDENT)
        return false;
    auto string_hash = [](const std::string& str) noexcept {
        std::size_t h = 14695981039346656037ULL;
        for (const char& it : str) {
            h ^= static_cast<std::size_t>(it);
            h *= 1099511628211ULL;
        }
        return h;
    };
    return string_hash(std::get<std::string>(_token->value())) == Hash;
}

constexpr OptNodePtr Cursor::get_keyword() const noexcept {
    if (_token->type() != TokenType::KEYWORD)
        return std::nullopt;
    auto keyword = std::get<Keyword>(_token->value());
    return std::make_unique<Node>(
        NodeType::Keyword, _token->location(), keyword);
}

constexpr OptNodePtr Cursor::get_ident() const noexcept {
    if (_token->type() != TokenType::IDENT)
        return std::nullopt;
    auto ident = std::get<std::string>(_token->value());
    return std::make_unique<Node>(
        NodeType::Variable, _token->location(), std::move(ident));
}

constexpr OptNodePtr Cursor::get_constant() const noexcept {
    OptNodePtr ptr;
    switch (_token->type()) {
    case TokenType::NUMBER: {
        auto value = std::get<std::int64_t>(_token->value());
        ptr = std::make_unique<Node>(
            NodeType::Number, _token->location(), value);
        break;
    }
    case TokenType::BOOLEAN: {
        auto value = std::get<bool>(_token->value());
        ptr = std::make_unique<Node>(
            NodeType::Boolean, _token->location(), value);
        break;
    }
    case TokenType::CHARACTER: {
        auto value = std::get<char>(_token->value());
        ptr = std::make_unique<Node>(
            NodeType::Character, _token->location(), value);
        break;
    }
    case TokenType::STRING: {
        auto value = std::get<std::string>(_token->value());
        ptr = std::make_unique<Node>(
            NodeType::String, _token->location(), std::move(value));
        break;
    }
    default: return std::nullopt;
    }
    return ptr;
}
} // namespace lpc::frontend

namespace lpc::frontend::combinators {

template <TokenType T>
OptNodeList OneToken<T>::operator()(Cursor& cursor) const noexcept {
    if (cursor.is<T>()) {
        cursor.advance();
        return NodeList {};
    }
    return std::nullopt;
}

template <Keyword K>
OptNodeList OneKeyword<K>::operator()(Cursor& cursor) const noexcept {
    if (cursor.is<K>()) {
        cursor.advance();
        return NodeList {};
    }
    return std::nullopt;
}

OptNodeList GetKeyword::operator()(Cursor& cursor) const noexcept {
    auto keyword = cursor.get_keyword();
    if (!keyword)
        return std::nullopt;
    cursor.advance();
    NodeList result;
    result.emplace_back(std::move(keyword.value()));
    return result;
}

template <std::size_t Hash>
OptNodeList OneVariable<Hash>::operator()(Cursor& cursor) const noexcept {
    if (cursor.is<Hash>()) {
        cursor.advance();
        return NodeList {};
    }
    return std::nullopt;
}

OptNodeList GetVariable::operator()(Cursor& cursor) const noexcept {
    auto ident = cursor.get_ident();
    if (!ident)
        return std::nullopt;
    cursor.advance();
    NodeList result;
    result.emplace_back(std::move(ident.value()));
    return result;
}

OptNodeList GetConstant::operator()(Cursor& cursor) const noexcept {
    auto node = cursor.get_constant();
    if (!node)
        return std::nullopt;
    cursor.advance();
    NodeList result;
    result.emplace_back(std::move(node.value()));
    return result;
}

template <NodeType T, ParserRule R>
OptNodeList OneNode<T, R>::operator()(Cursor& cursor) const noexcept {
    NodeType t = T;
    (void)t; // Avoid unused variable warning
    if (cursor.is_eof() || cursor.is_failed())
        return std::nullopt;
    Location loc = cursor.loc();
    OptNodeList res;
    if constexpr (R::manages_rollback::value) {
        res = R()(cursor);
        if (!res)
            return std::nullopt;
    } else {
        auto save = cursor.save();
        res = R()(cursor);
        if (!res) {
            cursor.set(save);
            return std::nullopt;
        }
    }
    auto node = std::make_unique<Node>(T, loc, std::move(res.value()));
    NodeList result;
    result.emplace_back(std::move(node));
    return result;
}

template <ParserRule Lhs, ParserRule Rhs>
OptNodeList Any<Lhs, Rhs>::operator()(Cursor& cursor) const noexcept {
    if constexpr (Lhs::manages_rollback::value
        && Rhs::manages_rollback::value) {
        auto left = Lhs()(cursor);
        if (left)
            return left;
        if (cursor.is_failed())
            return std::nullopt;
        auto right = Rhs()(cursor);
        return right;
    } else if constexpr (Lhs::manages_rollback::value) {
        auto left = Lhs()(cursor);
        if (left)
            return left;
        auto save = cursor.save();
        auto right = Rhs()(cursor);
        if (!right)
            cursor.set(save);
        return right;
    } else if constexpr (Rhs::manages_rollback::value) {
        auto save = cursor.save();
        auto left = Lhs()(cursor);
        if (left)
            return left;
        cursor.set(save);
        auto right = Rhs()(cursor);
        return right;
    } else {
        auto save = cursor.save();
        auto left = Lhs()(cursor);
        if (left)
            return left;
        cursor.set(save);
        auto right = Rhs()(cursor);
        if (!right)
            cursor.set(save);
        return right;
    }
}

template <ParserRule Lhs, ParserRule Rhs>
OptNodeList Then<Lhs, Rhs>::operator()(Cursor& cursor) const noexcept {
    auto left = Lhs()(cursor);
    if (!left)
        return std::nullopt;
    auto right = Rhs()(cursor);
    if (!right)
        return std::nullopt;

    left->reserve(left->size() + right->size());
    left->insert(left->end(), std::make_move_iterator(right->begin()),
        std::make_move_iterator(right->end()));

    return left;
}

template <ParserRule R>
OptNodeList Maybe<R>::operator()(Cursor& cursor) const noexcept {
    if constexpr (R::manages_rollback::value) {
        auto result = R()(cursor);
        if (result)
            return std::move(result.value());
        return NodeList {};
    } else {
        auto save = cursor.save();
        auto result = R()(cursor);
        if (!result) {
            cursor.set(save);
            return NodeList {};
        }
        return std::move(result.value());
    }
}

template <ParserRule R>
OptNodeList Many<R>::operator()(Cursor& cursor) const noexcept {
    NodeList result;
    if constexpr (R::manages_rollback::value) {
        while (auto nl = R()(cursor)) {
            if (result.capacity() - result.size() < nl->size())
                result.reserve(result.capacity() * 2);
            result.insert(result.end(), std::make_move_iterator(nl->begin()),
                std::make_move_iterator(nl->end()));
        }
    } else {
        auto save = cursor.save();
        while (auto nl = R()(cursor)) {
            if (result.capacity() - result.size() < nl->size())
                result.reserve(result.capacity() * 2);
            result.insert(result.end(), std::make_move_iterator(nl->begin()),
                std::make_move_iterator(nl->end()));
            save = cursor.save();
        }
        cursor.set(save);
    }
    return result;
}

template <ParserRule R>
OptNodeList Require<R>::operator()(Cursor& cursor) const noexcept {
    auto result = R()(cursor);
    if (!result) {
        cursor.fail();
        Error("Required rule failed at ", cursor.loc());
        return std::nullopt;
    }
    return std::move(result.value());
}

template <ParserRule R>
OptNodeList Drop<R>::operator()(Cursor& cursor) const noexcept {
    if constexpr (!R::produces_nodes::value)
        return R()(cursor);
    auto result = R()(cursor);
    if (!result)
        return std::nullopt;
    return NodeList {};
}

template <ParserRule R>
OptNodeList Flatten<R>::operator()(Cursor& cursor) const noexcept {
    auto result = R()(cursor);
    if (!result)
        return std::nullopt;
    if (result->size() != 1) {
        Error(
            "Flatten rule expected exactly one node, found: ", result->size());
        cursor.fail();
        return std::nullopt;
    }
    return std::move(result.value()[0]->children());
}

template <ParserRule R>
OptNodeList When<R>::operator()(Cursor& cursor) const noexcept {
    if constexpr (R::pure::value) {
        if (R()(cursor))
            return NodeList {};
        return std::nullopt;
    } else {
        auto save = cursor.save();
        bool result = !!R()(cursor);
        cursor.set(save);
        if (result)
            return NodeList {};
        return std::nullopt;
    }
}

template <ParserRule R>
OptNodeList Not<R>::operator()(Cursor& cursor) const noexcept {
    if constexpr (R::pure::value) {
        if (R()(cursor))
            return std::nullopt;
        return NodeList {};
    } else {
        auto save = cursor.save();
        bool result = !!R()(cursor);
        cursor.set(save);
        if (result)
            return std::nullopt;
        return NodeList {};
    }
}

} // namespace lpc::frontend::combinators
