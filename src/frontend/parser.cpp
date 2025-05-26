module lpc.frontend.parser;

import std;
import lpc.logging;
import lpc.frontend.token;
import lpc.frontend.ast;

namespace lpc::frontend {

using namespace lpc::frontend::combinators;

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
DEFTOKEN(DOT);

template <NodeType T>
constexpr auto placeholder() noexcept {
    return !LPAREN>>!RPAREN;
}

constexpr const auto GetIdentifier =
    any(
        GetVariable(),
        GetKeyword()
    );

constexpr const auto TransformerSpec = placeholder<NodeType::TransformerSpec>();

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
  , ~Def<Expression>()
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
  , ~Def<Expression>()
  , Many(~Def<Expression>())
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
  , chain(
        !LPAREN
      , GetVariable()
      , Many(GetVariable())
      , !DOT
      , GetVariable()
      , !RPAREN
    )
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
    ~Def<Expression>()
  , Many(~Def<Expression>()) 
)
DEF_RULE_END(Sequence)

// Conditional
DEF_RULE_BEGIN(If)
chain(
    !LPAREN
  , !OneKeyword<Keyword::IF>()
  , ~Def<Expression>()         // test
  , ~Def<Expression>()         // consequent
  , Maybe(~Def<Expression>())  // alternate
  , !RPAREN
)
DEF_RULE_END(If)

// Assignment
DEF_RULE_BEGIN(Assignment)
chain(
    !LPAREN
  , !OneKeyword<Keyword::SET>()
  , GetVariable()           // variable
  , ~Def<Expression>()         // value
  , !RPAREN
)
DEF_RULE_END(Assignment)

DEF_RULE_BEGIN(MacroUse)
chain(
    !LPAREN
  , GetIdentifier
  , Many(~Def<Expression>())   // arguments
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
  , GetVariable()              // name
  , TransformerSpec            // transformer spec
  , Many(~Def<Expression>())    // body
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
          , ~Def<Expression>()
        )
      , chain(
            !LPAREN
          , GetVariable()
          , Def<DefFormals>()
          , !RPAREN
          , Def<Body>()
        )
    )
  , !RPAREN
)
DEF_RULE_END(Define)

DEF_RULE_BEGIN(DefFormals)
chain(
    Many(GetVariable())
  , Maybe(
        chain(
            !DOT
          , GetVariable()
        )
    )
)
DEF_RULE_END(DefFormals)

// 5.3 Syntax Definitions
DEF_RULE_BEGIN(SyntaxDefinition)
chain(
    !LPAREN
  , !OneVariable<hash_string("define-syntax")>()
  , GetIdentifier
  , TransformerSpec
  , !RPAREN
)
DEF_RULE_END(SyntaxDefinition)

DEF_RULE_BEGIN(Datum)
!LPAREN >> !RPAREN // TODO
DEF_RULE_END(Datum)

} // namespace rules
// clang-format on

void ParserImpl::run() noexcept {
    OptNodeList program = Def<rules::Program>()(*this);
    if (!program) {
        Error("Failed to parse program at ", loc());
        _failed = true;
        return;
    }
    if (program->size() != 1) {
        Error("Program should have exactly one root node, found: ",
            program->size());
        _failed = true;
        return;
    }
    _root = std::move(program.value()[0]);
    if (!is_eof()) {
        Error("Unexpected tokens after parsing the root node");
        _failed = true;
        return;
    }
}

template <TokenType T>
bool ParserImpl::match() noexcept {
    if (is_eof())
        return false;
    if (_cursor->type() == T) {
        _cursor++;
        return true;
    }
    return false;
}

template <Keyword K>
bool ParserImpl::match() noexcept {
    if (is_eof())
        return false;
    if (_cursor->type() == TokenType::KEYWORD
        && std::get<Keyword>(_cursor->value()) == K) {
        _cursor++;
        return true;
    }
    return false;
}

bool ParserImpl::match(std::size_t hash) noexcept {
    if (is_eof())
        return false;
    auto string_hash = [](const std::string& str) noexcept {
        std::size_t h = 14695981039346656037ULL;
        for (const char& it : str) {
            h ^= static_cast<std::size_t>(it);
            h *= 1099511628211ULL;
        }
        return h;
    };
    if (_cursor->type() == TokenType::IDENT
        && (hash == 0
            || string_hash(std::get<std::string>(_cursor->value())) == hash)) {
        _cursor++;
        return true;
    }
    return false;
}

std::optional<std::string> ParserImpl::get_ident() noexcept {
    if (_cursor == _tokens.cend() || _cursor->type() != TokenType::IDENT)
        return std::nullopt;
    auto ident = std::get<std::string>(_cursor->value());
    _cursor++;
    return ident;
}

std::optional<Keyword> ParserImpl::get_keyword() noexcept {
    if (_cursor == _tokens.cend() || _cursor->type() != TokenType::KEYWORD)
        return std::nullopt;
    auto keyword = std::get<Keyword>(_cursor->value());
    _cursor++;
    return keyword;
}

OptNodePtr ParserImpl::get_constant() noexcept {
    if (_cursor == _tokens.cend())
        return std::nullopt;
    OptNodePtr ptr;
    switch (_cursor->type()) {
    case TokenType::NUMBER: {
        auto value = std::get<std::int64_t>(_cursor->value());
        ptr = std::make_unique<Node>(
            NodeType::Number, _cursor->location(), value);
        break;
    }
    case TokenType::BOOLEAN: {
        auto value = std::get<bool>(_cursor->value());
        ptr = std::make_unique<Node>(
            NodeType::Boolean, _cursor->location(), value);
        break;
    }
    case TokenType::CHARACTER: {
        auto value = std::get<char>(_cursor->value());
        ptr = std::make_unique<Node>(
            NodeType::Character, _cursor->location(), value);
        break;
    }
    case TokenType::STRING: {
        auto value = std::get<std::string>(_cursor->value());
        ptr = std::make_unique<Node>(
            NodeType::String, _cursor->location(), std::move(value));
        break;
    }
    default: return std::nullopt;
    }
    _cursor++;
    return ptr;
}

} // namespace lpc::frontend

namespace lpc::frontend::combinators {

template <TokenType T>
OptNodeList OneToken<T>::operator()(ParserImpl& parser) const noexcept {
    if (parser.match<T>())
        return NodeList {};
    return std::nullopt;
}

template <Keyword K>
OptNodeList OneKeyword<K>::operator()(ParserImpl& parser) const noexcept {
    if (parser.match<K>())
        return NodeList {};
    return std::nullopt;
}

OptNodeList GetKeyword::operator()(ParserImpl& parser) const noexcept {
    auto kw = parser.get_keyword();
    if (!kw)
        return std::nullopt;
    NodeList result;
    result.emplace_back(std::make_unique<Node>(
        NodeType::Keyword, (parser.cur() - 1)->location(), *kw));
    return result;
}

template <std::size_t Hash>
OptNodeList OneVariable<Hash>::operator()(ParserImpl& parser) const noexcept {
    if (parser.match(Hash))
        return NodeList {};
    return std::nullopt;
}

OptNodeList GetVariable::operator()(ParserImpl& parser) const noexcept {
    auto ident = parser.get_ident();
    if (!ident)
        return std::nullopt;
    NodeList result;
    result.emplace_back(std::make_unique<Node>(
        NodeType::Variable, (parser.cur() - 1)->location(), std::move(*ident)));
    return result;
}

OptNodeList GetConstant::operator()(ParserImpl& parser) const noexcept {
    auto node = parser.get_constant();
    if (!node)
        return std::nullopt;
    NodeList result;
    result.emplace_back(std::move(node.value()));
    return result;
}

template <NodeType T, ParserRule R>
OptNodeList OneNode<T, R>::operator()(ParserImpl& parser) const noexcept {
    NodeType t = T;
    (void)t; // Avoid unused variable warning
    if (parser.is_eof() || parser.is_failed())
        return std::nullopt;
    Location loc = parser.loc();
    OptNodeList res;
    if constexpr (R::manages_rollback::value) {
        res = R()(parser);
        if (!res)
            return std::nullopt;
        parser.sync();
    } else {
        parser.push();
        res = R()(parser);
        if (!res) {
            parser.pop();
            return std::nullopt;
        }
        parser.commit();
    }
    auto node = std::make_unique<Node>(T, loc, std::move(res.value()));
    NodeList result;
    result.emplace_back(std::move(node));
    return result;
}

template <ParserRule Lhs, ParserRule Rhs>
OptNodeList Any<Lhs, Rhs>::operator()(ParserImpl& parser) const noexcept {
    if constexpr (Lhs::manages_rollback::value
        && Rhs::manages_rollback::value) {
        auto left = Lhs()(parser);
        if (left) {
            parser.sync();
            return left;
        }
        if (parser.is_failed())
            return std::nullopt;
        auto right = Rhs()(parser);
        parser.sync();
        return right;
    } else if constexpr (Lhs::manages_rollback::value) {
        auto left = Lhs()(parser);
        if (left) {
            parser.sync();
            return left;
        }
        parser.push();
        auto right = Rhs()(parser);
        if (!right)
            parser.pop();
        else
            parser.commit();
        return right;
    } else if constexpr (Rhs::manages_rollback::value) {
        parser.push();
        auto left = Lhs()(parser);
        if (left) {
            parser.commit();
            return left;
        }
        parser.pop();
        auto right = Rhs()(parser);
        parser.sync();
        return right;
    } else {
        parser.push();
        auto left = Lhs()(parser);
        if (left) {
            parser.commit();
            return left;
        }
        parser.reset_top();
        auto right = Rhs()(parser);
        if (!right)
            parser.pop();
        else
            parser.commit();
        return right;
    }
}

template <ParserRule Lhs, ParserRule Rhs>
OptNodeList Then<Lhs, Rhs>::operator()(ParserImpl& parser) const noexcept {
    auto left = Lhs()(parser);
    if (!left)
        return std::nullopt;
    parser.sync();
    auto right = Rhs()(parser);
    if (!right)
        return std::nullopt;
    parser.sync();

    left->reserve(left->size() + right->size());
    left->insert(left->end(), std::make_move_iterator(right->begin()),
        std::make_move_iterator(right->end()));

    return left;
}

template <ParserRule R>
OptNodeList Maybe<R>::operator()(ParserImpl& parser) const noexcept {
    if constexpr (R::manages_rollback::value) {
        auto result = R()(parser);
        if (result)
            return std::move(result.value());
        return NodeList {};
    } else {
        parser.push();
        auto result = R()(parser);
        if (!result) {
            parser.pop();
            return NodeList {};
        }
        parser.commit();
        return std::move(result.value());
    }
}

template <ParserRule R>
OptNodeList Many<R>::operator()(ParserImpl& parser) const noexcept {
    NodeList result;
    if constexpr (R::manages_rollback::value) {
        while (auto nl = R()(parser)) {
            if (result.capacity() - result.size() < nl->size())
                result.reserve(result.capacity() * 2);
            result.insert(result.end(), std::make_move_iterator(nl->begin()),
                std::make_move_iterator(nl->end()));
            parser.sync();
        }
    } else {
        parser.push();
        while (auto nl = R()(parser)) {
            if (result.capacity() - result.size() < nl->size())
                result.reserve(result.capacity() * 2);
            result.insert(result.end(), std::make_move_iterator(nl->begin()),
                std::make_move_iterator(nl->end()));
            parser.commit();
            parser.push();
        }
        parser.pop();
    }
    return result;
}

template <ParserRule R>
OptNodeList Require<R>::operator()(ParserImpl& parser) const noexcept {
    auto result = R()(parser);
    if (!result) {
        parser.fail();
        Error("Required rule failed at ", parser.loc());
        return std::nullopt;
    }
    return std::move(result.value());
}

template <ParserRule R>
OptNodeList Drop<R>::operator()(ParserImpl& parser) const noexcept {
    if constexpr (!R::produces_nodes::value)
        return R()(parser);
    auto result = R()(parser);
    if (!result)
        return std::nullopt;
    return NodeList {};
}

template <ParserRule R>
OptNodeList Flatten<R>::operator()(ParserImpl& parser) const noexcept {
    auto result = R()(parser);
    if (!result)
        return std::nullopt;
    if (result->size() != 1) {
        Error(
            "Flatten rule expected exactly one node, found: ", result->size());
        parser.fail();
        return std::nullopt;
    }
    return std::move(result.value()[0]->children());
}

} // namespace lpc::frontend::combinators
