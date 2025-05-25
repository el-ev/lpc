module lpc.frontend.parser;

import std;
import lpc.logging;
import lpc.frontend.token;
import lpc.frontend.ast;

namespace lpc::frontend {

using namespace lpc::frontend::combinators;

// clang-format off
namespace rules {

#define DECL_RULE(R)                                                           \
    struct R {                                                                 \
        [[nodiscard]] static constexpr auto rule() noexcept;                   \
    }

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
    return OneIdent(" !won't match anything! ");
}

DECL_RULE(Program);
DECL_RULE(TopLevel);
DECL_RULE(Expression);
DECL_RULE(Variable);
DECL_RULE(Literal);
DECL_RULE(ProcedureCall);
DECL_RULE(Lambda);
DECL_RULE(Formals);
DECL_RULE(Body);
DECL_RULE(Sequence);
DECL_RULE(If);
DECL_RULE(Assignment);
DECL_RULE(Definition);
DECL_RULE(Define);
DECL_RULE(DefFormals);
DECL_RULE(SyntaxDefinition);

constexpr const auto TransformerSpec = placeholder<NodeType::TransformerSpec>();

// 5.1 Programs
// A program is a sequence of expressions, definitions,
// and syntax definitions.
DEF_RULE_BEGIN(Program)
Many(Def<TopLevel>())
DEF_RULE_END(Program)

DEF_RULE_BEGIN(TopLevel)
any(
    Def<Definition>()
  , Def<SyntaxDefinition>()
  , Def<Expression>()
  , chain(
        !LPAREN
      , !OneKeyword<Keyword::BEGIN>()
      , Def<TopLevel>()
      , !RPAREN
    )
)
DEF_RULE_END(TopLevel)

// (4.) (7.1.3.) Expressions
DEF_RULE_BEGIN(Expression)
any(
    Def<Variable>()
  , Def<Literal>()
  , Def<ProcedureCall>()
  , Def<Lambda>()
  , Def<If>()
  , Def<Assignment>()
  , placeholder<NodeType::MacroUse>()
  , placeholder<NodeType::MacroBlock>()
)
DEF_RULE_END(Expression)

// (4.1.1) a variable reference
DEF_RULE_BEGIN(Variable)
OneIdent()
DEF_RULE_END(Variable)

// (4.1.2) a literal
DEF_RULE_BEGIN(Literal)
any(
    placeholder<NodeType::Quotation>()
  , placeholder<NodeType::SelfEvaluating>()
)
DEF_RULE_END(Literal)


// (4.1.3) Procedure Call
DEF_RULE_BEGIN(ProcedureCall)
chain(
    !LPAREN
  , Def<Expression>()  // operator
  , Many(
        Def<Expression>()  // operands
    )
  , !RPAREN
)
DEF_RULE_END(ProcedureCall)

// (4.1.4) Procedures
DEF_RULE_BEGIN(Lambda)
chain(
    !LPAREN
  , !OneKeyword<Keyword::LAMBDA>()
  , placeholder<NodeType::Formals>()
  , placeholder<NodeType::Body>()
  , !RPAREN
)
DEF_RULE_END(Lambda)

DEF_RULE_BEGIN(Formals)
any(
    OneIdent()
  , chain(
        !LPAREN
      , Many(OneIdent())
      , !RPAREN
    )
  , chain(
        !LPAREN
      , OneIdent()
      , Many<OneIdent>()
      , !DOT   // FIXME: maybe confusing
      , OneIdent()
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
  , Def<Variable>()           // variable
  , Def<Expression>()         // value
  , !RPAREN
)
DEF_RULE_END(Assignment)

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
            OneIdent()
          , Def<Expression>()
        )
      , chain(
            !LPAREN
          , OneIdent()
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
    Many(OneIdent())
  , Maybe(
        chain(
            !DOT  // FIXME: maybe confusing
          , OneIdent()
        )
    )
)
DEF_RULE_END(DefFormals)

// 5.3 Syntax Definitions
DEF_RULE_BEGIN(SyntaxDefinition)
chain(
    !LPAREN
  , !OneIdent("define-syntax")
  , OneIdent()
  , TransformerSpec 
  , !RPAREN
)
DEF_RULE_END(SyntaxDefinition)

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
    if (_cur_stack.size() != 1) {
        Error("Parser stack is not empty");
        _failed = true;
        return;
    }
}

template <TokenType T>
[[nodiscard]] OptNodePtr ParserImpl::match() noexcept {
    if (_cursor == _tokens.cend())
        return std::nullopt;
    if (_cursor->type() == T)
        return std::make_unique<TerminalNode>(*_cursor++);
    return std::nullopt;
}

template <Keyword K>
[[nodiscard]] OptNodePtr ParserImpl::match() noexcept {
    if (_cursor == _tokens.cend())
        return std::nullopt;
    if (_cursor->type() == TokenType::KEYWORD
        && std::get<Keyword>(_cursor->value()) == K)
        return std::make_unique<TerminalNode>(*_cursor++);
    return std::nullopt;
}

[[nodiscard]] OptNodePtr ParserImpl::match(std::string_view id) noexcept {
    if (_cursor == _tokens.cend())
        return std::nullopt;
    if (_cursor->type() == TokenType::IDENT
        && (id.empty() || std::get<std::string>(_cursor->value()) == id))
        return std::make_unique<TerminalNode>(*_cursor++);
    return std::nullopt;
}

} // namespace lpc::frontend

namespace lpc::frontend::combinators {

template <TokenType T>
[[nodiscard]] OptNodeList OneToken<T>::operator()(
    ParserImpl& parser) const noexcept {
    if (auto node = parser.match<T>()) {
        NodeList result;
        result.emplace_back(std::move(node.value()));
        return result;
    }
    return std::nullopt;
}

template <Keyword K>
[[nodiscard]] OptNodeList OneKeyword<K>::operator()(
    ParserImpl& parser) const noexcept {
    if (auto node = parser.match<K>()) {
        NodeList result;
        result.emplace_back(std::move(node.value()));
        return result;
    }
    return std::nullopt;
}

[[nodiscard]] OptNodeList OneIdent::operator()(
    ParserImpl& parser) const noexcept {
    if (auto node = parser.match(id)) {
        NodeList result;
        result.emplace_back(std::move(node.value()));
        return result;
    }
    return std::nullopt;
}

template <NodeType T, ParserRule R>
[[nodiscard]] OptNodeList OneNode<T, R>::operator()(
    ParserImpl& parser) const noexcept {
    Location loc = parser.loc();
    OptNodeList res;
    if constexpr (R::no_rollback::value) {
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
[[nodiscard]] OptNodeList Any<Lhs, Rhs>::operator()(
    ParserImpl& parser) const noexcept {
    if constexpr (Lhs::no_rollback::value && Rhs::no_rollback::value) {
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
    } else if constexpr (Lhs::no_rollback::value) {
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
    } else if constexpr (Rhs::no_rollback::value) {
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
[[nodiscard]] OptNodeList Then<Lhs, Rhs>::operator()(
    ParserImpl& parser) const noexcept {
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
[[nodiscard]] OptNodeList Maybe<R>::operator()(
    ParserImpl& parser) const noexcept {
    if constexpr (R::no_rollback::value) {
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
[[nodiscard]] OptNodeList Many<R>::operator()(
    ParserImpl& parser) const noexcept {
    NodeList result;
    if constexpr (R::no_rollback::value) {
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
[[nodiscard]] OptNodeList Require<R>::operator()(
    ParserImpl& parser) const noexcept {
    auto result = R()(parser);
    if (!result) {
        parser.fail();
        Error("Required rule failed at ", parser.loc());
        return std::nullopt;
    }
    return std::move(result.value());
}

template <ParserRule R>
[[nodiscard]] OptNodeList Drop<R>::operator()(
    ParserImpl& parser) const noexcept {
    auto result = R()(parser);
    if (!result)
        return std::nullopt;
    return NodeList {};
}

} // namespace lpc::frontend::combinators
