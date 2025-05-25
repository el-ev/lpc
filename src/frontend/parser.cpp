module lpc.frontend.parser;

import std;
import lpc.logging;
import lpc.frontend.token;
import lpc.frontend.ast;

namespace lpc::frontend {

using namespace lpc::frontend::combinators;

// clang-format off
namespace rules {

#define DEFTOKEN(T) \
    constexpr const OneToken<TokenType::T> (T);

DEFTOKEN(LPAREN)
DEFTOKEN(RPAREN)

template <NodeType T>
constexpr auto placeholder() noexcept {
    return !LPAREN >> !RPAREN;
}

constexpr const auto Define = placeholder<NodeType::Define>();
constexpr const auto TransformerSpec = placeholder<NodeType::TransformerSpec>();

// 4. Expressions
constexpr const auto Expression = 
    make_node<NodeType::Expression>(
        any(
            placeholder<NodeType::Symbol>()            // (4.1.1) a variable reference
          , placeholder<NodeType::Literal>()           // (4.1.2) Literal
          , placeholder<NodeType::ProcedureCall>()     // (4.1.4) Procedures
          , placeholder<NodeType::Lambda>()            // (4.1.4) Procedures
          , placeholder<NodeType::If>()                // (4.1.5) Conditionals, If
          , placeholder<NodeType::Assignment>()        // (4.1.6) Assignment
          , placeholder<NodeType::DerivedExpression>() // (4.2)   Derived expressions
          , placeholder<NodeType::MacroUse>()          // (4.3)   Macros - Macro use
          , placeholder<NodeType::MacroBlock>()        // (4.3)   Macros - Macro block
        )
    );

// 5.2 Definitions
constexpr const auto Definition = 
    make_node<NodeType::Definition>(
        any(
            Define
          , chain(
                !LPAREN
              , !OneKeyword<Keyword::BEGIN>()
              , Define
              , Many(Define)
              , !RPAREN
            )
        )
    );

constexpr const auto SyntaxDefinition = 
    make_node<NodeType::SyntaxDefinition>(
        chain(
            !LPAREN
          , !OneIdent("define-syntax")
          , OneIdent()
          , TransformerSpec 
        )
    );

// 5.1 Programs
// A program is a sequence of expressions, definitions,
// and syntax definitions.
constexpr const auto Program = 
    make_node<NodeType::Program>(
        Many(
            Definition
          | SyntaxDefinition
          | Expression
        )
    );

} // namespace rules
// clang-format on

void ParserImpl::run() noexcept {
    OptNodeList program = rules::Program(*this);
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
        && (id.empty() || std::get<std::string>(_cursor->value()) == id)) {
        return std::make_unique<TerminalNode>(*_cursor++);
    }
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
        Error("Required rule failed to match: ", typeid(R).name(), " at ",
            parser.loc());
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
