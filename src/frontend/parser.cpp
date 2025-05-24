module lpc.frontend.parser;

import std;
import lpc.logging;
import lpc.frontend.token;
import lpc.frontend.ast;

namespace lpc::frontend {

using namespace lpc::frontend::combinators;

template <>
OptNodePtr ParserImpl::parse_impl<NodeType::Program>() noexcept {
    // A Scheme program consists of a sequence of expressions,
    // definitions, and syntax definitions.
    // clang-format off
    Rule rule = many(one<NodeType::Expression>() 
                   | one<NodeType::Definition>()
                   | one<NodeType::SyntaxDefinition>());
    // clang-format on
    auto result = rule(*this);
    if (!result) {
        // If the token list is empty, we won't reach this point.
        Error("Failed to parse program at ", loc());
        _failed = true;
        return std::nullopt;
    }
    return std::make_unique<Node>(NodeType::Program, _tokens.front().location(),
        std::move(result.value()));
}

void ParserImpl::run() noexcept {
    _root = parse_impl<NodeType::Program>().value();
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

} // namespace lpc::frontend

namespace lpc::frontend::combinators {

template <Parseable auto V>
constexpr Rule make_rule() {
    if constexpr (std::is_same_v<decltype(V), NodeType>) {
        return [](ParserImpl& parser) -> OptNodeList {
            if (auto&& node = parser.parse_impl<V>()) {
                NodeList list;
                list.push_back(std::move(node.value()));
                return list;
            }
            return std::nullopt;
        };
    } else {
        return [](ParserImpl& parser) -> OptNodeList {
            if (auto&& node = parser.match<V>()) {
                NodeList list;
                list.push_back(std::move(node.value()));
                return list;
            }
            return std::nullopt;
        };
    }
}

constexpr Rule operator|(Rule&& lhs, Rule&& rhs) {
    return [lhs = std::move(lhs), rhs = std::move(rhs)](
               ParserImpl& parser) -> OptNodeList {
        parser.push();
        if (auto result = lhs(parser)) {
            parser.commit();
            return result;
        }
        parser.reset_top();
        if (auto result = rhs(parser)) {
            parser.commit();
            return result;
        }
        parser.pop();
        return std::nullopt;
    };
}

constexpr Rule operator+(Rule&& lhs, Rule&& rhs) {
    return [lhs = std::move(lhs), rhs = std::move(rhs)](
               ParserImpl& parser) -> OptNodeList {
        parser.push();
        auto left = lhs(parser);
        if (!left) {
            parser.pop();
            return std::nullopt;
        }
        auto right = rhs(parser);
        if (!right) {
            parser.pop();
            return std::nullopt;
        }
        parser.commit();

        left->reserve(left->size() + right->size());
        left->insert(left->end(), std::make_move_iterator(right->begin()),
            std::make_move_iterator(right->end()));

        return left;
    };
}

constexpr Rule maybe(Rule&& rule) {
    return [rule = std::move(rule)](ParserImpl& parser) -> OptNodeList {
        if (auto result = rule(parser))
            return result;
        return NodeList {};
    };
}

constexpr Rule many(Rule&& rule) {
    return [rule = std::move(rule)](ParserImpl& parser) -> OptNodeList {
        NodeList result;
        while (auto nl = rule(parser)) {
            if (result.capacity() - result.size() < nl->size())
                result.reserve(result.capacity() * 2);
            result.insert(result.end(), std::make_move_iterator(nl->begin()),
                std::make_move_iterator(nl->end()));
        }
        return result;
    };
}

template <Parseable auto V>
constexpr Rule maybe() {
    return [](ParserImpl& parser) {
        if (auto&& node = make_rule<V>()(parser))
            return NodeList { std::move(node) };
        return NodeList {};
    };
}

template <Parseable auto V>
constexpr Rule many() {
    return [](ParserImpl& parser) {
        NodeList result;
        while (auto&& node = make_rule<V>()(parser)) {
            result.push_back(std::move(node));
        }
        return result;
    };
}

template <Parseable auto V>
constexpr Rule one() {
    return [](ParserImpl& parser) -> OptNodeList {
        return make_rule<V>()(parser);
    };
}

template <Parseable auto V>
constexpr Rule require() {
    return [](ParserImpl& parser) -> OptNodeList {
        if (auto&& result = make_rule<V>()(parser))
            return result;
        // TODO what
        Error("Expected something at ", parser.cur()->location());
        parser.fail();
        return std::nullopt;
    };
}

} // namespace lpc::frontend::combinators
