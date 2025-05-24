module lpc.frontend.parser;

import std;
import lpc.frontend.token;
import lpc.frontend.ast;

namespace lpc::frontend::combinators {

template <NodeType T>
constexpr Rule make_rule() {
    return [](ParserImpl& parser) -> OptNodeList {
        if (auto&& node = parser.parse_impl<T>())
            return NodeList { std::move(node) };
        return std::nullopt;
    };
}

template <TokenType T>
constexpr Rule make_rule() {
    return [](ParserImpl& parser) -> OptNodeList {
        if (auto&& node = parser.match<T>())
            return NodeList { std::move(node) };
        return std::nullopt;
    };
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
            result.insert(result.end(), std::make_move_iterator(nl->begin()),
                std::make_move_iterator(nl->end()));
        }
        return result;
    };
}

template <NodeType T>
constexpr Rule maybe() {
    return [](ParserImpl& parser) {
        if (auto&& node = parser.parse_impl<T>())
            return NodeList { std::move(node) };
        return NodeList {};
    };
}

template <NodeType T>
constexpr Rule many() {
    return [](ParserImpl& parser) {
        NodeList result;
        while (auto&& node = parser.parse_impl<T>()) {
            result.push_back(std::move(node));
        }
        return result;
    };
}

template <NodeType T>
constexpr Rule one() {
    return [](ParserImpl& parser) -> OptNodeList { return make_rule<T>()(parser); };
}

template <NodeType T>
constexpr Rule require() {
    return [](ParserImpl& parser) -> OptNodeList {
        if (auto&& result = one<T>(parser))
            return result;
        Error("Expected something at ", parser.cur()->location());
        parser.fail();
        return std::nullopt;
    };
}

template <TokenType T>
constexpr Rule maybe() {
    return [](ParserImpl& parser) {
        if (auto&& node = parser.match<T>())
            return NodeList { std::move(node) };
        return NodeList {};
    };
}

template <TokenType T>
constexpr Rule many() {
    return [](ParserImpl& parser) {
        NodeList result;
        while (auto&& node = parser.match<T>()) {
            result.push_back(std::move(node));
        }
        return result;
    };
}

template <TokenType T>
constexpr Rule one() {
    return [](ParserImpl& parser) -> OptNodeList { return make_rule<T>()(parser); };
}

template <TokenType T>
constexpr Rule require() {
    return [](ParserImpl& parser) -> OptNodeList {
        if (auto&& result = one<T>(parser))
            return result;
        Error("Expected some token at ", parser.cur()->location());
        parser.fail();
        return std::nullopt;
    };
}

} // namespace lpc::frontend::combinators
