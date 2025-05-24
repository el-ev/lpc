module lpc.frontend.parser;

import std;
import lpc.frontend.token;
import lpc.frontend.ast;

namespace lpc::frontend::combinators {

template <Parseable T>
constexpr Rule make_rule() {
    if constexpr (std::is_same_v<T, NodeType>) {
        return [](ParserImpl& parser) -> OptNodeList {
            if (auto&& node = parser.parse_impl<T>())
                return NodeList { std::move(node) };
            return std::nullopt;
        };
    } else {
        return [](ParserImpl& parser) -> OptNodeList {
            if (auto&& node = parser.match<T>())
                return NodeList { std::move(node) };
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

template <Parseable T>
constexpr Rule maybe() {
    return [](ParserImpl& parser) {
        if (auto&& node = make_rule<T>()(parser))
            return NodeList { std::move(node) };
        return NodeList {};
    };
}

template <Parseable T>
constexpr Rule many() {
    return [](ParserImpl& parser) {
        NodeList result;
        while (auto&& node = make_rule<T>()(parser)) {
            result.push_back(std::move(node));
        }
        return result;
    };
}

template <Parseable T>
constexpr Rule one() {
    return [](ParserImpl& parser) -> OptNodeList {
        return make_rule<T>()(parser);
    };
}

template <Parseable T>
constexpr Rule require() {
    return [](ParserImpl& parser) -> OptNodeList {
        if (auto&& result = make_rule<T>()(parser))
            return result;
        // TODO what
        Error("Expected something at ", parser.cur()->location());
        parser.fail();
        return std::nullopt;
    };
}

} // namespace lpc::frontend::combinators
