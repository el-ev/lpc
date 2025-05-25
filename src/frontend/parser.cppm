export module lpc.frontend.parser;

import std;
import lpc.logging;
import lpc.frontend.token;
import lpc.frontend.ast;

namespace lpc::frontend {

using Node = ASTNode;
using TerminalNode = TerminalASTNode;
using NodePtr = std::unique_ptr<Node>;
using OptNodePtr = std::optional<NodePtr>;
using NodeList = std::vector<NodePtr>;
using OptNodeList = std::optional<NodeList>;

export class Parser;

class ParserImpl {
private:
    using token_iterator = std::vector<Token>::const_iterator;
    std::vector<Token> _tokens;
    token_iterator _cursor;
    std::vector<token_iterator> _cur_stack;
    NodePtr _root;
    bool _failed = false;

public:
    explicit constexpr ParserImpl(std::vector<Token>&& tokens) noexcept
        : _tokens(std::move(tokens))
        , _cursor(_tokens.cbegin())
        , _cur_stack(1, _cursor) {
    }

    void run() noexcept;

    void fail() noexcept {
        _failed = true;
    }

    [[nodiscard]] inline bool is_eof() const noexcept {
        return _cursor == _tokens.cend();
    }

    [[nodiscard]] inline bool is_failed() const noexcept {
        return _failed;
    }

    [[nodiscard]] inline Location loc() const noexcept {
        return _cursor->location();
    }

    [[nodiscard]] inline NodePtr&& root() noexcept {
        return std::move(_root);
    }

    [[nodiscard]] inline auto cur() const noexcept -> token_iterator {
        return _cursor;
    }

    inline void push() noexcept {
        _cur_stack.push_back(_cursor);
    }

    inline void pop() noexcept {
        _cur_stack.pop_back();
        _cursor = _cur_stack.back();
    }

    inline void reset_top() noexcept {
        // assert(_cur_stack.size() > 1);
        _cur_stack.back() = _cur_stack[_cur_stack.size() - 2];
        _cursor = _cur_stack.back();
    }

    inline void commit() noexcept {
        _cur_stack.pop_back();
        _cur_stack.back() = _cursor;
    }

    inline void sync() noexcept {
        // assert(_cur_stack.size() > 1);
        _cur_stack.back() = _cursor;
    }

    template <NodeType T>
    [[nodiscard]] OptNodePtr parse_impl() noexcept {
        // This function is a template specialization for parsing nodes of type
        // T. It should be specialized for each NodeType.
        Error("parse_impl() is not specialized for NodeType: ",
            node_type_to_string(T));
        return std::nullopt;
    }

    template <TokenType T>
    [[nodiscard]] OptNodePtr match() noexcept;

    template <Keyword K>
    [[nodiscard]] OptNodePtr match() noexcept;
};

class Parser {
private:
    ParserImpl _impl;

public:
    explicit constexpr Parser(std::vector<Token>&& tokens) noexcept
        : _impl(std::move(tokens)) {
        _impl.run();
    }

    [[nodiscard]] inline bool is_failed() const noexcept {
        return _impl.is_failed();
    }

    [[nodiscard]] inline NodePtr&& root() noexcept {
        return std::move(_impl.root());
    }
};

namespace combinators {
    template <typename T>
    concept ParserRule = requires(T t) {
        typename T::no_rollback;
        { t(std::declval<ParserImpl&>()) } -> std::same_as<OptNodeList>;
        { T::no_rollback::value } -> std::convertible_to<bool>;
    };

    template <TokenType T>
    struct OneToken {
        // If a single token fails to match, it does not
        // consume the input, so rollback is not needed.
        using no_rollback = std::true_type;

        explicit constexpr OneToken() noexcept = default;
        explicit constexpr OneToken(TokenType /* t */) noexcept { };

        [[nodiscard]] OptNodeList operator()(ParserImpl& parser) const noexcept;
    };

    template <Keyword K>
    struct OneKeyword {
        // A keyword is lexically the same as a token
        using no_rollback = std::true_type;

        explicit constexpr OneKeyword() noexcept = default;
        explicit constexpr OneKeyword(Keyword /* k */) noexcept { };

        [[nodiscard]] OptNodeList operator()(ParserImpl& parser) const noexcept;
    };

    template <ParserRule R>
    struct OneNode {
        // OneNode is responsible for managing rollback behavior for
        // the rule it encapsulates. It ensures that rollback will
        // not propagate to higher levels.
        using no_rollback = std::true_type;

        explicit constexpr OneNode() noexcept = default;
        explicit constexpr OneNode(R /* r */) noexcept { };

        [[nodiscard]] OptNodeList operator()(ParserImpl& parser) const noexcept;
    };

    template <ParserRule Lhs, ParserRule Rhs>
    struct Any {
        // Any could also stop rollback propagation.
        using no_rollback = std::true_type;

        explicit constexpr Any() noexcept = default;
        explicit constexpr Any(Lhs /* lhs */, Rhs /* rhs */) noexcept { };

        [[nodiscard]] OptNodeList operator()(ParserImpl& parser) const noexcept;
    };

    template <ParserRule Lhs, ParserRule Rhs>
    struct Then {
        // Then creates rollback points.
        // Then itself does not process rollback, because long
        // Then chain exists, and it may cause performance
        // issues if rollback is processed for each Then.
        using no_rollback = std::false_type;

        explicit constexpr Then() noexcept = default;
        explicit constexpr Then(Lhs /* lhs */, Rhs /* rhs */) noexcept { };

        [[nodiscard]] OptNodeList operator()(ParserImpl& parser) const noexcept;
    };

    template <ParserRule R>
    struct Maybe {
        // Maybe eliminates rollback points.
        using no_rollback = std::true_type;

        explicit constexpr Maybe() noexcept = default;
        explicit constexpr Maybe(R /* r */) noexcept { };

        [[nodiscard]] OptNodeList operator()(ParserImpl& parser) const noexcept;
    };

    template <ParserRule R>
    struct Many {
        // Many eliminates rollback points.
        using no_rollback = std::true_type;

        explicit constexpr Many() noexcept = default;
        explicit constexpr Many(R /* r */) noexcept { };

        [[nodiscard]] OptNodeList operator()(ParserImpl& parser) const noexcept;
    };

    template <ParserRule R>
    struct Require {
        // Require throws an error if the rule fails to match.
        // So rollback does not matter.
        using no_rollback = std::true_type;

        explicit constexpr Require() noexcept = default;
        explicit constexpr Require(R /* r */) noexcept { };

        [[nodiscard]] OptNodeList operator()(ParserImpl& parser) const noexcept;
    };

    template <ParserRule Lhs, ParserRule Rhs>
    constexpr Any<Lhs, Rhs> operator|(Lhs lhs, Rhs rhs) {
        return Any<Lhs, Rhs> { lhs, rhs };
    }

    template <ParserRule Lhs, ParserRule Rhs>
    constexpr Then<Lhs, Rhs> operator>>(Lhs lhs, Rhs rhs) {
        return Then<Lhs, Rhs> { lhs, rhs };
    }
} // namespace combinators

} // namespace lpc::frontend
