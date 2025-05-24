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
        if (_cursor == _tokens.cend()) {
            Error("Top level is popped out, should not happen");
            _failed = true;
            return;
        }
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
    [[nodiscard]] OptNodePtr match() noexcept {
        Error("match() is not specialized for TokenType: ",
            token_type_to_string(T));
        return std::nullopt; // This function is a template specialization for
                             // matching tokens of type T.
    }
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
    concept Parseable = requires {
        std::is_same_v<T, NodeType> || std::is_same_v<T, TokenType>;
    };

    using Rule = std::function<OptNodeList(ParserImpl&)>;
    template <Parseable auto V>
    constexpr Rule make_rule();
    constexpr Rule operator|(Rule&& lhs, Rule&& rhs);
    constexpr Rule operator+(Rule&& lhs, Rule&& rhs);
    constexpr Rule maybe(Rule&& rule);
    constexpr Rule many(Rule&& rule);
    template <Parseable auto V>
    constexpr Rule maybe();
    template <Parseable auto V>
    constexpr Rule many();
    template <Parseable auto V>
    constexpr Rule one();
    template <Parseable auto V>
    constexpr Rule require();
} // namespace combinators

} // namespace lpc::frontend