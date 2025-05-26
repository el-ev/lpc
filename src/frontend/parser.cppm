#include <type_traits>
export module lpc.frontend.parser;

import std;
import lpc.logging;
import lpc.frontend.token;
import lpc.frontend.ast;

namespace lpc::frontend {

using Node = ASTNode;
using NodePtr = std::unique_ptr<Node>;
using OptNodePtr = std::optional<NodePtr>;
using NodeList = std::vector<NodePtr>;
using OptNodeList = std::optional<NodeList>;

export class Parser;

#ifdef NDEBUG
#define DEBUG_TRANSPARENT
#else
#define DEBUG_TRANSPARENT [[__gnu__::__always_inline__]] [[gnu::nodebug]]
#endif

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
        _cursor = _cur_stack.back();
    }

    inline void commit() noexcept {
        _cur_stack.pop_back();
        _cur_stack.back() = _cursor;
    }

    inline void sync() noexcept {
        _cur_stack.back() = _cursor;
    }

    template <TokenType T>
    [[nodiscard]] bool match() noexcept;
    template <Keyword K>
    [[nodiscard]] bool match() noexcept;
    [[nodiscard]] bool match(std::size_t hash) noexcept;
    [[nodiscard]] std::optional<std::string> get_ident() noexcept;
    [[nodiscard]] std::optional<Keyword> get_keyword() noexcept;
    [[nodiscard]] OptNodePtr get_constant() noexcept;
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
        { t(std::declval<ParserImpl&>()) } -> std::same_as<OptNodeList>;
        typename T::manages_rollback;
        requires std::same_as<typename T::manages_rollback, std::true_type>
            || std::same_as<typename T::manages_rollback, std::false_type>;
        typename T::produces_nodes;
        requires std::same_as<typename T::produces_nodes, std::true_type>
            || std::same_as<typename T::produces_nodes, std::false_type>;
    };

    template <typename Wrapper>
    struct Def {
        using manages_rollback = std::true_type;
        using produces_nodes = std::true_type;

        explicit constexpr Def() noexcept = default;

        DEBUG_TRANSPARENT [[nodiscard]] OptNodeList operator()(
            ParserImpl& parser) const noexcept {
            return Wrapper::rule()(parser);
        }
    };

    template <TokenType T>
    struct OneToken {
        using manages_rollback = std::true_type;
        using produces_nodes = std::false_type;

        explicit constexpr OneToken() noexcept = default;
        explicit constexpr OneToken(TokenType /* t */) noexcept { };

        [[nodiscard]] OptNodeList operator()(ParserImpl& parser) const noexcept;
    };

    template <Keyword K>
    struct OneKeyword {
        using manages_rollback = std::true_type;
        using produces_nodes = std::false_type;

        explicit constexpr OneKeyword() noexcept = default;
        explicit constexpr OneKeyword(Keyword /* k */) noexcept { };

        [[nodiscard]] OptNodeList operator()(ParserImpl& parser) const noexcept;
    };

    struct GetKeyword {
        using manages_rollback = std::true_type;
        using produces_nodes = std::true_type;

        explicit constexpr GetKeyword() noexcept = default;

        [[nodiscard]] OptNodeList operator()(ParserImpl& parser) const noexcept;
    };

    template <std::size_t Hash>
    struct OneVariable {
        using manages_rollback = std::true_type;
        using produces_nodes = std::false_type;

        explicit constexpr OneVariable() noexcept = default;

        [[nodiscard]] OptNodeList operator()(ParserImpl& parser) const noexcept;
    };

    template <std::size_t N>
    consteval std::size_t hash_string(const char (&str)[N]) noexcept {
        std::size_t h = 14695981039346656037ULL;
        for (std::size_t i = 0; i < N - 1; ++i) {
            h ^= static_cast<std::size_t>(str[i]);
            h *= 1099511628211ULL;
        }
        return h;
    }

    struct GetVariable {
        using manages_rollback = std::true_type;
        using produces_nodes = std::true_type;

        explicit constexpr GetVariable() noexcept = default;

        [[nodiscard]] OptNodeList operator()(ParserImpl& parser) const noexcept;
    };

    struct GetConstant {
        using manages_rollback = std::true_type;
        using produces_nodes = std::true_type;

        explicit constexpr GetConstant() noexcept = default;

        [[nodiscard]] OptNodeList operator()(ParserImpl& parser) const noexcept;
    };

    template <NodeType T, ParserRule R>
    struct OneNode {
        using manages_rollback = std::true_type;
        using produces_nodes = std::true_type;

        explicit constexpr OneNode() noexcept = default;
        explicit constexpr OneNode(NodeType /* t */, R /* r */) noexcept { };

        [[nodiscard]] OptNodeList operator()(ParserImpl& parser) const noexcept;
    };

    template <NodeType T, ParserRule R>
    [[nodiscard]] constexpr auto make_node(R r) noexcept {
        return OneNode<T, R> { T, r };
    }

    template <ParserRule Lhs, ParserRule Rhs>
    struct Any {
        using manages_rollback = std::true_type;
        using produces_nodes = std::bool_constant<Lhs::produces_nodes::value
            || Rhs::produces_nodes::value>;

        explicit constexpr Any() noexcept = default;
        explicit constexpr Any(Lhs /* lhs */, Rhs /* rhs */) noexcept { };

        [[nodiscard]] OptNodeList operator()(ParserImpl& parser) const noexcept;
    };

    template <ParserRule Lhs, ParserRule Rhs>
    struct Then {
        using manages_rollback = std::false_type;
        using produces_nodes = std::bool_constant<Lhs::produces_nodes::value
            || Rhs::produces_nodes::value>;

        explicit constexpr Then() noexcept = default;
        explicit constexpr Then(Lhs /* lhs */, Rhs /* rhs */) noexcept { };

        [[nodiscard]] OptNodeList operator()(ParserImpl& parser) const noexcept;
    };

    template <ParserRule R>
    struct Maybe {
        using manages_rollback = std::true_type;
        using produces_nodes = R::produces_nodes;

        explicit constexpr Maybe() noexcept = default;
        explicit constexpr Maybe(R /* r */) noexcept { };

        [[nodiscard]] OptNodeList operator()(ParserImpl& parser) const noexcept;
    };

    template <ParserRule R>
    struct Many {
        using manages_rollback = std::true_type;
        using produces_nodes = R::produces_nodes;

        explicit constexpr Many() noexcept = default;
        explicit constexpr Many(R /* r */) noexcept { };

        [[nodiscard]] OptNodeList operator()(ParserImpl& parser) const noexcept;
    };

    template <ParserRule R>
    struct Require {
        using manages_rollback = std::true_type;
        using produces_nodes = R::produces_nodes;

        explicit constexpr Require() noexcept = default;
        explicit constexpr Require(R /* r */) noexcept { };

        [[nodiscard]] OptNodeList operator()(ParserImpl& parser) const noexcept;
    };

    template <ParserRule R>
    struct Drop {
        using manages_rollback = R::manages_rollback;
        using produces_nodes = std::false_type;

        explicit constexpr Drop() noexcept = default;
        explicit constexpr Drop(R /* r */) noexcept { };

        DEBUG_TRANSPARENT [[nodiscard]] OptNodeList operator()(
            ParserImpl& parser) const noexcept;
    };

    template <ParserRule R>
    struct Flatten {
        using manages_rollback = R::manages_rollback;
        using produces_nodes = std::true_type;

        explicit constexpr Flatten() noexcept = default;
        explicit constexpr Flatten(R /* r */) noexcept { };

        DEBUG_TRANSPARENT [[nodiscard]] OptNodeList operator()(
            ParserImpl& parser) const noexcept;
    };

    template <ParserRule Lhs, ParserRule Rhs>
    constexpr Any<Lhs, Rhs> operator|(Lhs lhs, Rhs rhs) {
        return Any<Lhs, Rhs> { lhs, rhs };
    }

    template <ParserRule Lhs, ParserRule Rhs>
    constexpr Then<Lhs, Rhs> operator>>(Lhs lhs, Rhs rhs) {
        return Then<Lhs, Rhs> { lhs, rhs };
    }

    // Drop a syntax-only rule, usually a single token.
    template <ParserRule R>
    DEBUG_TRANSPARENT constexpr Drop<R> operator!(R r) {
        return Drop<R> { r };
    }

    template <ParserRule R>
    DEBUG_TRANSPARENT [[nodiscard]] constexpr Flatten<R> operator~(R r) {
        return Flatten<R> { r };
    }

    template <template <typename, typename> class Rewrite, ParserRule... Rules>
    struct Chain;

    template <template <typename, typename> class Rewrite, ParserRule R>
    struct Chain<Rewrite, R> {
        R rule;

        explicit constexpr Chain(R r) noexcept
            : rule(r) { };

        [[nodiscard]] constexpr auto build() const noexcept {
            return rule;
        }
    };

    template <template <typename, typename> class Rewrite, ParserRule First,
        ParserRule Second, ParserRule... Rest>
    struct Chain<Rewrite, First, Second, Rest...> {
        First first;
        Chain<Rewrite, Second, Rest...> rest;

        explicit constexpr Chain(First f, Second s, Rest... r) noexcept
            : first(f)
            , rest(s, r...) { };

        [[nodiscard]] constexpr auto build() const noexcept {
            return Rewrite<First, decltype(rest.build())> { first,
                rest.build() };
        }
    };

    template <template <typename, typename> class Rewrite, ParserRule... Rules>
    [[nodiscard]] constexpr auto build_chain(Rules... rules) noexcept {
        return Chain<Rewrite, Rules...> { rules... }.build();
    }

    template <ParserRule... Rules>
    [[nodiscard]] constexpr auto chain(Rules... rules) noexcept {
        return build_chain<Then>(rules...);
    }

    template <ParserRule... Rules>
    [[nodiscard]] constexpr auto any(Rules... rules) noexcept {
        return build_chain<Any>(rules...);
    }
} // namespace combinators

namespace rules {

#define DECL_RULE(R)                                                           \
    struct R {                                                                 \
        DEBUG_TRANSPARENT [[nodiscard]] static constexpr auto rule() noexcept; \
    }

    DECL_RULE(Program);
    DECL_RULE(ExprOrDef);
    DECL_RULE(Expression);
    DECL_RULE(Literal);
    DECL_RULE(Quotation);
    DECL_RULE(ProcedureCall);
    DECL_RULE(Lambda);
    DECL_RULE(Formals);
    DECL_RULE(Body);
    DECL_RULE(Sequence);
    DECL_RULE(If);
    DECL_RULE(Assignment);
    DECL_RULE(MacroUse);
    DECL_RULE(MacroBlock);
    DECL_RULE(LetSyntax);
    DECL_RULE(LetRecSyntax);
    DECL_RULE(SyntaxSpec);
    DECL_RULE(Definition);
    DECL_RULE(Define);
    DECL_RULE(DefFormals);
    DECL_RULE(SyntaxDefinition);
    DECL_RULE(Datum);
    DECL_RULE(TransformerSpec);
}

} // namespace lpc::frontend
