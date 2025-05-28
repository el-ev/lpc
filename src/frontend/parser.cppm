export module lpc.frontend.parser;

import std;
import lpc.frontend.ast;
import lpc.frontend.token;

namespace lpc::frontend {

using Node = ASTNode;
using NodeRef = ASTNodeArena::NodeRef;
using NodeList = std::vector<NodeRef>;
using OptNodeList = std::optional<NodeList>;

#ifdef NDEBUG
#define DEBUG_TRANSPARENT
#else
#define DEBUG_TRANSPARENT [[__gnu__::__always_inline__]] [[gnu::nodebug]]
#endif

class Cursor {
private:
    const std::vector<Token>& _tokens;
    bool _failed = false;
    std::vector<Token>::const_iterator _token;
    ASTNodeArena& _arena;

    struct SavePoint {
    private:
        std::vector<Token>::const_iterator _token;
        NodeRef _node;

        explicit constexpr SavePoint(
            std::vector<Token>::const_iterator token, NodeRef node) noexcept
            : _token(token)
            , _node(node) { };

        friend class Cursor;
    };

public:
    explicit constexpr Cursor(
        const std::vector<Token>& tokens, ASTNodeArena& arena) noexcept
        : _tokens(tokens)
        , _token(_tokens.begin())
        , _arena(arena) { };

    constexpr Cursor(const Cursor&) = delete;
    constexpr Cursor& operator=(const Cursor&) = delete;

    constexpr Cursor(Cursor&&) noexcept = default;

    [[nodiscard]] inline constexpr const Token& operator*() const noexcept {
        return *_token;
    }

    void advance() noexcept {
        if (_token->type() != TokenType::EOF)
            ++_token;
    }

    void fail() noexcept {
        _failed = true;
    }

    [[nodiscard]] inline constexpr bool is_failed() const noexcept {
        return _failed;
        // || _token->type() == TokenType::INVALID;
    }

    [[nodiscard]] inline constexpr bool is_eof() const noexcept {
        return _token->type() == TokenType::EOF;
    }

    [[nodiscard]] inline constexpr SavePoint save() const noexcept {
        return SavePoint(_token, _arena.back_ref());
    }

    inline void set(SavePoint sp) noexcept {
        _token = sp._token;
        _arena.reset_to(sp._node);
    }

    [[nodiscard]] inline constexpr TokenType type() const noexcept {
        return _token->type();
    }

    [[nodiscard]] inline constexpr LocRef loc() const noexcept {
        return _token->location();
    }

    [[nodiscard]] inline constexpr auto value() const& noexcept {
        return _token->value();
    }

    [[nodiscard]] inline constexpr ASTNodeArena& arena() & noexcept {
        return _arena;
    }

    [[nodiscard]] inline constexpr ASTNodeArena& arena() const& noexcept {
        return _arena;
    }

    template <TokenType T>
    [[nodiscard]] constexpr bool is() const noexcept;
    template <Keyword K>
    [[nodiscard]] constexpr bool is() const noexcept;
    template <std::size_t Hash>
    [[nodiscard]] constexpr bool is() const noexcept;
    [[nodiscard]] constexpr NodeRef get_keyword() const noexcept;
    [[nodiscard]] constexpr NodeRef get_ident() const noexcept;
    [[nodiscard]] constexpr NodeRef get_constant() const noexcept;
};

export class Parser {
private:
    std::vector<Token> _tokens;
    Cursor _cursor;
    ASTNodeArena _arena;
    NodeRef _root;

    void parse() noexcept;

public:
    explicit constexpr Parser(std::vector<Token>&& tokens) noexcept
        : _tokens(std::move(tokens))
        , _cursor(_tokens, _arena)
        , _root(NodeRef::invalid()) {
        parse();
    };

    Parser(const Parser&) = delete;
    Parser& operator=(const Parser&) = delete;

    Parser(Parser&&) noexcept = default;

    [[nodiscard]] inline bool is_failed() const noexcept {
        return _cursor.is_failed();
    }

    [[nodiscard]] inline bool is_eof() const noexcept {
        return _cursor.is_eof();
    }

    [[nodiscard]] inline NodeRef root() noexcept {
        return _arena.back_ref();
    }

    [[nodiscard]] inline ASTNodeArena&& arena() noexcept {
        return std::move(_arena);
    }
};

namespace combinators {
    template <typename T>
    concept BooleanType
        = std::same_as<T, std::true_type> || std::same_as<T, std::false_type>;

    template <typename T>
    concept ParserRule = requires(T t) {
        { t(std::declval<Cursor&>()) } -> std::same_as<OptNodeList>;
        typename T::pure;
        requires BooleanType<typename T::pure>;
        requires(!T::pure::value)
            || (std::same_as<typename T::manages_rollback, std::true_type>
                && std::same_as<typename T::produces_nodes, std::false_type>);
        typename T::manages_rollback;
        requires BooleanType<typename T::manages_rollback>;
        typename T::produces_nodes;
        requires BooleanType<typename T::produces_nodes>;
    };

    template <typename Derived, BooleanType Pure>
    struct CombinatorBase {
        using pure = Pure;
        using produces_nodes
            = std::conditional_t<Pure::value, std::false_type, void>;
        using manages_rollback
            = std::conditional_t<Pure::value, std::true_type, void>;

    private:
        friend Derived;
        explicit constexpr CombinatorBase() noexcept = default;
    };

    template <typename Derived>
    struct ImpureCombinator
        : CombinatorBase<ImpureCombinator<Derived>, std::false_type> {
    private:
        friend Derived;
        explicit constexpr ImpureCombinator() noexcept = default;
    };

    template <typename Derived>
    struct PureCombinator
        : CombinatorBase<PureCombinator<Derived>, std::true_type> {
    private:
        friend Derived;
        explicit constexpr PureCombinator() noexcept = default;
    };

    template <typename Derived, ParserRule Underlying>
    struct TransparentUnaryCombinator {
        using manages_rollback = Underlying::manages_rollback;
        using produces_nodes = Underlying::produces_nodes;
        using pure = Underlying::pure;
    };

    template <typename Wrapper>
    struct Def {
        using manages_rollback = std::true_type;
        using produces_nodes = std::true_type;
        using pure = std::false_type;

        explicit constexpr Def() noexcept = default;

        DEBUG_TRANSPARENT [[nodiscard]] OptNodeList operator()(
            Cursor& cursor) const noexcept {
            return Wrapper::rule()(cursor);
        }
    };

    template <TokenType T>
    struct OneToken : ImpureCombinator<OneToken<T>> {
        using produces_nodes = std::false_type;
        using manages_rollback = std::true_type;
        explicit constexpr OneToken() noexcept = default;

        [[nodiscard]] OptNodeList operator()(Cursor& cursor) const noexcept;
    };

    template <Keyword K>
    struct OneKeyword : ImpureCombinator<OneKeyword<K>> {
        using produces_nodes = std::false_type;
        using manages_rollback = std::true_type;
        explicit constexpr OneKeyword() noexcept = default;

        [[nodiscard]] OptNodeList operator()(Cursor& cursor) const noexcept;
    };

    struct GetKeyword : ImpureCombinator<GetKeyword> {
        using produces_nodes = std::true_type;
        using manages_rollback = std::true_type;
        explicit constexpr GetKeyword() noexcept = default;

        [[nodiscard]] OptNodeList operator()(Cursor& cursor) const noexcept;
    };

    template <std::size_t Hash>
    struct OneVariable : ImpureCombinator<OneVariable<Hash>> {
        using produces_nodes = std::false_type;
        using manages_rollback = std::true_type;
        explicit constexpr OneVariable() noexcept = default;

        [[nodiscard]] OptNodeList operator()(Cursor& cursor) const noexcept;
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

    struct GetVariable : ImpureCombinator<GetVariable> {
        using produces_nodes = std::true_type;
        using manages_rollback = std::true_type;
        explicit constexpr GetVariable() noexcept = default;

        [[nodiscard]] OptNodeList operator()(Cursor& cursor) const noexcept;
    };

    struct GetConstant : ImpureCombinator<GetConstant> {
        using produces_nodes = std::true_type;
        using manages_rollback = std::true_type;
        explicit constexpr GetConstant() noexcept = default;

        [[nodiscard]] OptNodeList operator()(Cursor& cursor) const noexcept;
    };

    template <NodeType T, ParserRule R>
    struct OneNode : ImpureCombinator<OneNode<T, R>> {
        using produces_nodes = std::false_type;
        using manages_rollback = std::true_type;
        explicit constexpr OneNode() noexcept = default;
        explicit constexpr OneNode(NodeType /* t */, R /* r */) noexcept { };

        [[nodiscard]] OptNodeList operator()(Cursor& cursor) const noexcept;
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
        using pure = std::bool_constant<Lhs::pure::value && Rhs::pure::value>;

        explicit constexpr Any() noexcept = default;
        explicit constexpr Any(Lhs /* lhs */, Rhs /* rhs */) noexcept { };

        [[nodiscard]] OptNodeList operator()(Cursor& cursor) const noexcept;
    };

    template <ParserRule Lhs, ParserRule Rhs>
    struct Then {
        using manages_rollback = std::false_type;
        using produces_nodes = std::bool_constant<Lhs::produces_nodes::value
            || Rhs::produces_nodes::value>;
        using pure = std::bool_constant<Lhs::pure::value && Rhs::pure::value>;

        explicit constexpr Then() noexcept = default;
        explicit constexpr Then(Lhs /* lhs */, Rhs /* rhs */) noexcept { };

        [[nodiscard]] OptNodeList operator()(Cursor& cursor) const noexcept;
    };

    template <ParserRule R>
    struct Maybe {
        using manages_rollback = std::true_type;
        using produces_nodes = R::produces_nodes;
        using pure = R::pure;

        explicit constexpr Maybe() noexcept = default;
        explicit constexpr Maybe(R /* r */) noexcept { };

        [[nodiscard]] OptNodeList operator()(Cursor& cursor) const noexcept;
    };

    template <ParserRule R>
    struct Many {
        using manages_rollback = std::true_type;
        using produces_nodes = R::produces_nodes;
        using pure = R::pure;

        explicit constexpr Many() noexcept = default;
        explicit constexpr Many(R /* r */) noexcept { };

        [[nodiscard]] OptNodeList operator()(Cursor& cursor) const noexcept;
    };

    template <ParserRule R>
    struct Require {
        using manages_rollback = std::true_type;
        using produces_nodes = R::produces_nodes;
        using pure = R::pure;

        explicit constexpr Require() noexcept = default;
        explicit constexpr Require(R /* r */) noexcept { };

        [[nodiscard]] OptNodeList operator()(Cursor& cursor) const noexcept;
    };

    template <ParserRule R>
    struct Drop : TransparentUnaryCombinator<Drop<R>, R> {
        using produces_nodes = std::false_type;
        explicit constexpr Drop() noexcept = default;
        explicit constexpr Drop(R /* r */) noexcept { };

        DEBUG_TRANSPARENT [[nodiscard]] OptNodeList operator()(
            Cursor& cursor) const noexcept;
    };

    template <ParserRule R>
    struct Flatten : TransparentUnaryCombinator<Flatten<R>, R> {
        explicit constexpr Flatten() noexcept = default;
        explicit constexpr Flatten(R /* r */) noexcept { };

        DEBUG_TRANSPARENT [[nodiscard]] OptNodeList operator()(
            Cursor& cursor) const noexcept;
    };

    template <ParserRule R>
    struct When : PureCombinator<When<R>> {
        explicit constexpr When() noexcept = default;
        explicit constexpr When(R /* r */) noexcept { };

        DEBUG_TRANSPARENT [[nodiscard]] OptNodeList operator()(
            Cursor& cursor) const noexcept;
    };

    template <ParserRule R>
    struct Not : PureCombinator<Not<R>> {
        explicit constexpr Not() noexcept = default;
        explicit constexpr Not(R /* r */) noexcept { };

        DEBUG_TRANSPARENT [[nodiscard]] OptNodeList operator()(
            Cursor& cursor) const noexcept;
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

    using namespace lpc::frontend::combinators;

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
    DECL_RULE(Definitions);
    DECL_RULE(Definition);
    DECL_RULE(SyntaxDefinition);
    DECL_RULE(Datum);
    DECL_RULE(TransformerSpec);
    DECL_RULE(SyntaxRule);
    DECL_RULE(Pattern);
    DECL_RULE(Template);
}

} // namespace lpc::frontend
