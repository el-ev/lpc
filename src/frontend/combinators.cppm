export module lpc.frontend.combinators;

import std;
import lpc.frontend.ast;

#ifdef NDEBUG
#define DEBUG_TRANSPARENT
#else
#define DEBUG_TRANSPARENT [[__gnu__::__always_inline__]] [[gnu::nodebug]]
#endif

export namespace lpc::frontend::combinators {

using Node = ASTNode;
using NodeRef = ASTNodeArena::NodeRef;
using NodeList = std::vector<NodeRef>;
using OptNodeList = std::optional<NodeList>;

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

template <Keyword K>
struct InsertKeyword : ImpureCombinator<InsertKeyword<K>> {
    using produces_nodes = std::true_type;
    using manages_rollback = std::true_type;
    explicit constexpr InsertKeyword() noexcept = default;

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
using Some = Then<R, Many<R>>;

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
        return Rewrite<First, decltype(rest.build())> { first, rest.build() };
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

template <TokenType T>
OptNodeList OneToken<T>::operator()(Cursor& cursor) const noexcept {
    if (cursor.is<T>()) {
        cursor.advance();
        return NodeList {};
    }
    return std::nullopt;
}

template <Keyword K>
OptNodeList OneKeyword<K>::operator()(Cursor& cursor) const noexcept {
    if (cursor.is<K>()) {
        cursor.advance();
        return NodeList {};
    }
    return std::nullopt;
}

OptNodeList GetKeyword::operator()(Cursor& cursor) const noexcept {
    NodeRef node = cursor.get_keyword();
    if (!node.is_valid())
        return std::nullopt;
    cursor.advance();
    return NodeList(1, node);
}

template <Keyword K>
OptNodeList InsertKeyword<K>::operator()(Cursor& cursor) const noexcept {
    NodeRef node = cursor.arena().emplace(NodeType::Keyword, cursor.loc(), K);
    return NodeList(1, node);
}

OptNodeList GetVariable::operator()(Cursor& cursor) const noexcept {
    NodeRef node = cursor.get_ident();
    if (!node.is_valid())
        return std::nullopt;
    cursor.advance();
    return NodeList(1, node);
}

OptNodeList GetConstant::operator()(Cursor& cursor) const noexcept {
    NodeRef node = cursor.get_constant();
    if (!node.is_valid())
        return std::nullopt;
    cursor.advance();
    return NodeList(1, node);
}

template <NodeType T, ParserRule R>
OptNodeList OneNode<T, R>::operator()(Cursor& cursor) const noexcept {
    NodeType t = T;
    (void)t; // Avoid unused variable warning
    if (cursor.is_eof() || cursor.is_failed())
        return std::nullopt;
    LocRef loc = cursor.loc();
    OptNodeList res;
    if constexpr (R::manages_rollback::value) {
        res = R()(cursor);
        if (!res)
            return std::nullopt;
    } else {
        auto save = cursor.save();
        res = R()(cursor);
        if (!res) {
            cursor.set(save);
            return std::nullopt;
        }
    }
    NodeRef node = cursor.arena().emplace(t, loc, std::move(res.value()));
    return NodeList(1, node);
}

template <ParserRule Lhs, ParserRule Rhs>
OptNodeList Any<Lhs, Rhs>::operator()(Cursor& cursor) const noexcept {
    if constexpr (Lhs::manages_rollback::value
        && Rhs::manages_rollback::value) {
        auto left = Lhs()(cursor);
        if (left)
            return left;
        if (cursor.is_failed())
            return std::nullopt;
        auto right = Rhs()(cursor);
        return right;
    } else if constexpr (Lhs::manages_rollback::value) {
        auto left = Lhs()(cursor);
        if (left)
            return left;
        auto save = cursor.save();
        auto right = Rhs()(cursor);
        if (!right)
            cursor.set(save);
        return right;
    } else if constexpr (Rhs::manages_rollback::value) {
        auto save = cursor.save();
        auto left = Lhs()(cursor);
        if (left)
            return left;
        cursor.set(save);
        auto right = Rhs()(cursor);
        return right;
    } else {
        auto save = cursor.save();
        auto left = Lhs()(cursor);
        if (left)
            return left;
        cursor.set(save);
        auto right = Rhs()(cursor);
        if (!right)
            cursor.set(save);
        return right;
    }
}

template <ParserRule Lhs, ParserRule Rhs>
OptNodeList Then<Lhs, Rhs>::operator()(Cursor& cursor) const noexcept {
    auto left = Lhs()(cursor);
    if (!left)
        return std::nullopt;
    auto right = Rhs()(cursor);
    if (!right)
        return std::nullopt;

    left->reserve(left->size() + right->size());
    left->insert(left->end(), std::make_move_iterator(right->begin()),
        std::make_move_iterator(right->end()));

    return left;
}

template <ParserRule R>
OptNodeList Maybe<R>::operator()(Cursor& cursor) const noexcept {
    if constexpr (R::manages_rollback::value) {
        auto result = R()(cursor);
        if (result)
            return std::move(result.value());
        return NodeList {};
    } else {
        auto save = cursor.save();
        auto result = R()(cursor);
        if (!result) {
            cursor.set(save);
            return NodeList {};
        }
        return std::move(result.value());
    }
}

template <ParserRule R>
OptNodeList Many<R>::operator()(Cursor& cursor) const noexcept {
    NodeList result;
    if constexpr (R::manages_rollback::value) {
        while (auto nl = R()(cursor)) {
            if (result.capacity() - result.size() < nl->size())
                result.reserve(result.capacity() * 2);
            result.insert(result.end(), std::make_move_iterator(nl->begin()),
                std::make_move_iterator(nl->end()));
        }
    } else {
        auto save = cursor.save();
        while (auto nl = R()(cursor)) {
            if (result.capacity() - result.size() < nl->size())
                result.reserve(result.capacity() * 2);
            result.insert(result.end(), std::make_move_iterator(nl->begin()),
                std::make_move_iterator(nl->end()));
            save = cursor.save();
        }
        cursor.set(save);
    }
    return result;
}

template <ParserRule R>
OptNodeList Drop<R>::operator()(Cursor& cursor) const noexcept {
    if constexpr (!R::produces_nodes::value)
        return R()(cursor);
    auto result = R()(cursor);
    if (!result)
        return std::nullopt;
    cursor.arena().pop_back();
    return NodeList {};
}
}