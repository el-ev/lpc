export module lpc.frontend.combinators;

import std;
import lpc.frontend.ast;

export namespace lpc::frontend::combinators {

template <typename T>
concept ParserRule = requires(T t) {
    { t(std::declval<Cursor&>()) } -> std::same_as<OptNodeList>;
    typename T::manages_rollback;
    requires std::same_as<typename T::manages_rollback, std::true_type>
        || std::same_as<typename T::manages_rollback, std::false_type>;
};

struct Succeed {
    using manages_rollback = std::true_type;

    explicit constexpr Succeed() noexcept = default;

    [[nodiscard]] OptNodeList operator()(Cursor& /* cursor */) const noexcept {
        return NodeList {};
    }
};

template <typename Wrapper>
struct Def {
    using manages_rollback = std::true_type;

    explicit constexpr Def() noexcept = default;

    [[nodiscard]] OptNodeList operator()(Cursor& cursor) const noexcept {
        return Wrapper::rule()(cursor);
    }
};

template <TokenType T>
struct OneToken {
    using manages_rollback = std::true_type;
    explicit constexpr OneToken() noexcept = default;

    [[nodiscard]] OptNodeList operator()(Cursor& cursor) const noexcept;
};

template <Keyword K>
struct OneKeyword {
    using manages_rollback = std::true_type;
    explicit constexpr OneKeyword() noexcept = default;

    [[nodiscard]] OptNodeList operator()(Cursor& cursor) const noexcept;
};

template <Keyword K>
struct InsertKeyword {
    using manages_rollback = std::true_type;
    explicit constexpr InsertKeyword() noexcept = default;

    [[nodiscard]] OptNodeList operator()(Cursor& cursor) const noexcept;
};

struct GetKeyword {
    using manages_rollback = std::true_type;
    explicit constexpr GetKeyword() noexcept = default;

    [[nodiscard]] OptNodeList operator()(Cursor& cursor) const noexcept;
};

struct GetVariable {
    using manages_rollback = std::true_type;
    explicit constexpr GetVariable() noexcept = default;

    [[nodiscard]] OptNodeList operator()(Cursor& cursor) const noexcept;
};

struct GetConstant {
    using manages_rollback = std::true_type;
    explicit constexpr GetConstant() noexcept = default;

    [[nodiscard]] OptNodeList operator()(Cursor& cursor) const noexcept;
};

template <NodeType T, ParserRule R>
struct OneNode {
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

    explicit constexpr Any() noexcept = default;
    explicit constexpr Any(Lhs /* lhs */, Rhs /* rhs */) noexcept { };

    [[nodiscard]] OptNodeList operator()(Cursor& cursor) const noexcept;
};

template <ParserRule Lhs, ParserRule Rhs>
struct Then {
    using manages_rollback = std::false_type;

    explicit constexpr Then() noexcept = default;
    explicit constexpr Then(Lhs /* lhs */, Rhs /* rhs */) noexcept { };

    [[nodiscard]] OptNodeList operator()(Cursor& cursor) const noexcept;
};

template <ParserRule R>
struct Maybe {
    using manages_rollback = std::true_type;

    explicit constexpr Maybe() noexcept = default;
    explicit constexpr Maybe(R /* r */) noexcept { };

    [[nodiscard]] OptNodeList operator()(Cursor& cursor) const noexcept;
};

template <ParserRule R>
struct Many {
    using manages_rollback = std::true_type;

    explicit constexpr Many() noexcept = default;
    explicit constexpr Many(R /* r */) noexcept { };

    [[nodiscard]] OptNodeList operator()(Cursor& cursor) const noexcept;
};

template <ParserRule R>
using Some = Then<R, Many<R>>;

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
    NodeLocRef node = cursor.get_keyword();
    if (!node.is_valid())
        return std::nullopt;
    cursor.advance();
    return NodeList(1, node);
}

template <Keyword K>
OptNodeList InsertKeyword<K>::operator()(Cursor& cursor) const noexcept {
    NodeLocRef node
        = cursor.arena().emplace(cursor.loc(), NodeType::Keyword, K);
    return NodeList(1, node);
}

OptNodeList GetVariable::operator()(Cursor& cursor) const noexcept {
    NodeLocRef node = cursor.get_ident();
    if (!node.is_valid())
        return std::nullopt;
    cursor.advance();
    return NodeList(1, node);
}

OptNodeList GetConstant::operator()(Cursor& cursor) const noexcept {
    NodeLocRef node = cursor.get_constant();
    if (!node.is_valid())
        return std::nullopt;
    cursor.advance();
    return NodeList(1, node);
}

template <NodeType T, ParserRule R>
OptNodeList OneNode<T, R>::operator()(Cursor& cursor) const noexcept {
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
    NodeLocRef node = cursor.arena().emplace(loc, T, std::move(res.value()));
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

} // namespace lpc::frontend::combinators
