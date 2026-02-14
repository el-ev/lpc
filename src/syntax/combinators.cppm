export module lpc.syntax.combinators;

import std;

import lpc.syntax.ast;
import lpc.syntax.cursor;
import lpc.syntax.refs;

export namespace lpc::syntax::combinators {

using ParseResult = std::optional<std::vector<SpanRef>>;

template <typename T>
concept ParserRule = requires(T t) {
    { t(std::declval<Cursor&>()) } -> std::same_as<ParseResult>;
    typename T::manages_rollback;
    requires std::same_as<typename T::manages_rollback, std::true_type>
        || std::same_as<typename T::manages_rollback, std::false_type>;
};

template <typename Wrapper>
struct Def {
    using manages_rollback = std::true_type;

    explicit constexpr Def() noexcept = default;

    [[nodiscard]] ParseResult operator()(Cursor& cursor) const noexcept {
        return Wrapper::rule()(cursor);
    }
};

template <TokenType T>
struct OneToken {
    using manages_rollback = std::true_type;
    explicit constexpr OneToken() noexcept = default;

    [[nodiscard]] ParseResult operator()(Cursor& cursor) const noexcept;
};

template <Keyword K>
struct InsertKeyword {
    using manages_rollback = std::true_type;
    explicit constexpr InsertKeyword() noexcept = default;

    [[nodiscard]] ParseResult operator()(Cursor& cursor) const noexcept;
};

struct GetIdentifier {
    using manages_rollback = std::true_type;
    explicit constexpr GetIdentifier() noexcept = default;

    [[nodiscard]] ParseResult operator()(Cursor& cursor) const noexcept;
};

struct GetConstant {
    using manages_rollback = std::true_type;
    explicit constexpr GetConstant() noexcept = default;

    [[nodiscard]] ParseResult operator()(Cursor& cursor) const noexcept;
};

template <ParserRule R>
struct CreateList {
    using manages_rollback = std::true_type;
    explicit constexpr CreateList() noexcept = default;
    explicit constexpr CreateList(R /* r */) noexcept { };

    [[nodiscard]] ParseResult operator()(Cursor& cursor) const noexcept;
};

template <ParserRule R>
struct CreateVector {
    using manages_rollback = std::true_type;
    explicit constexpr CreateVector() noexcept = default;
    explicit constexpr CreateVector(R /* r */) noexcept { };

    [[nodiscard]] ParseResult operator()(Cursor& cursor) const noexcept;
};

struct CreateNil {
    using manages_rollback = std::true_type;
    explicit constexpr CreateNil() noexcept = default;

    [[nodiscard]] ParseResult operator()(Cursor& cursor) const noexcept;
};

template <ParserRule Lhs, ParserRule Rhs>
struct Any {
    using manages_rollback = std::true_type;

    explicit constexpr Any() noexcept = default;
    explicit constexpr Any(Lhs /* lhs */, Rhs /* rhs */) noexcept { };

    [[nodiscard]] ParseResult operator()(Cursor& cursor) const noexcept;
};

template <ParserRule Lhs, ParserRule Rhs>
struct Then {
    using manages_rollback = std::false_type;

    explicit constexpr Then() noexcept = default;
    explicit constexpr Then(Lhs /* lhs */, Rhs /* rhs */) noexcept { };

    [[nodiscard]] ParseResult operator()(Cursor& cursor) const noexcept;
};

template <ParserRule R>
struct Maybe {
    using manages_rollback = std::true_type;

    explicit constexpr Maybe() noexcept = default;
    explicit constexpr Maybe(R /* r */) noexcept { };

    [[nodiscard]] ParseResult operator()(Cursor& cursor) const noexcept;
};

template <ParserRule R>
struct Many {
    using manages_rollback = std::true_type;

    explicit constexpr Many() noexcept = default;
    explicit constexpr Many(R /* r */) noexcept { };

    [[nodiscard]] ParseResult operator()(Cursor& cursor) const noexcept;
};

template <ParserRule R>
struct Must {
    using manages_rollback = std::true_type;

    explicit constexpr Must() noexcept = default;
    explicit constexpr Must(R /* r */) noexcept { };

    [[nodiscard]] ParseResult operator()(Cursor& cursor) const noexcept;
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
ParseResult OneToken<T>::operator()(Cursor& cursor) const noexcept {
    if (cursor.is<T>()) {
        cursor.advance();
        return std::vector<SpanRef> {};
    }
    return std::nullopt;
}

template <Keyword K>
ParseResult InsertKeyword<K>::operator()(Cursor& cursor) const noexcept {
    SpanRef node = cursor.arena().get_ident(cursor.loc(),
        std::string(lex_defs::KEYWORDS[static_cast<std::size_t>(K)]));
    return std::vector<SpanRef> { node };
}

ParseResult GetIdentifier::operator()(Cursor& cursor) const noexcept {
    SpanRef node = cursor.get_ident();
    if (!node.is_valid())
        return std::nullopt;
    cursor.advance();
    return std::vector<SpanRef> { node };
}

ParseResult GetConstant::operator()(Cursor& cursor) const noexcept {
    SpanRef node = cursor.get_constant();
    if (!node.is_valid())
        return std::nullopt;
    cursor.advance();
    return std::vector<SpanRef> { node };
}

template <ParserRule R>
ParseResult CreateList<R>::operator()(Cursor& cursor) const noexcept {
    if (cursor.is_failed())
        return std::nullopt;
    LocRef loc = cursor.loc();
    ParseResult res;
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
    SpanRef node
        = cursor.arena().from_loc(loc, SExprList(std::move(res.value())));
    return std::vector<SpanRef> { node };
}

template <ParserRule R>
ParseResult CreateVector<R>::operator()(Cursor& cursor) const noexcept {
    if (cursor.is_failed())
        return std::nullopt;
    LocRef loc = cursor.loc();
    ParseResult res;
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
    SpanRef node
        = cursor.arena().from_loc(loc, SExprVector(std::move(res.value())));
    return std::vector<SpanRef> { node };
}

ParseResult CreateNil::operator()(Cursor& cursor) const noexcept {
    SpanRef node = cursor.arena().nil(cursor.loc());
    return std::vector<SpanRef> { node };
}

template <ParserRule Lhs, ParserRule Rhs>
ParseResult Any<Lhs, Rhs>::operator()(Cursor& cursor) const noexcept {
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
ParseResult Then<Lhs, Rhs>::operator()(Cursor& cursor) const noexcept {
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
ParseResult Maybe<R>::operator()(Cursor& cursor) const noexcept {
    if constexpr (R::manages_rollback::value) {
        auto result = R()(cursor);
        if (result)
            return std::move(result.value());
        return {};
    } else {
        auto save = cursor.save();
        auto result = R()(cursor);
        if (!result) {
            cursor.set(save);
            return {};
        }
        return std::move(result.value());
    }
}

template <ParserRule R>
ParseResult Many<R>::operator()(Cursor& cursor) const noexcept {
    std::vector<SpanRef> result;
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
ParseResult Must<R>::operator()(Cursor& cursor) const noexcept {
    auto result = R()(cursor);
    if (!result) {
        cursor.fail();
    }
    return result;
}

} // namespace lpc::syntax::combinators
