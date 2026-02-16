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
    [[no_unique_address]] R r;

    explicit constexpr CreateList() noexcept = default;
    explicit constexpr CreateList(R r) noexcept : r(std::move(r)) { };

    [[nodiscard]] ParseResult operator()(Cursor& cursor) const noexcept;
};

template <ParserRule R>
struct CreateVector {
    using manages_rollback = std::true_type;
    [[no_unique_address]] R r;

    explicit constexpr CreateVector() noexcept = default;
    explicit constexpr CreateVector(R r) noexcept : r(std::move(r)) { };

    [[nodiscard]] ParseResult operator()(Cursor& cursor) const noexcept;
};

struct CreateNil {
    using manages_rollback = std::true_type;
    explicit constexpr CreateNil() noexcept = default;

    [[nodiscard]] ParseResult operator()(Cursor& cursor) const noexcept;
};

template <ParserRule R, auto F>
struct Map {
    using manages_rollback = typename R::manages_rollback;
    R rule;

    explicit constexpr Map() noexcept = default;
    explicit constexpr Map(R r) noexcept
        : rule(std::move(r)) { }

    [[nodiscard]] ParseResult operator()(Cursor& cursor) const noexcept {
        auto res = rule(cursor);
        if (res)
            return F(std::move(*res), cursor);
        return std::nullopt;
    }
};

template <ParserRule... Rules>
struct Choice {
    using manages_rollback = std::true_type;
    std::tuple<Rules...> rules;

    explicit constexpr Choice() noexcept = default;
    explicit constexpr Choice(Rules... r) noexcept
        : rules(std::move(r)...) { }

    [[nodiscard]] ParseResult operator()(Cursor& cursor) const noexcept {
        ParseResult result;
        auto try_rule = [&](const auto& rule) -> bool {
            using R = std::decay_t<decltype(rule)>;
            if constexpr (R::manages_rollback::value) {
                result = rule(cursor);
                return result.has_value();
            } else {
                auto save = cursor.save();
                result = rule(cursor);
                if (result)
                    return true;
                cursor.set(save);
                return false;
            }
        };

        bool found = std::apply(
            [&](const auto&... args) { return (try_rule(args) || ...); },
            rules);

        if (found)
            return result;
        return std::nullopt;
    }
};

template <ParserRule... Rules>
struct Sequence {
    using manages_rollback = std::false_type;
    std::tuple<Rules...> rules;

    explicit constexpr Sequence() noexcept = default;
    explicit constexpr Sequence(Rules... r) noexcept
        : rules(std::move(r)...) { }

    [[nodiscard]] ParseResult operator()(Cursor& cursor) const noexcept {
        std::vector<SpanRef> combined;
        bool ok = std::apply(
            [&](const auto&... args) {
                return ([&]() {
                    auto res = args(cursor);
                    if (!res)
                        return false;
                    combined.append_range(*res);
                    return true;
                }() && ...);
            },
            rules);

        if (ok)
            return combined;
        return std::nullopt;
    }
};

template <ParserRule R>
struct Maybe {
    using manages_rollback = std::true_type;
    [[no_unique_address]] R r;

    explicit constexpr Maybe() noexcept = default;
    explicit constexpr Maybe(R r) noexcept : r(std::move(r)) { };

    [[nodiscard]] ParseResult operator()(Cursor& cursor) const noexcept;
};

template <ParserRule R>
struct Many {
    using manages_rollback = std::true_type;
    [[no_unique_address]] R r;

    explicit constexpr Many() noexcept = default;
    explicit constexpr Many(R r) noexcept : r(std::move(r)) { };

    [[nodiscard]] ParseResult operator()(Cursor& cursor) const noexcept;
};

template <ParserRule R>
struct Must {
    using manages_rollback = std::true_type;
    [[no_unique_address]] R r;

    explicit constexpr Must() noexcept = default;
    explicit constexpr Must(R r) noexcept : r(std::move(r)) { };

    [[nodiscard]] ParseResult operator()(Cursor& cursor) const noexcept;
};

template <ParserRule R>
using Some = Sequence<R, Many<R>>;

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
        res = r(cursor);
        if (!res)
            return std::nullopt;
    } else {
        auto save = cursor.save();
        res = r(cursor);
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
        res = r(cursor);
        if (!res)
            return std::nullopt;
    } else {
        auto save = cursor.save();
        res = r(cursor);
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

template <ParserRule R>
ParseResult Maybe<R>::operator()(Cursor& cursor) const noexcept {
    if constexpr (R::manages_rollback::value) {
        auto result = r(cursor);
        if (result)
            return std::move(result.value());
        return {};
    } else {
        auto save = cursor.save();
        auto result = r(cursor);
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
        while (auto nl = r(cursor)) {
            if (result.capacity() - result.size() < nl->size())
                result.reserve(result.capacity() * 2);
            result.append_range(*nl);
        }
    } else {
        auto save = cursor.save();
        while (auto nl = r(cursor)) {
            if (result.capacity() - result.size() < nl->size())
                result.reserve(result.capacity() * 2);
            result.append_range(*nl);
            save = cursor.save();
        }
        cursor.set(save);
    }
    return result;
}

template <ParserRule R>
ParseResult Must<R>::operator()(Cursor& cursor) const noexcept {
    auto result = r(cursor);
    if (!result) {
        cursor.fail();
    }
    return result;
}

} // namespace lpc::syntax::combinators
