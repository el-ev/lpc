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
};

template <typename Wrapper>
struct Def {
    explicit constexpr Def() noexcept = default;

    [[nodiscard]] ParseResult operator()(Cursor& cursor) const noexcept {
        return Wrapper::rule()(cursor);
    }
};

template <TokenType T>
struct OneToken {
    explicit constexpr OneToken() noexcept = default;

    [[nodiscard]] ParseResult operator()(Cursor& cursor) const noexcept;
};

template <Keyword K>
struct InsertKeyword {
    explicit constexpr InsertKeyword() noexcept = default;

    [[nodiscard]] ParseResult operator()(Cursor& cursor) const noexcept;
};

struct GetIdentifier {
    explicit constexpr GetIdentifier() noexcept = default;

    [[nodiscard]] ParseResult operator()(Cursor& cursor) const noexcept;
};

struct GetConstant {
    explicit constexpr GetConstant() noexcept = default;

    [[nodiscard]] ParseResult operator()(Cursor& cursor) const noexcept;
};

template <ParserRule R>
struct CreateList {
    [[no_unique_address]] R r;

    explicit constexpr CreateList() noexcept = default;
    explicit constexpr CreateList(R r) noexcept : r(std::move(r)) { };

    [[nodiscard]] ParseResult operator()(Cursor& cursor) const noexcept;
};

template <ParserRule R>
struct CreateVector {
    [[no_unique_address]] R r;

    explicit constexpr CreateVector() noexcept = default;
    explicit constexpr CreateVector(R r) noexcept : r(std::move(r)) { };

    [[nodiscard]] ParseResult operator()(Cursor& cursor) const noexcept;
};

struct CreateNil {
    explicit constexpr CreateNil() noexcept = default;

    [[nodiscard]] ParseResult operator()(Cursor& cursor) const noexcept;
};

template <ParserRule R, auto F>
struct Map {
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
    std::tuple<Rules...> rules;

    explicit constexpr Choice() noexcept = default;
    explicit constexpr Choice(Rules... r) noexcept
        : rules(std::move(r)...) { }

    [[nodiscard]] ParseResult operator()(Cursor& cursor) const noexcept {
        ParseResult result;
        auto try_rule = [&](const auto& rule) -> bool {
            result = rule(cursor);
            return result.has_value();
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
    std::tuple<Rules...> rules;

    explicit constexpr Sequence() noexcept = default;
    explicit constexpr Sequence(Rules... r) noexcept
        : rules(std::move(r)...) { }

    [[nodiscard]] ParseResult operator()(Cursor& cursor) const noexcept {
        auto save = cursor.save();
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
        cursor.set(save);
        return std::nullopt;
    }
};

template <ParserRule R>
struct Maybe {
    [[no_unique_address]] R r;

    explicit constexpr Maybe() noexcept = default;
    explicit constexpr Maybe(R r) noexcept : r(std::move(r)) { };

    [[nodiscard]] ParseResult operator()(Cursor& cursor) const noexcept;
};

template <ParserRule R>
struct Many {
    [[no_unique_address]] R r;

    explicit constexpr Many() noexcept = default;
    explicit constexpr Many(R r) noexcept : r(std::move(r)) { };

    [[nodiscard]] ParseResult operator()(Cursor& cursor) const noexcept;
};

template <ParserRule R>
struct Must {
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
    auto res = r(cursor);
    if (!res)
        return std::nullopt;
    SpanRef node
        = cursor.arena().from_loc(loc, SExprList(std::move(res.value())));
    return std::vector<SpanRef> { node };
}

template <ParserRule R>
ParseResult CreateVector<R>::operator()(Cursor& cursor) const noexcept {
    if (cursor.is_failed())
        return std::nullopt;
    LocRef loc = cursor.loc();
    auto res = r(cursor);
    if (!res)
        return std::nullopt;
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
    auto result = r(cursor);
    if (result)
        return std::move(result.value());
    return {};
}

template <ParserRule R>
ParseResult Many<R>::operator()(Cursor& cursor) const noexcept {
    std::vector<SpanRef> result;
    while (auto nl = r(cursor))
        result.append_range(*nl);
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
