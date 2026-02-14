export module lpc.frontend.span;

import std;

import lpc.frontend.refs;

namespace lpc::frontend {

export struct Location {
private:
    std::string_view _file;
    std::uint32_t _line;
    std::uint32_t _column;
    std::string _lexeme;

public:
    explicit constexpr Location(std::string_view file, std::uint32_t line,
        std::uint32_t column, std::string&& lexeme) noexcept
        : _file(file)
        , _line(line)
        , _column(column)
        , _lexeme(std::move(lexeme)) {
    }

    [[nodiscard]] inline constexpr Location operator-(
        this Location lhs, std::uint32_t offset) {
        lhs._column -= offset;
        return lhs;
    }

    [[nodiscard]] constexpr auto file() const noexcept -> std::string_view {
        return _file;
    }

    [[nodiscard]] constexpr auto line() const noexcept -> std::uint32_t {
        return _line;
    }

    [[nodiscard]] constexpr auto column() const noexcept -> std::uint32_t {
        return _column;
    }

    [[nodiscard]] constexpr auto lexeme() const noexcept -> std::string_view {
        return _lexeme;
    }

    [[nodiscard]] auto source_location() const noexcept -> std::string {
        return std::format("{}:{}:{}", _file, _line, _column);
    }
};

export class Span {
private:
    LocRef _loc;
    SExprRef _expr;
    SpanRef _parent;
    ScopeSetRef _scopes;

public:
    explicit Span(
        LocRef loc, SExprRef expr, SpanRef parent, ScopeSetRef scopes) noexcept
        : _loc(loc)
        , _expr(expr)
        , _parent(parent)
        , _scopes(scopes) { };

    [[nodiscard]] LocRef loc() const noexcept {
        return _loc;
    }
    [[nodiscard]] SExprRef expr() const noexcept {
        return _expr;
    }
    [[nodiscard]] SpanRef parent() const noexcept {
        return _parent;
    }
    [[nodiscard]] ScopeSetRef scopes() const noexcept {
        return _scopes;
    }
};

} // namespace lpc::frontend
