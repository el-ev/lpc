export module lpc.frontend.token;

import std.compat; // <cstdint>

namespace lpc::frontend {

export enum class TokenType : uint8_t {
    IDENT,
    BOOLEAN,
    NUMBER,
    CHARACTER,
    STRING,

    LPAREN,
    RPAREN,
    SHELL_LPAREN,
    APOSTROPHE,
    BACKTICK,
    COMMA,
    COMMA_AT,
    DOT,
};

export struct Location {
private:
    std::string_view _file; // location can never outlive the session
    std::size_t _line;
    std::size_t _column;

    friend std::ostream& operator<<(std::ostream& os, const Location& loc);

public:
    explicit Location(
        std::string_view file, std::size_t line, std::size_t column) noexcept
        : _file(file)
        , _line(line)
        , _column(column) {
    }

    [[nodiscard]] inline Location operator-(this Location lhs, size_t offset) {
        lhs._column -= offset;
        return lhs;
    }

    [[nodiscard]] auto file() const noexcept -> std::string_view {
        return _file;
    }

    [[nodiscard]] auto line() const noexcept -> std::size_t {
        return _line;
    }

    [[nodiscard]] auto column() const noexcept -> std::size_t {
        return _column;
    }

    [[nodiscard]] auto to_string() const noexcept -> std::string {
        return std::format("{}:{}:{}", _file, _line, _column);
    }
};

inline auto operator<<(std::ostream& os, const Location& loc) -> std::ostream& {
    return os << loc.to_string();
}

export class Token {
private:
    TokenType _type;
    std::string _value;
    std::string _literal;
    Location _location;

    friend std::ostream& operator<<(std::ostream& os, const Token& token);

public:
    explicit Token(TokenType type, std::string&& value, std::string&& literal,
        Location location) noexcept
        : _type(type)
        , _value(std::move(value))
        , _literal(std::move(literal))
        , _location(location) {
    }

    explicit Token(const Token&) = delete;
    Token& operator=(const Token&) = delete;

    Token(Token&&) = default;
    Token& operator=(Token&&) = default;

    [[nodiscard]] auto type() const noexcept -> TokenType {
        return _type;
    }

    [[nodiscard]] auto value() const noexcept -> std::string_view {
        return _value;
    }

    [[nodiscard]] auto literal() const noexcept -> std::string_view {
        return _literal;
    }

    [[nodiscard]] auto location() const noexcept -> const Location& {
        return _location;
    }

    [[nodiscard]] auto len() const noexcept -> std::size_t {
        return _literal.length();
    }
};

inline auto operator<<(std::ostream& os, const Token& token) -> std::ostream& {
    return os << token.location() << ": " << token.value();
}

} // namespace lpc::frontend