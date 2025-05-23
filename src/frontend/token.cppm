export module lpc.frontend.token;

import std.compat; // <cstdint>

namespace lpc::frontend {

export enum class TokenType : uint8_t {
    IDENT,
    BOOLEAN,
    NUMBER,
    CHARACTER,
    STRING,

    // operators
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
    std::variant<std::int64_t, bool, char, std::string> _value_storage;
    std::string _literal;
    Location _location;

    friend std::ostream& operator<<(std::ostream& os, const Token& token);

public:
    explicit Token(TokenType type, std::int64_t value, std::string&& literal,
        Location location) noexcept
        : _type(type)
        , _value_storage(value)
        , _literal(std::move(literal))
        , _location(location) {
    }

    explicit Token(TokenType type, bool value, std::string&& literal,
        Location location) noexcept
        : _type(type)
        , _value_storage(value)
        , _literal(std::move(literal))
        , _location(location) {
    }

    explicit Token(TokenType type, char value, std::string&& literal,
        Location location) noexcept
        : _type(type)
        , _value_storage(value)
        , _literal(std::move(literal))
        , _location(location) {
    }

    explicit Token(TokenType type, std::string&& value, std::string&& literal,
        Location location) noexcept
        : _type(type)
        , _value_storage(std::move(value))
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

    [[nodiscard]] auto value() const noexcept
        -> std::variant<std::int64_t, bool, char, std::string> {
        return _value_storage;
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
    std::string value_str;
    switch (token.type()) {
    case TokenType::IDENT:
        value_str = std::get<std::string>(token.value());
        break;
    case TokenType::BOOLEAN:
        value_str = std::get<bool>(token.value()) ? "#t" : "#f";
        break;
    case TokenType::NUMBER:
        // TODO it is string now
        value_str = std::get<std::string>(token.value());
        break;
    case TokenType::CHARACTER:
        switch (char c = std::get<char>(token.value())) {
        case ' ' : value_str = "#\\space"; break;
        case '\n': value_str = "#\\newline"; break;
        default  : value_str = "#\\" + std::string(1, c); break;
        }
        break;
    case TokenType::STRING:
        value_str = token._literal;
        break;
    default: // operators
        value_str = std::get<std::string>(token.value());
        break;
    }
    return os << token.location() << ": " << value_str;
}

} // namespace lpc::frontend