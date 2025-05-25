export module lpc.frontend.token;

import std;

namespace lpc::frontend {

export namespace lex_defs {
    constexpr char COMMENT_START = ';';
    constexpr char NEWLINE = '\n';
    constexpr std::string_view WHITESPACE = " \n";
    constexpr std::string_view DELIMETER = " \n()\";";
    // constexpr std::string_view OPERATORS = "()'`,."; // and "#(", ",@"
    // constexpr std::string_view PECULIAR_IDENTIFIERS[3] = { "+", "-", "..." };
    constexpr std::string_view SPECIAL_INITIAL = "!$%&*/:<=>?^_~";
    constexpr std::string_view SPECIAL_SUBSEQUENT = "+-.@";
    constexpr std::string_view KEYWORDS[20]
        = { "and", "begin", "case", "cond", "define", "delay", "do", "else",
              "if", "lambda", "let", "let*", "letrec", "or", "quasiquote",
              "quote", "set!", "unquote", "unquote-splicing", "=>" };
}

export enum class TokenType : std::uint8_t {
    KEYWORD,
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

export [[nodiscard]] constexpr auto token_type_to_string(TokenType type)
    -> std::string_view {
    switch (type) {
    case TokenType::KEYWORD     : return "KEYWORD";
    case TokenType::IDENT       : return "IDENT";
    case TokenType::BOOLEAN     : return "BOOLEAN";
    case TokenType::NUMBER      : return "NUMBER";
    case TokenType::CHARACTER   : return "CHARACTER";
    case TokenType::STRING      : return "STRING";
    case TokenType::LPAREN      : return "LPAREN";
    case TokenType::RPAREN      : return "RPAREN";
    case TokenType::SHELL_LPAREN: return "SHELL_LPAREN";
    case TokenType::APOSTROPHE  : return "APOSTROPHE";
    case TokenType::BACKTICK    : return "BACKTICK";
    case TokenType::COMMA       : return "COMMA";
    case TokenType::COMMA_AT    : return "COMMA_AT";
    case TokenType::DOT         : return "DOT";
    }
    return "UNKNOWN";
}

export struct Location {
private:
    std::string_view _file; // location can never outlive the session
    std::size_t _line;
    std::size_t _column;

    friend std::ostream& operator<<(std::ostream& os, const Location& loc);

public:
    explicit constexpr Location(
        std::string_view file, std::size_t line, std::size_t column) noexcept
        : _file(file)
        , _line(line)
        , _column(column) {
    }

    [[nodiscard]] inline constexpr Location operator-(
        this Location lhs, std::size_t offset) {
        lhs._column -= offset;
        return lhs;
    }

    [[nodiscard]] constexpr auto file() const noexcept -> std::string_view {
        return _file;
    }

    [[nodiscard]] constexpr auto line() const noexcept -> std::size_t {
        return _line;
    }

    [[nodiscard]] constexpr auto column() const noexcept -> std::size_t {
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

    Token(const Token&) = default;

public:
    explicit constexpr Token(TokenType type, std::int64_t value,
        std::string&& literal, Location location) noexcept
        : _type(type)
        , _value_storage(value)
        , _literal(std::move(literal))
        , _location(location) {
    }

    explicit constexpr Token(TokenType type, bool value, std::string&& literal,
        Location location) noexcept
        : _type(type)
        , _value_storage(value)
        , _literal(std::move(literal))
        , _location(location) {
    }

    explicit constexpr Token(TokenType type, char value, std::string&& literal,
        Location location) noexcept
        : _type(type)
        , _value_storage(value)
        , _literal(std::move(literal))
        , _location(location) {
    }

    explicit constexpr Token(TokenType type, std::string&& value,
        std::string&& literal, Location location) noexcept
        : _type(type)
        , _value_storage(std::move(value))
        , _literal(std::move(literal))
        , _location(location) {
    }

    Token& operator=(const Token&) = delete;

    Token(Token&&) = default;
    Token& operator=(Token&&) = default;

    [[nodiscard]] constexpr auto copied() const noexcept -> Token {
        return { *this };
    }

    [[nodiscard]] constexpr auto type() const noexcept -> TokenType {
        return _type;
    }

    [[nodiscard]] constexpr auto value() const noexcept
        -> std::variant<std::int64_t, bool, char, std::string> {
        return _value_storage;
    }

    [[nodiscard]] constexpr auto literal() const noexcept -> std::string_view {
        return _literal;
    }

    [[nodiscard]] constexpr auto location() const noexcept -> const Location& {
        return _location;
    }

    [[nodiscard]] constexpr auto len() const noexcept -> std::size_t {
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
        value_str = std::to_string(std::get<std::int64_t>(token.value()));
        break;
    case TokenType::CHARACTER:
        switch (char c = std::get<char>(token.value())) {
        case ' ' : value_str = "#\\space"; break;
        case '\n': value_str = "#\\newline"; break;
        default  : value_str = "#\\" + std::string(1, c); break;
        }
        break;
    case TokenType::STRING: value_str = token._literal; break;
    default: // operators
        value_str = std::get<std::string>(token.value());
        break;
    }
    return os << token.location() << ": " << value_str;
}

} // namespace lpc::frontend
