export module lpc.frontend.token;

import std;
import lpc.frontend.location;
import lpc.utils.arena;

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

export enum class Keyword : std::uint8_t {
    AND,
    BEGIN,
    CASE,
    COND,
    DEFINE,
    DELAY,
    DO,
    ELSE,
    IF,
    LAMBDA,
    LET,
    LET_STAR,
    LET_REC,
    OR,
    QUASIQUOTE,
    QUOTE,
    SET,
    UNQUOTE,
    UNQUOTE_SPLICING,
    ARROW,
};

export enum class TokenType : std::uint8_t {
    KEYWORD,
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

    EOF,
    INVALID,
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
    case TokenType::EOF         : return "EOF";
    case TokenType::INVALID     : return "INVALID";
    }
    return "UNKNOWN";
}

export class Token {
private:
    TokenType _type;
    std::variant<std::int64_t, bool, char, std::string, Keyword> _value_storage;
    std::string _lexeme;
    LocRef _location;

    friend std::ostream& operator<<(std::ostream& os, const Token& token);

    Token(const Token&) = default;

public:
    explicit constexpr Token(TokenType type, std::int64_t value,
        std::string&& lexeme, LocRef location) noexcept
        : _type(type)
        , _value_storage(value)
        , _lexeme(std::move(lexeme))
        , _location(location) { };

    explicit constexpr Token(TokenType type, bool value, std::string&& lexeme,
        LocRef location) noexcept
        : _type(type)
        , _value_storage(value)
        , _lexeme(std::move(lexeme))
        , _location(location) { };

    explicit constexpr Token(TokenType type, char value, std::string&& lexeme,
        LocRef location) noexcept
        : _type(type)
        , _value_storage(value)
        , _lexeme(std::move(lexeme))
        , _location(location) { };

    explicit constexpr Token(TokenType type, std::string&& value,
        std::string&& lexeme, LocRef location) noexcept
        : _type(type)
        , _value_storage(std::move(value))
        , _lexeme(std::move(lexeme))
        , _location(location) { };

    explicit constexpr Token(TokenType type, Keyword keyword,
        std::string&& lexeme, LocRef location) noexcept
        : _type(type)
        , _value_storage(keyword)
        , _lexeme(std::move(lexeme))
        , _location(location) { };

    explicit constexpr Token(
        TokenType type, std::string&& lexeme, LocRef location) noexcept
        : _type(type)
        , _lexeme(std::move(lexeme))
        , _location(location) { };

    Token(Token&&) = default;
    Token& operator=(Token&&) = default;

    [[nodiscard]] constexpr auto copied() const noexcept -> Token {
        return { *this };
    }

    [[nodiscard]] constexpr auto type() const noexcept -> TokenType {
        return _type;
    }

    [[nodiscard]] constexpr auto value() const noexcept
        -> std::variant<std::int64_t, bool, char, std::string, Keyword> {
        return _value_storage;
    }

    [[nodiscard]] constexpr auto lexeme() const noexcept -> std::string_view {
        return _lexeme;
    }

    [[nodiscard]] constexpr auto location() const noexcept -> LocRef {
        return _location;
    }

    [[nodiscard]] constexpr auto len() const noexcept -> std::size_t {
        return _lexeme.length();
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
    case TokenType::STRING : value_str = token._lexeme; break;
    case TokenType::KEYWORD: {
        auto keyword = std::get<Keyword>(token.value());
        value_str = lex_defs::KEYWORDS[static_cast<std::size_t>(keyword)];
    }
    default: // operators
        value_str = std::get<std::string>(token.value());
        break;
    }
    return os << value_str;
}

} // namespace lpc::frontend
