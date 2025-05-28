export module lpc.frontend.token;

export import lpc.frontend.location;

import std;
import lpc.utils.arena;
import lpc.utils.tagged_union;

namespace lpc::frontend {

using lpc::utils::TaggedUnion;

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
    TaggedUnion<Keyword, std::string, std::int64_t, char, bool> _value;
    std::string _lexeme;
    LocRef _location;

    Token(const Token&) = default;

public:
    template <typename T>
    explicit Token(
        TokenType type, T value, std::string&& lexeme, LocRef location)
        : _type(type)
        , _value(std::forward<T>(value))
        , _lexeme(std::move(lexeme))
        , _location(location) {};

    explicit Token(TokenType type, std::string&& value, std::string&& lexeme,
        LocRef location)
        : _type(type)
        , _value(std::move(value))
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

    [[nodiscard]] constexpr const auto& value() const noexcept {
        return _value;
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

} // namespace lpc::frontend
