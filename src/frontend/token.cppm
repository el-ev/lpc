export module lpc.frontend.token;

import std;

import lpc.frontend.span;
import lpc.frontend.refs;
import lpc.utils.tagged_union;

namespace lpc::frontend {

using lpc::utils::TaggedUnion;

export using LispNumber = std::int64_t;
export using LispChar = char;
export using LispBool = bool;

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
    ARROW
};

export enum class TokenType : std::uint8_t {
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
};

export [[nodiscard]] constexpr auto token_type_to_string(TokenType type)
    -> std::string_view {
    switch (type) {
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
    }
}

export class Token {
private:
    TokenType _type;
    LocRef _location;
    TaggedUnion<std::string, LispNumber, LispChar, LispBool> _value;

    Token(const Token&) = default;

public:
    template <typename T>
    explicit Token(TokenType type, LocRef location, T value)
        : _type(type)
        , _location(location)
        , _value(std::forward<T>(value)) {};

    explicit Token(TokenType type, LocRef location)
        : _type(type)
        , _location(location) { };

    Token(Token&&) = default;
    Token& operator=(Token&&) = default;

    [[nodiscard]] inline static constexpr Token eof(LocRef location) noexcept {
        return Token(TokenType::EOF, location);
    }

    [[nodiscard]] inline constexpr TokenType type() const noexcept {
        return _type;
    }

    [[nodiscard]] inline constexpr const auto& value() const& noexcept {
        return _value;
    }

    [[nodiscard]] inline constexpr LocRef loc() const noexcept {
        return _location;
    }
};

} // namespace lpc::frontend
