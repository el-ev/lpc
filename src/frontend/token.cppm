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
    COUNT 
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
    }
}

export class Token {
private:
    TokenType _type;
    LocRef _location;
    std::string _lexeme;
    TaggedUnion<Keyword, std::string, std::int64_t, char, bool> _value;

    Token(const Token&) = default;

public:
    template <typename T>
    explicit Token(
        TokenType type, LocRef location, std::string&& lexeme, T value)
        : _type(type)
        , _location(location)
        , _lexeme(std::move(lexeme))
        , _value(std::forward<T>(value)) {};

    explicit Token(TokenType type, LocRef location, std::string&& lexeme)
        : _type(type)
        , _location(location)
        , _lexeme(std::move(lexeme)) { };

    Token(Token&&) = default;
    Token& operator=(Token&&) = default;

    [[nodiscard]] inline static constexpr Token eof(LocRef location) noexcept {
        return Token(TokenType::EOF, location, "<EOF>");
    }

    [[nodiscard]] inline constexpr TokenType type() const noexcept {
        return _type;
    }

    [[nodiscard]] inline constexpr const auto& value() const& noexcept {
        return _value;
    }

    [[nodiscard]] inline constexpr std::string_view lexeme() const noexcept {
        return _lexeme;
    }

    [[nodiscard]] inline constexpr LocRef location() const noexcept {
        return _location;
    }
};

} // namespace lpc::frontend
