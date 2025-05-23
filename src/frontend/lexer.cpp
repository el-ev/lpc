module lpc.frontend.lexer;

import std;

namespace lpc::frontend {

namespace lex_defs {
    constexpr char COMMENT_START = ';';
    constexpr std::string_view WHITESPACE = " \t\n";
    constexpr std::string_view OPERATORS = "()'`,."; // and "#(", ",@"
    constexpr std::string_view SPECIAL_INITIAL = "!$%&*/:<=>?^_~";
    constexpr std::string_view SPECIAL_SUBSEQUENT = "+-.@";
    constexpr std::string_view PECULIAR_IDENTIFIERS[3] = { "+", "-", "@" };
    constexpr std::string_view KEYWORDS[20]
        = { "quote", "lambda", "if", "set!", "begin", "cond", "and", "or",
              "case", "let", "let*", "letrec", "do", "delay", "quasiquote",
              "else", "=>", "define", "unquote", "unquote-splicing" };
    constexpr std::string_view CHARACTER_NAME[2] = { "newline", "space" };
}

bool Lexer::skip_atmosphere() noexcept {
    return skip_comment() || skip_whitespaces();
}

bool Lexer::skip_comment() noexcept {
    if (is_eof())
        return false;
    if (_cursor[0] == lex_defs::COMMENT_START) {
        auto end = _cursor.find('\n');
        if (end == std::string_view::npos)
            end = _cursor.size();
        _cursor.remove_prefix(end);
        _line++;
        _line_start = _cursor.begin();
        return true;
    }
    return false;
}

bool Lexer::skip_whitespaces() noexcept {
    if (is_eof())
        return false;
    if (std::ranges::find(lex_defs::WHITESPACE, _cursor[0])
        == lex_defs::WHITESPACE.end())
        return false;
    while (std::ranges::find(lex_defs::WHITESPACE, _cursor[0])
        != lex_defs::WHITESPACE.end()) {
        if (_cursor[0] == '\n') {
            _line++;
            _line_start = _cursor.begin();
        }
        _cursor.remove_prefix(1);
    }
    return true;
}

[[nodiscard]] std::optional<Token> Lexer::advance() noexcept {
    while (skip_atmosphere())
        ;
    if (is_eof())
        return std::nullopt;
    if (auto t = read_operator())
        return t;
    if (auto t = read_ident())
        return t;
    if (auto t = read_number())
        return t;
    if (auto t = read_character())
        return t;
    if (auto t = read_string())
        return t;
    return std::nullopt;
}

[[nodiscard]] std::optional<Token> Lexer::read_ident() noexcept {
    return std::nullopt;
}
[[nodiscard]] std::optional<Token> Lexer::read_number() noexcept {
    return std::nullopt;
}
[[nodiscard]] std::optional<Token> Lexer::read_character() noexcept {
    return std::nullopt;
}
[[nodiscard]] std::optional<Token> Lexer::read_string() noexcept {
    return std::nullopt;
}
[[nodiscard]] std::optional<Token> Lexer::read_operator() noexcept {
    return std::nullopt;
}

} // namespace lpc::frontend