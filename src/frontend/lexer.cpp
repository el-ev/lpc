module lpc.frontend.lexer;

import std.compat;

namespace lpc::frontend {

namespace lex_defs {
    constexpr char COMMENT_START = ';';
    constexpr char NEWLINE = '\n';
    constexpr std::string_view WHITESPACE = " \n";
    constexpr std::string_view DELIMETER = " ()\";";
    // constexpr std::string_view OPERATORS = "()'`,."; // and "#(", ",@"
    constexpr std::string_view SPECIAL_INITIAL = "!$%&*/:<=>?^_~";
    constexpr std::string_view SPECIAL_SUBSEQUENT = "+-.@";
    // constexpr std::string_view PECULIAR_IDENTIFIERS[3] = { "+", "-", "..." };
    // constexpr std::string_view KEYWORDS[20]
    //     = { "quote", "lambda", "if", "set!", "begin", "cond", "and", "or",
    //           "case", "let", "let*", "letrec", "do", "delay", "quasiquote",
    //           "else", "=>", "define", "unquote", "unquote-splicing" };
}

bool Lexer::skip_atmosphere() noexcept {
    return skip_comment() || skip_whitespaces();
}

bool Lexer::skip_comment() noexcept {
    if (is_eof())
        return false;
    if (_cursor[0] == lex_defs::COMMENT_START) {
        auto nl_pos = _cursor.find(lex_defs::NEWLINE);
        if (nl_pos != std::string_view::npos) {
            _cursor.remove_prefix(nl_pos + 1);
            _line++;
            _line_start = _cursor.begin();
        } else {
            _cursor.remove_prefix(_cursor.size());
        }
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
        if (_cursor[0] == lex_defs::NEWLINE) {
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
    if (auto t = read_boolean())
        return t;
    if (auto t = read_character())
        return t;
    if (auto t = read_string())
        return t;
    return std::nullopt;
}

size_t split_till_delimeter(std::string_view str) {
    const char* it = std::ranges::find_if(str.begin(), str.end(), [](char c) {
        return std::ranges::find(lex_defs::DELIMETER, c)
            != lex_defs::DELIMETER.end();
    });
    if (it == str.end())
        return str.size();
    return std::distance(str.begin(), it);
}

// TODO handle keywords here
[[nodiscard]] std::optional<Token> Lexer::read_ident() noexcept {
    // <identifier> -> <initial> <subsequent>* | <peculiar identifier>
    // <initial> -> <letter> | <special initial>
    // <subsequent> -> <letter> | <digit> | <special subsequent>
    // <letter> -> [a-zA-Z]
    // <digit> -> [0-9]
    Location location = loc();
    auto size = split_till_delimeter(_cursor);
    if (std::ranges::find(lex_defs::SPECIAL_INITIAL, _cursor[0])
            != lex_defs::SPECIAL_INITIAL.end()
        || std::isalpha(_cursor[0]) != 0) {
        auto is_valid_subsequent = [](char c) {
            return std::isalpha(c) != 0 || std::isdigit(c) != 0
                || std::ranges::find(lex_defs::SPECIAL_SUBSEQUENT, c)
                != lex_defs::SPECIAL_SUBSEQUENT.end()
                || std::ranges::find(lex_defs::SPECIAL_INITIAL, c)
                != lex_defs::SPECIAL_INITIAL.end();
        };
        size_t pos = 1;
        while (pos < _cursor.size() && is_valid_subsequent(_cursor[pos]))
            pos++;

        auto ident = _cursor.substr(0, pos);
        _cursor.remove_prefix(pos);
        std::string value = std::string(ident);
        std::ranges::transform(value.begin(), value.end(), value.begin(),
            [](unsigned char c) { return std::tolower(c); });
        if (pos != size) {
            Error("Invalid identifier: \"", ident, "\" at", location);
            _failed = true;
            return std::nullopt;
        }
        return Token(
            TokenType::IDENT, std::move(value), std::string(ident), location);
    }
    // check for peculiar identifiers
    if (size == 1) {
        // '+' and '-'
        auto ident = _cursor.substr(0, 1);
        if (ident == "+" || ident == "-") {
            _cursor.remove_prefix(1);
            return Token(TokenType::IDENT, std::string(ident),
                std::string(ident), location);
        }
    } else if (size == 3) {
        // "..."
        auto ident = _cursor.substr(0, 3);
        if (ident == "...") {
            _cursor.remove_prefix(3);
            return Token(TokenType::IDENT, "...", "...", location);
        }
    }
    return std::nullopt;
}

// TODO: the standard accepts radix 2/8/10/16 complex numbers
//       only dec int is supported for now
[[nodiscard]] std::optional<Token> Lexer::read_number() noexcept {
    // <number> -> <num 10>
    // <num 10> -> <radix 10> <real 10>
    // <radix 10> -> <empty> | #d
    // <real 10> -> <sign> <uint 10>
    // <sign> -> <empty> | + | -
    // <uint 10> -> <digit 10>* #*
    // <digit 10> -> [0-9]
    Location location = loc();

    bool number_pattern = false;
    auto start = _cursor;
    auto size = split_till_delimeter(_cursor);

    if (_cursor.size() > 2 && _cursor.substr(0, 2) == "#d") {
        _cursor.remove_prefix(2);
        number_pattern = true;

        if (_cursor.empty()) {
            Error("Incomplete number literal at", location);
            _failed = true;
            return std::nullopt;
        }
    }

    auto value_start = _cursor;
    // check for sign
    if (_cursor[0] == '+' || _cursor[0] == '-') {
        _cursor.remove_prefix(1);
        number_pattern = true;
        if (_cursor.empty()) {
            Error("Incomplete number literal at", location);
            _failed = true;
            return std::nullopt;
        }
    }

    size_t digit_count = 0;
    while (digit_count < _cursor.size()
        && (std::isdigit(_cursor[digit_count])) != 0)
        digit_count++;
    if (digit_count == 0) {
        if (!number_pattern)
            return std::nullopt;
        Error("Incomplete number literal (missing digits) at", location);
        _failed = true;
        return std::nullopt;
    }
    // number_pattern = true;

    _cursor.remove_prefix(digit_count);

    size_t sharp_count = 0;
    while (sharp_count < _cursor.size() && _cursor[sharp_count] == '#')
        sharp_count++;

    if (std::ranges::distance(start.begin(), _cursor.begin()) + sharp_count
        != size) {
        Error("Invalid number literal: \"", start.substr(0, size), "\" at ",
            location);
        _failed = true;
        return std::nullopt;
    }

    return Token(TokenType::NUMBER,
        std::string(value_start.substr(
            std::distance(value_start.begin(), _cursor.begin()))),
        std::string(
            start.substr(std::distance(start.begin(), _cursor.begin()))),
        location);
}

[[nodiscard]] std::optional<Token> Lexer::read_boolean() noexcept {
    // <boolean> -> #t | #f
    Location location = loc();
    if (_cursor.length() < 2)
        return std::nullopt;
    if (_cursor.substr(0, 2) == "#t") {
        _cursor.remove_prefix(2);
        // TODO: value "t/f" seems bad
        return Token(TokenType::BOOLEAN, "t", "#t", location);
    }
    if (_cursor.substr(0, 2) == "#f") {
        _cursor.remove_prefix(2);
        return Token(TokenType::BOOLEAN, "f", "#f", location);
    }
    return std::nullopt;
}

[[nodiscard]] std::optional<Token> Lexer::read_character() noexcept {
    // <character> -> #\<char> | #\<char name>
    // <char> -> (any character)
    // <char name> -> newline | space
    // alphabetical characters must followed by a delimiter
    Location location = loc();
    if (_cursor.length() < 2)
        return std::nullopt;
    if (_cursor[0] != '#' || _cursor[1] != '\\')
        return std::nullopt;
    if (_cursor.length() == 2) {
        Error("Incomplete character literal at", location);
        _failed = true;
        return std::nullopt;
    }
    if (std::isalpha(_cursor[2]) != 0) {
        // find till the next delimiter
        auto end = split_till_delimeter(_cursor);
        // if size is not 3, check against <char name>s, case insensitive
        if (end != 3) {
            auto name = _cursor.substr(0, end);
            auto cmp_ci = [](std::string_view a, std::string_view b) {
                return std::equal(a.begin(), a.end(), b.begin(),
                    [](char c1, char c2) { return std::tolower(c1) == c2; });
            };
            if (cmp_ci(name, "#\\newline")) {
                _cursor.remove_prefix(end);
                return Token(
                    TokenType::CHARACTER, "\n", std::string(name), location);
            }
            if (cmp_ci(name, "#\\space")) {
                _cursor.remove_prefix(end);
                return Token(
                    TokenType::CHARACTER, " ", std::string(name), location);
            }
            Error("Invalid character name: \"", name, "\" at", location);
            _failed = true;
            return std::nullopt;
        }
        // if size is 3, it is a single character, fall through
    }
    // it is a single character
    auto character = _cursor.substr(0, 3);
    _cursor.remove_prefix(3);
    return Token(TokenType::CHARACTER, std::string(1, character[2]),
        std::string(character), location);
}

[[nodiscard]] std::optional<Token> Lexer::read_string() noexcept {
    // <string> -> "<string element>*"
    // <string element> -> <string char> | \\ | \"
    // <string char> -> [^"\\]
    if (_cursor[0] != '"')
        return std::nullopt;
    Location location = loc();

    std::string_view content_view = _cursor.substr(1);
    size_t end = std::string_view::npos;
    size_t search_start = 0;

    while (search_start < content_view.length()) {
        size_t current_find = content_view.find('"', search_start);
        if (current_find == std::string_view::npos) {
            Error("Unterminated string literal at", location);
            _failed = true;
            return std::nullopt;
        }
        size_t backslashes = 0;
        for (size_t j = current_find;
            j > search_start && content_view[j - 1] == '\\'; --j) {
            backslashes++;
        }
        if (backslashes % 2 == 0) {
            end = current_find;
            break;
        }
        search_start = current_find + 1;
    }

    if (end == std::string_view::npos) {
        Error("Unterminated string literal at", location);
        _failed = true;
        return std::nullopt;
    }

    std::string_view unescaped_value = content_view.substr(0, end);
    std::string lexeme(_cursor.substr(0, end + 2));
    _cursor.remove_prefix(end + 2);

    auto unescape = [](std::string_view str) {
        std::string result;
        for (size_t i = 0; i < str.size(); ++i) {
            if (str[i] == '\\') {
                if (i + 1 < str.size()) {
                    result += str[i + 1];
                    ++i;
                }
            } else {
                result += str[i];
            }
        }
        return result;
    };

    return Token(TokenType::STRING, unescape(unescaped_value),
        std::move(lexeme), location);
}

[[nodiscard]] std::optional<Token> Lexer::read_operator() noexcept {
    // ()'`,. #( ,@
    Location location = loc();
    switch (_cursor[0]) {
    case '(':
        _cursor.remove_prefix(1);
        return Token(TokenType::LPAREN, "(", "(", location);
    case ')':
        _cursor.remove_prefix(1);
        return Token(TokenType::RPAREN, ")", ")", location);
    case '\'':
        _cursor.remove_prefix(1);
        return Token(TokenType::APOSTROPHE, "'", "'", location);
    case '`':
        _cursor.remove_prefix(1);
        return Token(TokenType::BACKTICK, "`", "`", location);
    case ',':
        _cursor.remove_prefix(1);
        if (_cursor.size() > 1 && _cursor[0] == '@') {
            _cursor.remove_prefix(1);
            return Token(TokenType::COMMA_AT, ",@", ",@", location);
        }
        return Token(TokenType::COMMA, ",", ",", location);
    case '.': {
        // . requires a delimiter
        auto size = split_till_delimeter(_cursor);
        if (size > 1)
            return std::nullopt;
        _cursor.remove_prefix(1);
        return Token(TokenType::DOT, ".", ".", location);
    }
    case '#':
        if (_cursor.size() > 1 && _cursor[1] == '(') {
            _cursor.remove_prefix(2);
            return Token(TokenType::SHELL_LPAREN, "#(", "#(", location);
        }
        break;
    default: break;
    };
    return std::nullopt;
}

} // namespace lpc::frontend