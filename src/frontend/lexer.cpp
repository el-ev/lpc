module lpc.frontend.lexer;

import std;
import lpc.logging;

namespace lpc::frontend {

std::size_t count_till_delimeter(std::string_view str) {
    const char* it = std::ranges::find_if(str.begin(), str.end(), [](char c) {
        return std::ranges::find(lex_defs::DELIMETER, c)
            != lex_defs::DELIMETER.end();
    });
    if (it == str.end())
        return str.size();
    return std::ranges::distance(str.begin(), it);
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

std::optional<Token> Lexer::advance() noexcept {
    while (skip_atmosphere())
        ;
    if (is_eof())
        return std::nullopt;
    _loc = loc();
    if (_cursor[0] == '#')
        return read_sharp();
    if (auto t = read_operator())
        return t;
    if (auto t = read_ident())
        return t;
    if (auto t = read_string())
        return t;
    // implicitly read a decimal number
    if (auto t = read_number())
        return t;
    return std::nullopt;
}

std::optional<Token> Lexer::read_ident() noexcept {
    // <identifier> -> <initial> <subsequent>* | <peculiar identifier>
    // <initial> -> <letter> | <special initial>
    // <subsequent> -> <letter> | <digit> | <special subsequent>
    // <letter> -> [a-zA-Z]
    // <digit> -> [0-9]
    auto size = count_till_delimeter(_cursor);
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
        std::size_t pos = 1;
        while (pos < _cursor.size() && is_valid_subsequent(_cursor[pos]))
            pos++;

        auto ident = _cursor.substr(0, pos);
        _cursor.remove_prefix(pos);
        std::string value = std::string(ident);
        std::ranges::transform(value.begin(), value.end(), value.begin(),
            [](unsigned char c) { return std::tolower(c); });
        if (pos != size) {
            Error("Invalid identifier: \"", ident, "\" at", _loc);
            _failed = true;
            return std::nullopt;
        }

        const auto* it = std::ranges::lower_bound(lex_defs::KEYWORDS, value);
        if (it != std::end(lex_defs::KEYWORDS) && *it == value) {
            auto keyword = static_cast<Keyword>(
                std::distance(std::begin(lex_defs::KEYWORDS), it));
            return Token(
                TokenType::KEYWORD, keyword, std::move(value), _loc);
        }
        
        return Token(
            TokenType::IDENT, std::move(value), std::string(ident), _loc);
    }
    // check for peculiar identifiers
    if (size == 1) {
        // '+' and '-'
        auto ident = _cursor.substr(0, 1);
        if (ident == "+" || ident == "-") {
            _cursor.remove_prefix(1);
            return Token(
                TokenType::IDENT, std::string(ident), std::string(ident), _loc);
        }
    } else if (size == 3) {
        // "..."
        auto ident = _cursor.substr(0, 3);
        if (ident == "...") {
            _cursor.remove_prefix(3);
            return Token(TokenType::IDENT, std::string(ident), "...", _loc);
        }
    }
    return std::nullopt;
}

std::optional<Token> Lexer::read_sharp() noexcept {
    // <boolean> -> #t | #f
    if (_cursor.length() < 2) {
        Error("Incomplete token \"#\" at", _loc);
        return std::nullopt;
    }
    char c = _cursor[1];
    if (c == 't' || c == 'f') {
        bool value = (c == 't');
        _cursor.remove_prefix(2);
        return Token(TokenType::BOOLEAN, value, value ? "#t" : "#f", _loc);
    }

    std::optional<Token> token;
    switch (c) {
    case 'b' : token = read_number(2); break;
    case 'o' : token = read_number(8); break;
    case 'd' : token = read_number(10); break;
    case 'x' : token = read_number(16); break;
    case '\\': token = read_character(); break;
    default  : {
        Error("Invalid token: \"#", c, "\" at", _loc);
        _failed = true;
        return std::nullopt;
    }
    }
    return token;
}

[[nodiscard]] constexpr bool is_digit_radixn(char c, int radix) {
    switch (radix) {
    case 2 : return c == '0' || c == '1';
    case 8 : return c >= '0' && c <= '7';
    case 10: return c >= '0' && c <= '9';
    case 16: return (std::isxdigit(c) != 0);
    default: return false;
    }
}

// TODO: only signed integers are supported for now
std::optional<Token> Lexer::read_number(std::optional<int> radix) noexcept {
    int radix_value = 10;
    bool number_pattern = false;
    auto value_start = _cursor;
    auto size = count_till_delimeter(_cursor);

    if (radix.has_value()) {
        // radix is explicitly provided
        value_start.remove_prefix(2);
        if (*radix != 2 && *radix != 8 && *radix != 10 && *radix != 16) {
            Error("Invalid radix: ", *radix, " at", _loc);
            _failed = true;
            return std::nullopt;
        }
        number_pattern = true;
        radix_value = *radix;
    }

    auto pos = value_start;

    // check for sign
    if (pos[0] == '+' || pos[0] == '-')
        pos.remove_prefix(1);

    std::size_t digit_count = 0;
    while (digit_count < pos.size()
        && is_digit_radixn(pos[digit_count], radix_value))
        digit_count++;
    if (digit_count == 0) {
        if (!number_pattern)
            return std::nullopt;
        if (pos.empty())
            Error("Incomplete number literal at ", _loc);
        else
            Error("Invalid number literal at ", _loc, ". Expected radix-",
                radix_value, " digit, found '", pos[0], "'");
        _failed = true;
        return std::nullopt;
    }
    // number_pattern = true;

    pos.remove_prefix(digit_count);

    std::size_t sharp_count = 0;
    while (sharp_count < pos.size() && pos[sharp_count] == '#')
        sharp_count++;

    pos.remove_prefix(sharp_count);

    if (std::ranges::distance(_cursor.begin(), pos.begin()) != (long)size) {
        Error("Invalid number literal: \"", _cursor.substr(0, size), "\" at ",
            _loc);
        _cursor = pos;
        _failed = true;
        return std::nullopt;
    }

    std::string value_string(value_start.substr(0, size - sharp_count));
    std::int64_t value = std::stoll(value_string, nullptr, radix_value);
    std::string lexeme = std::string(_cursor.substr(0, size));
    _cursor.remove_prefix(size);
    return Token(TokenType::NUMBER, value, std::move(lexeme), _loc);
}

std::optional<Token> Lexer::read_character() noexcept {
    // <character> -> #\<char> | #\<char name>
    // <char> -> (any character)
    // <char name> -> newline | space
    // alphabetical characters must followed by a delimiter

    // #\ handled in read_sharp
    if (_cursor.length() < 3) {
        Error("Incomplete character literal at ", _loc);
        _failed = true;
        return std::nullopt;
    }
    if (std::isalpha(_cursor[2]) != 0) {
        // find till the next delimiter
        auto end = count_till_delimeter(_cursor);
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
                    TokenType::CHARACTER, '\n', std::string(name), _loc);
            }
            if (cmp_ci(name, "#\\space")) {
                _cursor.remove_prefix(end);
                return Token(
                    TokenType::CHARACTER, ' ', std::string(name), _loc);
            }
            Error("Invalid character name: \"", name, "\" at", _loc);
            _failed = true;
            return std::nullopt;
        }
        // if size is 3, it is a single character, fall through
    }
    // it is a single character
    auto character = _cursor.substr(0, 3);
    _cursor.remove_prefix(3);
    return Token(
        TokenType::CHARACTER, character[2], std::string(character), _loc);
}

std::optional<Token> Lexer::read_string() noexcept {
    // <string> -> "<string element>*"
    // <string element> -> <string char> | \\ | \"
    // <string char> -> [^"\\]
    if (_cursor[0] != '"')
        return std::nullopt;

    std::string_view content_view = _cursor.substr(1);
    std::size_t end = std::string_view::npos;
    std::size_t search_start = 0;

    while (search_start < content_view.length()) {
        std::size_t current_find = content_view.find('"', search_start);
        if (current_find == std::string_view::npos) {
            Error("Unterminated string literal at", _loc);
            _failed = true;
            return std::nullopt;
        }
        std::size_t backslashes = 0;
        for (std::size_t j = current_find;
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
        Error("Unterminated string literal at", _loc);
        _failed = true;
        return std::nullopt;
    }

    std::string_view unescaped_value = content_view.substr(0, end);
    std::string lexeme(_cursor.substr(0, end + 2));
    _cursor.remove_prefix(end + 2);

    auto unescape = [](std::string_view str) {
        std::string result;
        for (std::size_t i = 0; i < str.size(); ++i) {
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

    return Token(
        TokenType::STRING, unescape(unescaped_value), std::move(lexeme), _loc);
}

std::optional<Token> Lexer::read_operator() noexcept {
    // ()'`,. #( ,@
    switch (_cursor[0]) {
    case '(':
        _cursor.remove_prefix(1);
        return Token(
            TokenType::LPAREN, std::string("("), std::string("("), _loc);
    case ')':
        _cursor.remove_prefix(1);
        return Token(
            TokenType::RPAREN, std::string(")"), std::string(")"), _loc);
    case '\'':
        _cursor.remove_prefix(1);
        return Token(
            TokenType::APOSTROPHE, std::string("'"), std::string("'"), _loc);
    case '`':
        _cursor.remove_prefix(1);
        return Token(
            TokenType::BACKTICK, std::string("`"), std::string("`"), _loc);
    case ',':
        _cursor.remove_prefix(1);
        if (_cursor.size() > 1 && _cursor[0] == '@') {
            _cursor.remove_prefix(1);
            return Token(TokenType::COMMA_AT, std::string(",@"),
                std::string(",@"), _loc);
        }
        return Token(
            TokenType::COMMA, std::string(","), std::string(","), _loc);
    case '.': {
        // . requires a delimiter
        auto size = count_till_delimeter(_cursor);
        if (size > 1)
            return std::nullopt;
        _cursor.remove_prefix(1);
        return Token(TokenType::DOT, std::string("."), std::string("."), _loc);
    }
    case '#':
        if (_cursor.size() > 1 && _cursor[1] == '(') {
            _cursor.remove_prefix(2);
            return Token(TokenType::SHELL_LPAREN, std::string("#("),
                std::string("#("), _loc);
        }
        break;
    default: break;
    };
    return std::nullopt;
}

} // namespace lpc::frontend
