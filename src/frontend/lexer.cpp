module lpc.frontend.lexer;

import std;

import lpc.frontend.ast;
import lpc.utils.logging;

namespace lpc::frontend {

using lpc::utils::Error;

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
            _cursor.remove_prefix(nl_pos);
            _line++;
            _line_start = _cursor.begin();
            _cursor.remove_prefix(1);
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
    while (!is_eof()
        && std::ranges::find(lex_defs::WHITESPACE, _cursor[0])
            != lex_defs::WHITESPACE.end()) {
        if (_cursor[0] == lex_defs::NEWLINE) {
            _line++;
            _line_start = _cursor.begin();
        }
        _cursor.remove_prefix(1);
    }
    return true;
}

bool Lexer::advance() noexcept {
    while (skip_atmosphere())
        ;
    if (is_eof())
        return false;
    if (_cursor[0] == '#')
        return read_sharp();
    if (read_operator() || read_ident() || read_string() || read_number(0))
        return true;

    Error("Unrecognized token starting with '{}' at {}", _cursor[0],
        loc_string());
    _failed = true;
    return false;
}

bool Lexer::read_ident() noexcept {
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

        std::string_view ident = _cursor.substr(0, pos);
        std::string value = std::string(ident);
        std::ranges::transform(value.begin(), value.end(), value.begin(),
            [](unsigned char c) { return std::tolower(c); });
        if (pos != size) {
            Error("Invalid identifier: \"{}\" at {}", ident, loc_string());
            _failed = true;
            return false;
        }

        _tokens.emplace_back(
            TokenType::IDENT, loc(std::string(ident)), std::move(value));
        _cursor.remove_prefix(pos);
        return true;
    }
    // check for peculiar identifiers
    if (size == 1) {
        // '+' and '-'
        auto ident = _cursor.substr(0, 1);
        if (ident == "+" || ident == "-") {
            _tokens.emplace_back(
                TokenType::IDENT, loc(std::string(ident)), std::string(ident));
            _cursor.remove_prefix(1);
            return true;
        }
    } else if (size == 3) {
        // "..."
        auto ident = _cursor.substr(0, 3);
        if (ident == "...") {
            _tokens.emplace_back(
                TokenType::IDENT, loc("..."), std::string(ident));
            _cursor.remove_prefix(3);
            return true;
        }
    }
    return false;
}

bool Lexer::read_sharp() noexcept {
    // <boolean> -> #t | #f
    if (_cursor.length() < 2) {
        Error("Incomplete token \"#\" at {}", loc_string());
        return false;
    }
    char c = _cursor[1];
    if (c == 't' || c == 'f') {
        bool value = (c == 't');
        _tokens.emplace_back(
            TokenType::BOOLEAN, loc(value ? "#t" : "#f"), value);
        _cursor.remove_prefix(2);
        return true;
    }

    switch (c) {
    case '(': {
        _tokens.emplace_back(TokenType::SHELL_LPAREN, loc("#("));
        _cursor.remove_prefix(2);
        return true;
    }
    case 'b':
        return read_number(2);
    case 'o':
        return read_number(8);
    case 'd':
        return read_number(10);
    case 'x':
        return read_number(16);
    case '\\':
        return read_character();
    default: {
        Error("Invalid token: \"#{}\" at {}", c, loc_string());
        _failed = true;
        return false;
    }
    }
}

[[nodiscard]] constexpr bool is_digit_radixn(char c, int radix) {
    switch (radix) {
    case 2:
        return c == '0' || c == '1';
    case 8:
        return c >= '0' && c <= '7';
    case 10:
        return c >= '0' && c <= '9';
    case 16:
        return (std::isxdigit(c) != 0);
    default:
        return false;
    }
}

// TODO: only signed integers are supported for now
bool Lexer::read_number(int radix) noexcept {
    int radix_value = 10;
    bool number_pattern = false;
    auto value_start = _cursor;
    auto size = count_till_delimeter(_cursor);

    if (radix != 0) {
        // radix is explicitly provided
        value_start.remove_prefix(2);
        if (radix != 2 && radix != 8 && radix != 10 && radix != 16) {
            Error("Invalid radix: {} at {}", radix, loc_string());
            _failed = true;
            return false;
        }
        number_pattern = true;
        radix_value = radix;
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
            return false;
        if (pos.empty())
            Error("Incomplete number literal at {}", loc_string());
        else
            Error("Invalid number literal at {}. Expected radix-{} digit, "
                  "found '{}'",
                loc_string(), radix_value, pos[0]);
        _failed = true;
        return false;
    }
    // number_pattern = true;

    pos.remove_prefix(digit_count);

    std::size_t sharp_count = 0;
    while (sharp_count < pos.size() && pos[sharp_count] == '#')
        sharp_count++;

    pos.remove_prefix(sharp_count);

    if (std::ranges::distance(_cursor.begin(), pos.begin()) != (long)size) {
        Error("Invalid number literal: \"{}\" at {}", _cursor.substr(0, size),
            loc_string());
        _cursor = pos;
        _failed = true;
        return false;
    }

    auto prefix_len = static_cast<std::size_t>(
        std::distance(_cursor.begin(), value_start.begin()));
    std::string value_string(
        value_start.substr(0, size - prefix_len - sharp_count));
    std::int64_t value = std::stoll(value_string, nullptr, radix_value);
    _tokens.emplace_back(
        TokenType::NUMBER, loc(std::string(_cursor.substr(0, size))), value);
    _cursor.remove_prefix(size);
    return true;
}

bool Lexer::read_character() noexcept {
    // <character> -> #\<char> | #\<char name>
    // <char> -> (any character)
    // <char name> -> newline | space
    // alphabetical characters must followed by a delimiter

    // #\ handled in read_sharp
    if (_cursor.length() < 3) {
        Error("Incomplete character literal at {}", loc_string());
        _failed = true;
        return false;
    }
    if (std::isalpha(_cursor[2]) != 0) {
        // find till the next delimiter
        auto end = count_till_delimeter(_cursor);
        // if size is not 3, check against <char name>s, case insensitive
        if (end != 3) {
            std::string name(_cursor.substr(0, end));
            auto cmp_ci = [](std::string_view a, std::string_view b) {
                return std::ranges::equal(a, b,
                    [](char c1, char c2) { return std::tolower(c1) == c2; });
            };
            if (cmp_ci(name, "#\\newline")) {
                _tokens.emplace_back(
                    TokenType::CHARACTER, loc(std::move(name)), '\n');
                _cursor.remove_prefix(end);
                return true;
            }
            if (cmp_ci(name, "#\\space")) {
                _tokens.emplace_back(
                    TokenType::CHARACTER, loc(std::move(name)), ' ');
                _cursor.remove_prefix(end);
                return true;
            }
            Error("Invalid character name: \"{}\" at {}", name, loc_string());
            _failed = true;
            return false;
        }
        // if size is 3, it is a single character, fall through
    }
    // it is a single character
    auto character = _cursor.substr(0, 3);
    _tokens.emplace_back(
        TokenType::CHARACTER, loc(std::string(character)), character[2]);
    _cursor.remove_prefix(3);
    return true;
}

bool Lexer::read_string() noexcept {
    // <string> -> "<string element>*"
    // <string element> -> <string char> | \\ | \"
    // <string char> -> [^"\\]
    if (_cursor[0] != '"')
        return false;

    std::string_view content_view = _cursor.substr(1);
    std::size_t end = std::string_view::npos;
    std::size_t search_start = 0;

    while (search_start < content_view.length()) {
        std::size_t current_find = content_view.find('"', search_start);
        if (current_find == std::string_view::npos)
            break;
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
        Error("Unterminated string literal at {}", loc_string());
        _failed = true;
        _cursor.remove_prefix(_cursor.size());
        return true;
    }

    std::string_view unescaped_value = content_view.substr(0, end);

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

    _tokens.emplace_back(TokenType::STRING,
        loc(std::string(_cursor.substr(0, end + 2))),
        unescape(unescaped_value));
    _cursor.remove_prefix(end + 2);
    return true;
}

bool Lexer::read_operator() noexcept {
    switch (_cursor[0]) {
    case '(':
        _tokens.emplace_back(TokenType::LPAREN, loc("("));
        _cursor.remove_prefix(1);
        return true;
    case ')':
        _tokens.emplace_back(TokenType::RPAREN, loc(")"));
        _cursor.remove_prefix(1);
        return true;
    case '\'':
        _tokens.emplace_back(TokenType::APOSTROPHE, loc("'"));
        _cursor.remove_prefix(1);
        return true;
    case '`':
        _tokens.emplace_back(TokenType::BACKTICK, loc("`"));
        _cursor.remove_prefix(1);
        return true;
    case ',':
        if (_cursor.size() > 1 && _cursor[1] == '@') {
            _tokens.emplace_back(TokenType::COMMA_AT, loc(",@"));
            _cursor.remove_prefix(2);
            return true;
        }
        _tokens.emplace_back(TokenType::COMMA, loc(","));
        _cursor.remove_prefix(1);
        return true;
    case '.':
        // . requires a delimiter
        if (count_till_delimeter(_cursor) > 1)
            return false;
        _tokens.emplace_back(TokenType::DOT, loc("."));
        _cursor.remove_prefix(1);
        return true;
    default:
        return false;
    };
}

SpanRef Cursor::get_ident() noexcept {
    if (type() != TokenType::IDENT)
        return SpanRef::invalid();
    std::string name = *value().get_unchecked<std::string>();
    return arena().get_ident(loc(), name);
}

SpanRef Cursor::get_constant() noexcept {
    SpanRef ref;
    switch (type()) {
    case TokenType::NUMBER: {
        LispNumber v = *value().get_unchecked<LispNumber>();
        ref = arena().from_loc(loc(), SExpr(v));
        break;
    }
    case TokenType::BOOLEAN: {
        bool v = *value().get_unchecked<LispBool>();
        ref = arena().from_loc(loc(), SExpr(v));
        break;
    }
    case TokenType::CHARACTER: {
        char v = *value().get_unchecked<LispChar>();
        ref = arena().from_loc(loc(), SExpr(v));
        break;
    }
    case TokenType::STRING: {
        auto v = *value().get_unchecked<LispString>();
        ref = arena().from_loc(loc(), SExpr(std::move(v)));
        break;
    }
    default:
        break;
    }
    return ref;
}

} // namespace lpc::frontend
