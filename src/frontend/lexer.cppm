export module lpc.frontend.lexer;

import std;
import lpc.logging;
import lpc.frontend.token;

namespace lpc::frontend {

export class Lexer {
private:
    std::string_view _file;
    std::string_view _source;
    std::string_view _cursor;
    std::size_t _line = 1;
    std::string_view::iterator _line_start;
    std::vector<Token> _tokens;
    bool _failed = false;
    Location _loc;

public:
    explicit Lexer(std::string_view file, std::string_view source) noexcept
        : _file(file)
        , _source(source)
        , _cursor(source)
        , _line_start(source.begin())
        , _loc(file, 1, 0) {
        while (!is_eof() && !_failed) {
            if (auto token = advance()) {
                _tokens.push_back(std::move(*token));
            } else if (!is_eof()) {
                _failed = true;
                break;
            }
        }
    }

    using token_iterator = std::vector<Token>::iterator;
    using const_token_iterator = std::vector<Token>::const_iterator;

    [[nodiscard]] constexpr auto& tokens() const noexcept {
        return _tokens;
    }

    [[nodiscard]] inline auto begin() noexcept -> token_iterator {
        return _tokens.begin();
    }

    [[nodiscard]] inline auto end() noexcept -> token_iterator {
        return _tokens.end();
    }

    [[nodiscard]] inline auto begin() const noexcept -> const_token_iterator {
        return _tokens.cbegin();
    }

    [[nodiscard]] inline auto end() const noexcept -> const_token_iterator {
        return _tokens.cend();
    }

    [[nodiscard]] inline bool is_eof() const noexcept {
        return _cursor.empty();
    }

    [[nodiscard]] inline bool is_failed() const noexcept {
        return _failed;
    }

private:
    [[nodiscard]] inline Location loc() const noexcept {
        return Location(_file, _line,
            (_line == 1 ? 1 : 0) + std::distance(_line_start, _cursor.begin()));
    }

    bool skip_atmosphere() noexcept;
    bool skip_comment() noexcept;
    bool skip_whitespaces() noexcept;

    [[nodiscard]] std::optional<Token> advance() noexcept;

    [[nodiscard]] std::optional<Token> read_ident() noexcept;
    [[nodiscard]] std::optional<Token> read_sharp() noexcept;
    [[nodiscard]] std::optional<Token> read_number(
        std::optional<int> radix = std::nullopt) noexcept;
    [[nodiscard]] std::optional<Token> read_boolean() noexcept;
    [[nodiscard]] std::optional<Token> read_character() noexcept;
    [[nodiscard]] std::optional<Token> read_string() noexcept;
    [[nodiscard]] std::optional<Token> read_operator() noexcept;
};
} // namespace lpc::frontend