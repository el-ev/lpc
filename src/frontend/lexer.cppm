export module lpc.frontend.lexer;

import std;
import lpc.frontend.token;

namespace lpc::frontend {

export class Lexer {
private:
    std::string_view _source;
    std::string_view _cursor;
    std::size_t _line = 1;
    std::string_view::iterator _line_start;
    std::vector<Token> _tokens;
    LocationArena _loc_arena;

    bool _failed = false;

public:
    explicit Lexer(std::string_view file, std::string_view source) noexcept
        : _source(source)
        , _cursor(source)
        , _line_start(source.begin())
        , _loc_arena(std::string(file)) {
        while (!is_eof() && !_failed)
            if (!advance() && !is_eof())
                _failed = true;
        if (_failed)
            return;
        _tokens.emplace_back(Token::eof(loc("<EOF>")));
    }

    [[nodiscard]] inline LocationArena&& loc_arena() noexcept {
        return std::move(_loc_arena);
    }

    [[nodiscard]] inline std::vector<Token>&& tokens() noexcept {
        return std::move(_tokens);
    }

    [[nodiscard]] inline bool is_eof() const noexcept {
        return _cursor.empty();
    }

    [[nodiscard]] inline bool is_failed() const noexcept {
        return _failed;
    }

private:
    [[nodiscard]] inline constexpr LocRef loc(std::string&& lexeme) noexcept {
        return _loc_arena.emplace(_line,
            (_line == 1 ? 1 : 0) + std::distance(_line_start, _cursor.begin()),
            std::move(lexeme));
    }

    [[nodiscard]] inline constexpr std::string loc_string() noexcept {
        return std::format("{}:{}:{}", _loc_arena.file(), _line,
            (_line == 1 ? 1 : 0) + std::distance(_line_start, _cursor.begin()));
    }

    [[nodiscard]] inline constexpr std::string loc_string(LocRef ref) noexcept {
        return _loc_arena.at(ref).source_location();
    }

    bool skip_atmosphere() noexcept;
    bool skip_comment() noexcept;
    bool skip_whitespaces() noexcept;

    [[nodiscard]] bool advance() noexcept;

    [[nodiscard]] bool read_ident() noexcept;
    [[nodiscard]] bool read_sharp() noexcept;
    [[nodiscard]] bool read_number(int radix = 0) noexcept;
    [[nodiscard]] bool read_boolean() noexcept;
    [[nodiscard]] bool read_character() noexcept;
    [[nodiscard]] bool read_string() noexcept;
    [[nodiscard]] bool read_operator() noexcept;
};
} // namespace lpc::frontend
