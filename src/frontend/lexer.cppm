export module lpc.frontend.lexer;

import std;

import lpc.frontend.arenas;
import lpc.frontend.refs;
import lpc.frontend.token;
import lpc.utils.logging;

namespace lpc::frontend {
using lpc::utils::Error;

export class Lexer {
private:
    std::string_view _source;
    std::string_view _cursor;
    std::size_t _line = 1;
    std::string_view::iterator _line_start;
    std::vector<Token> _tokens;
    LocationArena& _loc_arena;
    std::uint32_t _file_idx;

    bool _failed = false;

public:
    explicit Lexer(LocationArena& loc_arena, std::string_view file,
        std::string_view source) noexcept
        : _source(source)
        , _cursor(source)
        , _line_start(source.begin())
        , _loc_arena(loc_arena)
        , _file_idx(_loc_arena.add_file(std::string(file))) {
        while (!is_eof() && !_failed)
            if (!advance() && !is_eof())
                _failed = true;
        if (_failed)
            return;
        _tokens.emplace_back(Token::eof(loc("<EOF>")));
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
    [[nodiscard]] inline constexpr std::uint32_t file_idx() const noexcept {
        return _file_idx;
    }

    [[nodiscard]] inline constexpr LocRef loc(std::string&& lexeme) noexcept {
        return _loc_arena.emplace(_file_idx, _line,
            (_line == 1 ? 1 : 0) + std::distance(_line_start, _cursor.begin()),
            std::move(lexeme));
    }

    [[nodiscard]] inline constexpr std::string loc_string() noexcept {
        return std::format("{}:{}:{}", _loc_arena.file(_file_idx), _line,
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

// TODO should not be here
export class Cursor {
private:
    const std::vector<Token>& _tokens;
    bool _failed = false;
    std::vector<Token>::const_iterator _token;
    SpanArena& _arena;

    struct SavePoint {
    private:
        std::vector<Token>::const_iterator _token;

        explicit constexpr SavePoint(
            std::vector<Token>::const_iterator token) noexcept
            : _token(token) { };

        friend class Cursor;
    };

public:
    explicit constexpr Cursor(
        const std::vector<Token>& tokens, SpanArena& arena) noexcept
        : _tokens(tokens)
        , _token(_tokens.begin())
        , _arena(arena) { };

    constexpr Cursor(const Cursor&) = delete;
    constexpr Cursor& operator=(const Cursor&) = delete;

    constexpr Cursor(Cursor&&) noexcept = default;

    [[nodiscard]] inline constexpr const Token& operator*() const noexcept {
        return *_token;
    }

    void advance() noexcept {
        if (_token->type() != TokenType::EOF)
            ++_token;
    }

    void fail() noexcept {
        if (!_failed) {
            auto loc = _arena.loc(this->loc());
            lpc::utils::Error("Unexpected token \"{}\" at {}", loc.lexeme(),
                loc.source_location());
            _failed = true;
        }
    }

    [[nodiscard]] inline constexpr bool is_failed() const noexcept {
        return _failed;
    }

    [[nodiscard]] inline constexpr bool is_eof() const noexcept {
        return _token->type() == TokenType::EOF;
    }

    [[nodiscard]] inline constexpr SavePoint save() const noexcept {
        return SavePoint(_token);
    }

    inline void set(SavePoint sp) noexcept {
        _token = sp._token;
    }

    [[nodiscard]] inline constexpr TokenType type() const noexcept {
        return _token->type();
    }

    [[nodiscard]] inline constexpr LocRef loc() const noexcept {
        return _token->loc();
    }

    [[nodiscard]] inline constexpr auto value() const& noexcept {
        return _token->value();
    }

    [[nodiscard]] inline SpanArena& arena() & noexcept {
        return _arena;
    }

    template <TokenType T>
    [[nodiscard]] constexpr bool is() const noexcept {
        return type() == T;
    }

    [[nodiscard]] SpanRef get_ident() noexcept;
    [[nodiscard]] SpanRef get_constant() noexcept;
};
} // namespace lpc::frontend
