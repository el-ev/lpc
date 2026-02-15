export module lpc.syntax.cursor;

import std;

import lpc.syntax.arenas;
import lpc.syntax.refs;
import lpc.syntax.token;
import lpc.utils.logging;

namespace lpc::syntax {

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

} // namespace lpc::syntax
