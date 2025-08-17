export module lpc.frontend.syntax;

import std;
import lpc.frontend.ast;

namespace lpc::frontend {

export class Parser {
private:
    std::vector<Token> _tokens;
    Cursor _cursor;
    SExprArena _arena;
    SExprLocRef _root;

    void parse() noexcept;

public:
    explicit constexpr Parser(
        std::vector<Token>&& tokens, LocationArena&& location_arena) noexcept
        : _tokens(std::move(tokens))
        , _cursor(_tokens, _arena)
        , _arena(std::move(location_arena))
        , _root(SExprLocRef::invalid()) {
        _arena.reserve(_tokens.size() >> 2u);
        parse();
    };

    Parser(const Parser&) = delete;
    Parser& operator=(const Parser&) = delete;

    Parser(Parser&&) noexcept = default;

    [[nodiscard]] inline bool is_failed() const noexcept {
        return _cursor.is_failed();
    }

    [[nodiscard]] inline bool is_eof() const noexcept {
        return _cursor.is_eof();
    }

    [[nodiscard]] inline SExprLocRef root() noexcept {
        return _root;
    }

    [[nodiscard]] inline SExprArena&& arena() noexcept {
        return std::move(_arena);
    }
};
}
