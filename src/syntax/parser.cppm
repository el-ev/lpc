export module lpc.syntax.syntax;

import std;

import lpc.core.arenas;
import lpc.core.refs;
import lpc.syntax.ast;
import lpc.syntax.cursor;

namespace lpc::syntax {

using namespace lpc::core;

export class Parser {
private:
    std::vector<Token> _tokens;
    SpanArena& _arena;
    Cursor _cursor;
    SpanRef _root;

    void parse() noexcept;

public:
    explicit constexpr Parser(
        std::vector<Token>&& tokens, SpanArena& arena) noexcept
        : _tokens(std::move(tokens))
        , _arena(arena)
        , _cursor(_tokens, _arena)
        , _root(SpanRef::invalid()) {
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

    [[nodiscard]] inline SpanRef root() noexcept {
        return _root;
    }
};
} // namespace lpc::syntax
