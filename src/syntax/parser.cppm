export module lpc.syntax.syntax;

import std;

import lpc.context;
import lpc.passes;
import lpc.syntax.arenas;
import lpc.syntax.ast;
import lpc.syntax.cursor;
import lpc.syntax.refs;

namespace lpc::syntax {

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

export class ParsePass final : public Pass<std::vector<Token>, SpanRef> {
private:
    bool _failed = false;

public:
    [[nodiscard]] std::string name() const noexcept final {
        return "parse";
    }

    [[nodiscard]] SpanRef run(
        std::vector<Token> tokens, CompilerContext& ctx) noexcept final;

    [[nodiscard]] std::string dump(
        const SpanRef& result, CompilerContext& ctx) const noexcept final {
        return ctx.span_arena().dump_root(result);
    }

    [[nodiscard]] bool is_failed() const noexcept final {
        return _failed;
    }

    explicit ParsePass() noexcept = default;
};

} // namespace lpc::syntax
