export module lpc.frontend.syntax;

import std;
import lpc.frontend.ast;

namespace lpc::frontend {

using Node = ASTNode;
using NodeRef = ASTNodeArena::NodeRef;
using NodeList = std::vector<NodeRef>;
using OptNodeList = std::optional<NodeList>;

export class Parser {
private:
    std::vector<Token> _tokens;
    Cursor _cursor;
    ASTNodeArena _arena;
    NodeRef _root;

    void parse() noexcept;

public:
    explicit constexpr Parser(std::vector<Token>&& tokens) noexcept
        : _tokens(std::move(tokens))
        , _cursor(_tokens, _arena)
        , _root(NodeRef::invalid()) {

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

    [[nodiscard]] inline NodeRef root() noexcept {
        return _arena.back_ref();
    }

    [[nodiscard]] inline ASTNodeArena&& arena() noexcept {
        return std::move(_arena);
    }
};
}
