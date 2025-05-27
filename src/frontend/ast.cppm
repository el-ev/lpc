export module lpc.frontend.ast;

import std;
import lpc.frontend.token;
import lpc.frontend.location;

namespace lpc::frontend {

// AST node types
// 7.1 Formal Syntax
#define NODE_TYPE_LIST(X)                                                      \
    X(Program)                                                                 \
    X(ExprOrDef)                                                               \
    X(Expression)                                                              \
    X(Variable)                                                                \
    X(Literal)                                                                 \
    X(Quotation)                                                               \
    X(Boolean)                                                                 \
    X(Number)                                                                  \
    X(Character)                                                               \
    X(String)                                                                  \
    X(ProcedureCall)                                                           \
    X(Lambda)                                                                  \
    X(Formals)                                                                 \
    X(Body)                                                                    \
    X(Sequence)                                                                \
    X(If)                                                                      \
    X(Assignment)                                                              \
    /* Derived */                                                              \
    X(Quasiquote)                                                              \
    /* TODO */                                                                 \
    X(Unquote)                                                                 \
    X(UnquoteSplicing)                                                         \
    X(MacroUse)                                                                \
    X(MacroBlock)                                                              \
    X(LetSyntax)                                                               \
    X(LetRecSyntax)                                                            \
    X(SyntaxSpec)                                                              \
    X(Definitions)                                                             \
    X(Definition)                                                              \
    X(SyntaxDefinition)                                                        \
    X(TransformerSpec)                                                         \
    X(SyntaxRule)                                                              \
    X(Pattern)                                                                 \
    X(Template)                                                                \
    X(Keyword)                                                                 \
    X(Datum)                                                                   \
    X(List)                                                                    \
    X(Vector)

#define ENUM_VALUE(name) name,
export enum class NodeType : std::uint8_t { NODE_TYPE_LIST(ENUM_VALUE) };
#undef ENUM_VALUE

#define CASE_STATEMENT(name)                                                   \
    case NodeType::name: return #name;
export [[nodiscard]] constexpr auto node_type_to_string(NodeType type)
    -> std::string_view {
    switch (type) { NODE_TYPE_LIST(CASE_STATEMENT) }
    return "Unknown";
}
#undef CASE_STATEMENT

// TODO Arena

export class ASTNode {
private:
    using Node = ASTNode;
    using NodePtr = std::unique_ptr<Node>;
    using NodeList = std::vector<NodePtr>;
    NodeType _type;
    // TODO: ref only
    LocRef _location;
    // TODO could move into variant
    NodeList _children;
    std::variant<Keyword, std::string, std::int64_t, char, bool> _value;

public:
    explicit ASTNode(NodeType type, LocRef location, NodeList&& children = {})
        : _type(type)
        , _location(location)
        , _children(std::move(children)) {
    }

    explicit ASTNode(NodeType type, LocRef location,
        std::variant<Keyword, std::string, std::int64_t, char, bool> value)
        : _type(type)
        , _location(location)
        , _value(std::move(value)) {
    }

    explicit ASTNode(const ASTNode&) = delete;
    ASTNode& operator=(const ASTNode&) = delete;

    ASTNode(ASTNode&&) = default;
    ASTNode& operator=(ASTNode&&) = default;

    virtual ~ASTNode() = default;

    [[nodiscard]] NodeType type() const noexcept {
        return _type;
    }

    [[nodiscard]] const LocRef& location() const noexcept {
        return _location;
    }

    [[nodiscard]] const NodeList& children() const& noexcept {
        return _children;
    }

    [[nodiscard]] NodeList&& children() noexcept {
        return std::move(_children);
    }

    void add_child(NodePtr child) {
        _children.push_back(std::move(child));
    }

    [[nodiscard]] std::string dump_json(
        const LocationArena& loc_arena, std::size_t indent = 0) const;
};

} // namespace lpc::frontend
