export module lpc.frontend.ast;

import std;
import lpc.frontend.token;

namespace lpc::frontend {

// AST node types
// 7.1 Formal Syntax
#define NODE_TYPE_LIST(X)                                                      \
    X(Program)                                                                 \
    /* Expressions */                                                          \
    X(Expression)                                                              \
    X(Variable)                                                                \
    /* literals */                                                             \
    X(Literal)                                                                 \
    X(Quotation)                                                               \
    X(SelfEvaluating)                                                          \
    X(Boolean)                                                                 \
    X(Number)                                                                  \
    X(Character)                                                               \
    X(String)                                                                  \
    /* Procedures */                                                           \
    X(ProcedureCall)                                                           \
    X(Lambda)                                                                  \
    X(Formals)                                                                  \
    X(Body)                                                                    \
    X(Sequence)                                                                \
    /* Conditional */                                                          \
    X(If)                                                                      \
    X(Test)                                                                    \
    X(Consequent)                                                              \
    X(Alternate)                                                               \
    /* Binding constructs */                                                   \
    X(Assignment)                                                              \
    /* Derived */                                                              \
    X(DerivedExpression)                                                       \
    X(Conditional)                                                             \
    X(CondClause)                                                              \
    X(Else)                                                                    \
    X(Case)                                                                    \
    X(CaseClause)                                                              \
    X(And)                                                                     \
    X(Or)                                                                      \
    X(Let)                                                                     \
    X(LetStar)                                                                 \
    X(LetRec)                                                                  \
    X(Begin)                                                                   \
    X(Iteration)                                                               \
    X(IterSpec)                                                                \
    X(Delay)                                                                   \
    X(Quasiquote)                                                              \
    /* TODO */                                                                 \
    X(Unquote)                                                                 \
    X(UnquoteSplicing)                                                         \
    X(MacroUse)                                                                \
    X(MacroBlock)                                                              \
    X(SyntaxSpec)                                                              \
    X(Definition)                                                              \
    X(Define)                                                                  \
    X(DefFormals)                                                              \
    /* TODO transformer spec */                                                \
    X(SyntaxDefinition)                                                        \
    X(TransformerSpec)                                                         \
    X(Keyword)                                                                 \
    X(Token)

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

export class TerminalASTNode;

export class ASTNode {
private:
    using Node = ASTNode;
    using NodePtr = std::unique_ptr<Node>;
    using NodeList = std::vector<NodePtr>;
    NodeType _type;
    Location _location;
    NodeList _children;

public:
    explicit ASTNode(NodeType type, Location location, NodeList&& children = {})
        : _type(type)
        , _location(location)
        , _children(std::move(children)) {
    }

    explicit ASTNode(const ASTNode&) = delete;
    ASTNode& operator=(const ASTNode&) = delete;

    ASTNode(ASTNode&&) = default;
    ASTNode& operator=(ASTNode&&) = default;

    virtual ~ASTNode() = default;

    [[nodiscard]] NodeType type() const noexcept {
        return _type;
    }

    [[nodiscard]] const Location& location() const noexcept {
        return _location;
    }

    [[nodiscard]] bool is_terminal() const noexcept {
        return _type == NodeType::Token;
    }

    [[nodiscard]] const NodeList& children() const noexcept {
        return _children;
    }

    void add_child(NodePtr child) {
        _children.push_back(std::move(child));
    }

    [[nodiscard]] std::string dump(std::size_t indent = 0) const;
    [[nodiscard]] std::string dump_json(std::size_t indent = 0) const;
};

class TerminalASTNode : public ASTNode {
private:
    Token _token;

public:
    explicit TerminalASTNode(const Token& token)
        : ASTNode(NodeType::Token, token.location())
        , _token(token.copied()) { };

    explicit TerminalASTNode(const TerminalASTNode&) = delete;
    TerminalASTNode& operator=(const TerminalASTNode&) = delete;

    TerminalASTNode(TerminalASTNode&&) = default;
    TerminalASTNode& operator=(TerminalASTNode&&) = default;

    [[nodiscard]] const Token& token() const noexcept {
        return _token;
    }
};

} // namespace lpc::frontend
