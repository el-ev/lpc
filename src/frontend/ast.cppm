export module lpc.frontend.ast;

import std;
import lpc.frontend.token;

namespace lpc::frontend {

// AST node types
export enum class NodeType : std::uint8_t {
    Program, // Top-level program
    Expression, // Generic expression
    List, // List expression
    Nil, // ()
    Quote, // Quoted expression '
    Lambda, // Lambda expression

    // If-then-else as well
    Cond, // (cond (clause1) (clause2) ...)
    CondClause, // an individual clause in cond
    Else, // Else branch

    Assignment, // set!

    // Let expressions
    Let,
    LetStar,
    LetRec,

    // Sequencing
    Sequence, // (begin expr1 expr2 ...)

    // Iteration
    Iteration, // (do expr1 expr2 ...)

    // Delayed evaluation
    Delay, // (delay expr)

    // Quasiquote, Unquote, UnquoteSplicing
    Quasiquote, // (quasiquote expr)
    Unquote, // (unquote expr)
    UnquoteSplicing, // (unquote-splicing expr)

    Definition, // Define statement
    SyntaxDefinition, // define-syntax

    Symbol, // Symbol identifier
    Keyword, // Keyword identifier
    Constant,

    Number, // Numeric literal
    String, // String literal
    Character, // Character literal
    Boolean, // Boolean (#t/#f)
    Token, // Raw token
};

export [[nodiscard]] constexpr auto node_type_to_string(NodeType type)
    -> std::string_view {
    switch (type) {
    case NodeType::Program   : return "Program";
    case NodeType::Expression: return "Expression";
    case NodeType::List      : return "List";
    case NodeType::Nil       : return "Nil";
    case NodeType::Quote     : return "Quote";
    case NodeType::Lambda    : return "Lambda";

    case NodeType::Cond      : return "Cond";
    case NodeType::CondClause: return "CondClause";
    case NodeType::Else      : return "Else";

    case NodeType::Assignment: return "Assignment";

    case NodeType::Let    : return "Let";
    case NodeType::LetStar: return "LetStar";
    case NodeType::LetRec : return "LetRec";

    case NodeType::Sequence       : return "Sequence";
    case NodeType::Iteration      : return "Iteration";
    case NodeType::Delay          : return "Delay";
    case NodeType::Quasiquote     : return "Quasiquote";
    case NodeType::Unquote        : return "Unquote";
    case NodeType::UnquoteSplicing: return "UnquoteSplicing";

    case NodeType::Definition      : return "Definition";
    case NodeType::SyntaxDefinition: return "SyntaxDefinition";
    case NodeType::Symbol          : return "Symbol";
    case NodeType::Keyword         : return "Keyword";

    case NodeType::Constant : return "Constants";
    case NodeType::Number   : return "Number";
    case NodeType::String   : return "String";
    case NodeType::Character: return "Character";
    case NodeType::Boolean  : return "Boolean";
    case NodeType::Token    : return "Token";
    }
    return "Unknown";
}

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
