export module lpc.frontend.ast;

import std;
import lpc.frontend.token;

namespace lpc::frontend {

//
// AST node types
export enum class NodeType : std::uint8_t {
    Program, // Top-level program
    Expression, // Generic expression
    List, // List expression
    Nil, // ()
    Quote, // Quoted expression '
    Function, // Function definition
    Lambda, // Lambda expression
    Conditional, // If-then-else
    Definition, // Define statement

    // Terminal nodes
    Symbol, // Symbol identifier
    Number, // Numeric literal
    String, // String literal
    Boolean, // Boolean (#t/#f)
};

export class TerminalASTNode;

export class ASTNode {
private:
    NodeType _type;
    Location _location;
    std::vector<std::unique_ptr<ASTNode>> _children;

public:
    explicit ASTNode(NodeType type, Location location)
        : _type(type)
        , _location(location) {
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
        return _type >= NodeType::Symbol && _type <= NodeType::Boolean;
    }

    [[nodiscard]] const std::vector<std::unique_ptr<ASTNode>>&
    children() const noexcept {
        return _children;
    }

    void add_child(std::unique_ptr<ASTNode> child) {
        _children.push_back(std::move(child));
    }
};

class TerminalASTNode : public ASTNode {
private:
    std::variant<std::int64_t, bool, char, std::string> _value;

public:
    explicit TerminalASTNode(NodeType type,
        std::variant<std::int64_t, bool, char, std::string> value,
        Location location)
        : ASTNode(type, location)
        , _value(std::move(value)) {
    }

    explicit TerminalASTNode(const TerminalASTNode&) = delete;
    TerminalASTNode& operator=(const TerminalASTNode&) = delete;

    TerminalASTNode(TerminalASTNode&&) = default;
    TerminalASTNode& operator=(TerminalASTNode&&) = default;

    [[nodiscard]] const std::variant<std::int64_t, bool, char, std::string>&
    value() const noexcept {
        return _value;
    }
};

} // namespace lpc::frontend
