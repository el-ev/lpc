export module lpc.frontend.ast;

import std;
import lpc.frontend.token;
import lpc.frontend.location;
import lpc.utils.arena;
import lpc.utils.tagged_union;

namespace lpc::frontend {

using lpc::utils::Arena;
using lpc::utils::TaggedUnion;

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

export class ASTNode;
export class ASTNodeArena;

class ASTNodeArena : Arena<ASTNode, std::uint32_t> {
public:
    using NodeRef = Arena<ASTNode, std::uint32_t>::elem_ref;

    explicit ASTNodeArena() noexcept = default;

    [[nodiscard]] const ASTNode& operator[](NodeRef ref) const& {
        return at(ref);
    }

    [[nodiscard]] NodeRef emplace(ASTNode&& node);
    template <typename... Args>
    [[nodiscard]] NodeRef emplace(Args&&... args) {
        return Arena::emplace(
            ASTNode(std::forward<Args>(args)...));
    }

    inline void pop_back() {
        Arena::pop_back();
    }

    [[nodiscard]] NodeRef back_ref() const noexcept;
    [[nodiscard]] const ASTNode& at(NodeRef ref) const&;
    [[nodiscard]] const ASTNode* get(NodeRef ref) const noexcept;
};

class ASTNode {
private:
    using NodeList = std::vector<ASTNodeArena::NodeRef>;
    NodeType _type;
    LocRef _location;
    TaggedUnion<NodeList, Keyword, std::string, std::int64_t, char, bool>
        _value;

public:
    template <typename T>
    explicit ASTNode(NodeType type, LocRef location, T value)
        requires(std::same_as<std::remove_cvref_t<T>, Keyword>
                    || std::same_as<std::remove_cvref_t<T>, std::int64_t>
                    || std::same_as<std::remove_cvref_t<T>, char>
                    || std::same_as<std::remove_cvref_t<T>, bool>)
        : _type(type)
        , _location(location)
        , _value(std::forward<T>(value)) {
    }

    template <typename T>
    explicit ASTNode(NodeType type, LocRef location, T&& value)
        requires(std::same_as<T, NodeList> || std::same_as<T, std::string>)
        : _type(type)
        , _location(location)
        , _value(std::forward<T>(value)) {
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

    [[nodiscard]] NodeList&& children() noexcept {
        return std::move(_value.get_unchecked<NodeList>());
    }

    [[nodiscard]] const NodeList& children() const& noexcept {
        return _value.get_unchecked<NodeList>();
    }

    [[nodiscard]] std::string dump_json(
        const ASTNodeArena& arena,
        const LocationArena& loc_arena, std::size_t indent = 0) const;
};

} // namespace lpc::frontend
