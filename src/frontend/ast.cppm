export module lpc.frontend.ast;

import std;
export import lpc.frontend.token;
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
    X(ExpressionLike)                                                          \
    X(Expression)                                                              \
    X(Variable)                                                                \
    X(Literal)                                                                 \
    X(Quotation)                                                               \
    X(Boolean)                                                                 \
    X(Number)                                                                  \
    X(Character)                                                               \
    X(String)                                                                  \
    X(CallLike)                                                                \
    X(ProcedureCall)                                                           \
    X(Lambda)                                                                  \
    X(Formals)                                                                 \
    X(Body)                                                                    \
    X(Sequence)                                                                \
    X(If)                                                                      \
    X(Assignment)                                                              \
    X(MacroUse)                                                                \
    X(MacroBlock)                                                              \
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
    X(Vector)                                                                  \
    X(Invalid)

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
        return Arena::emplace(std::forward<Args>(args)...);
    }

    inline void pop_back() noexcept {
        Arena::pop_back();
    }

    inline void reset_to(NodeRef ref) noexcept {
        Arena::reset_to(ref);
    }

    [[nodiscard]] NodeRef back_ref() const noexcept;
    [[nodiscard]] const ASTNode& at(NodeRef ref) const&;
    [[nodiscard]] const ASTNode* get(NodeRef ref) const noexcept;
};

export using NodeRef = ASTNodeArena::NodeRef;

class ASTNode {
private:
    using NodeList = std::vector<ASTNodeArena::NodeRef>;
    NodeType _type;
    LocRef _location;
    TaggedUnion<NodeList, Keyword, std::string, std::int64_t, char, bool>
        _value;

public:
    explicit ASTNode()
        : _type(NodeType::Invalid)
        , _location(LocRef::invalid()) { };

    template <typename T>
    explicit ASTNode(NodeType type, LocRef location, T&& value)
        : _type(type)
        , _location(location)
        , _value(std::forward<T>(value)) {
    }

    explicit ASTNode(const ASTNode&) = delete;
    ASTNode& operator=(const ASTNode&) = delete;

    ASTNode(ASTNode&&) noexcept = default;
    ASTNode& operator=(ASTNode&&) noexcept = default;

    virtual ~ASTNode() = default;

    [[nodiscard]] NodeType type() const noexcept {
        return _type;
    }

    [[nodiscard]] const LocRef& location() const noexcept {
        return _location;
    }

    [[nodiscard]] std::string dump_json(const ASTNodeArena& arena,
        const LocationArena& loc_arena, std::size_t indent = 0) const;
};

export class Cursor {
private:
    const std::vector<Token>& _tokens;
    bool _failed = false;
    std::vector<Token>::const_iterator _token;
    ASTNodeArena& _arena;

    struct SavePoint {
    private:
        std::vector<Token>::const_iterator _token;
        NodeRef _node;

        explicit constexpr SavePoint(
            std::vector<Token>::const_iterator token, NodeRef node) noexcept
            : _token(token)
            , _node(node) { };

        friend class Cursor;
    };

public:
    explicit constexpr Cursor(
        const std::vector<Token>& tokens, ASTNodeArena& arena) noexcept
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
        _failed = true;
    }

    [[nodiscard]] inline constexpr bool is_failed() const noexcept {
        return _failed;
    }

    [[nodiscard]] inline constexpr bool is_eof() const noexcept {
        return _token->type() == TokenType::EOF;
    }

    [[nodiscard]] inline constexpr SavePoint save() const noexcept {
        return SavePoint(_token, _arena.back_ref());
    }

    inline void set(SavePoint sp) noexcept {
        _token = sp._token;
        _arena.reset_to(sp._node);
    }

    [[nodiscard]] inline constexpr TokenType type() const noexcept {
        return _token->type();
    }

    [[nodiscard]] inline constexpr LocRef loc() const noexcept {
        return _token->location();
    }

    [[nodiscard]] inline constexpr auto value() const& noexcept {
        return _token->value();
    }

    [[nodiscard]] inline constexpr ASTNodeArena& arena() & noexcept {
        return _arena;
    }

    [[nodiscard]] inline constexpr ASTNodeArena& arena() const& noexcept {
        return _arena;
    }

    template <TokenType T>
    [[nodiscard]] constexpr bool is() const noexcept {
        return type() == T;
    }

    template <Keyword K>
    [[nodiscard]] constexpr bool is() const noexcept {
        return type() == TokenType::KEYWORD
            && value().get_unchecked<Keyword>() == K;
    }

    template <std::size_t Hash>
    [[nodiscard]] constexpr bool is() const noexcept {
        if (type() != TokenType::IDENT)
            return false;
        auto string_hash = [](const std::string& str) noexcept {
            std::size_t h = 14695981039346656037ULL;
            for (const char& it : str) {
                h ^= static_cast<std::size_t>(it);
                h *= 1099511628211ULL;
            }
            return h;
        };
        return string_hash(value().get_unchecked<std::string>()) == Hash;
    }

    [[nodiscard]] NodeRef get_keyword() const noexcept;
    [[nodiscard]] NodeRef get_ident() const noexcept;
    [[nodiscard]] NodeRef get_constant() const noexcept;
};

} // namespace lpc::frontend
