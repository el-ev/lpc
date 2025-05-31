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
    X(List)                                                                    \
    X(Vector)                                                                  \
    X(Keyword)                                                                 \
    X(Quotation)                                                               \
    X(Variable)                                                                \
    X(Boolean)                                                                 \
    X(Number)                                                                  \
    X(Character)                                                               \
    X(String)                                                                  \
    X(ProcedureCall)                                                           \
    X(Lambda)                                                                  \
    X(Formals)                                                                 \
    X(Body)                                                                    \
    X(If)                                                                      \
    X(Assignment)                                                              \
    X(Definition)                                                              \
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
export class NodeArena;
export class NodeLocRef;
export using NodeList = std::vector<NodeLocRef>;
export using OptNodeList = std::optional<NodeList>;

class ASTNode {
private:
    using NodeList = std::vector<NodeLocRef>;
    NodeType _type;
    TaggedUnion<NodeList, Keyword, std::string, std::int64_t, char, bool>
        _value;

public:
    explicit ASTNode()
        : _type(NodeType::Invalid) { };

    template <typename T>
    explicit ASTNode(NodeType type, T&& value)
        : _type(type)
        , _value(std::forward<T>(value)) {};

    explicit ASTNode(const ASTNode&) = delete;
    ASTNode& operator=(const ASTNode&) = delete;

    ASTNode(ASTNode&&) noexcept = default;
    ASTNode& operator=(ASTNode&&) noexcept = default;

    [[nodiscard]] NodeType type() const noexcept {
        return _type;
    }

    [[nodiscard]] const auto& value() const& noexcept {
        return _value;
    }
};

class NodeLocRef {
private:
    using ASTNodeRef = Arena<ASTNode, std::uint32_t>::elem_ref;
    ASTNodeRef _node_ref;
    LocRef _loc_ref;

public:
    explicit NodeLocRef() noexcept = default;
    explicit NodeLocRef(ASTNodeRef node_ref, LocRef loc_ref) noexcept
        : _node_ref(node_ref)
        , _loc_ref(loc_ref) { };

    static constexpr NodeLocRef invalid() noexcept {
        return NodeLocRef(ASTNodeRef::invalid(), LocRef::invalid());
    }

    [[nodiscard]] inline constexpr bool is_valid() const noexcept {
        return _node_ref.is_valid();
    }

    [[nodiscard]] inline constexpr ASTNodeRef node_ref() const noexcept {
        return _node_ref;
    }

    [[nodiscard]] inline constexpr LocRef loc_ref() const noexcept {
        return _loc_ref;
    }
};

class NodeArena : Arena<ASTNode, std::uint32_t> {
private:
    LocationArena _loc_arena;

public:
    using ASTNodeRef = Arena<ASTNode, std::uint32_t>::elem_ref;

    explicit NodeArena(LocationArena&& loc_arena)
        : _loc_arena(std::move(loc_arena)) { };

    [[nodiscard]] const ASTNode& operator[](NodeLocRef ref) const& {
        return at(ref);
    }

    template <typename... Args>
    [[nodiscard]] NodeLocRef emplace(LocRef loc, Args&&... args) {
        return NodeLocRef(Arena::emplace(std::forward<Args>(args)...), loc);
    }

    inline void pop_back() noexcept {
        Arena::pop_back();
    }

    [[nodiscard]] inline Location location(NodeLocRef ref) const noexcept {
        return _loc_arena.at(ref.loc_ref());
    }

    [[nodiscard]] const ASTNode& at(NodeLocRef ref) const&;
    [[nodiscard]] const ASTNode* get(NodeLocRef ref) const noexcept;

    [[nodiscard]] NodeLocRef get_boolean(LocRef loc, bool value) noexcept;
    [[nodiscard]] NodeLocRef get_keyword(LocRef loc, Keyword keyword) noexcept;
    [[nodiscard]] NodeLocRef get_variable(
        LocRef loc, const std::string& name) noexcept;

    [[nodiscard]] NodeLocRef insert_keyword(Keyword keyword, LocRef loc);
    [[nodiscard]] NodeLocRef insert_variable(const std::string& name, LocRef loc);

    [[nodiscard]] std::string dump_json(
        NodeLocRef ref, std::size_t indent = 0) const;

    [[nodiscard]] std::string dump(
        NodeLocRef ref) const;

private:
    // std::unordered_map<Keyword, NodeLocRef> _keywords;
    std::array<ASTNodeRef, static_cast<std::size_t>(Keyword::COUNT)> _keywords;
    std::unordered_map<std::string, ASTNodeRef> _variables;
    std::pair<ASTNodeRef, ASTNodeRef> _boolean_nodes;
};

export class Cursor {
private:
    const std::vector<Token>& _tokens;
    bool _failed = false;
    std::vector<Token>::const_iterator _token;
    NodeArena& _arena;

    struct SavePoint {
    private:
        std::vector<Token>::const_iterator _token;

        explicit constexpr SavePoint(
            std::vector<Token>::const_iterator token) noexcept
            : _token(token) { };

        friend class Cursor;
    };

public:
    explicit constexpr Cursor(
        const std::vector<Token>& tokens, NodeArena& arena) noexcept
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
        return SavePoint(_token);
    }

    inline void set(SavePoint sp) noexcept {
        _token = sp._token;
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

    [[nodiscard]] inline constexpr NodeArena& arena() & noexcept {
        return _arena;
    }

    [[nodiscard]] inline constexpr NodeArena& arena() const& noexcept {
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

    [[nodiscard]] NodeLocRef get_keyword() const noexcept;
    [[nodiscard]] NodeLocRef get_ident() const noexcept;
    [[nodiscard]] NodeLocRef get_constant() const noexcept;
};

} // namespace lpc::frontend
