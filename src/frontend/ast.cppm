export module lpc.frontend.ast;

import std;
export import lpc.frontend.token;
import lpc.utils.arena;
import lpc.utils.tagged_union;
import lpc.utils.logging;

namespace lpc::frontend {

using lpc::utils::Arena;
using lpc::utils::TaggedUnion;

export using ScopeID = std::uint32_t;
export class LispNil { };

export inline constexpr bool operator==(
    const LispNil& /* lhs */, const LispNil& /* rhs */) noexcept {
    return true;
}

export class LispIdent {
public:
    std::string name;
    std::set<ScopeID> scopes;

    explicit LispIdent(std::string name, std::set<ScopeID> scopes = {})
        : name(std::move(name))
        , scopes(std::move(scopes)) {
    }

    [[nodiscard]] inline constexpr bool operator==(
        const LispIdent& other) const noexcept {
        return name == other.name && scopes == other.scopes;
    }
};
export using LispString = std::string;
export class SExpr;
export class SExprArena;
export using SExprRef = Arena<SExpr, std::uint32_t>::elem_ref;
export class SExprLocRef {
private:
    SExprRef _expr_ref;
    LocRef _loc_ref;

public:
    explicit SExprLocRef() noexcept = default;
    explicit SExprLocRef(SExprRef expr_ref, LocRef loc_ref) noexcept
        : _expr_ref(expr_ref)
        , _loc_ref(loc_ref) { };

    static constexpr SExprLocRef invalid() noexcept {
        return SExprLocRef(SExprRef::invalid(), LocRef::invalid());
    }

    [[nodiscard]] inline constexpr bool is_valid() const noexcept {
        return _expr_ref.is_valid();
    }

    [[nodiscard]] inline constexpr SExprRef expr_ref() const noexcept {
        return _expr_ref;
    }

    [[nodiscard]] inline constexpr LocRef loc_ref() const noexcept {
        return _loc_ref;
    }

    [[nodiscard]] inline constexpr bool operator==(
        const SExprLocRef& other) const noexcept {
        return expr_ref() == other.expr_ref() && loc_ref() == other.loc_ref();
    }
};

export struct SExprList {
    std::vector<SExprLocRef> elem;

    [[nodiscard]] explicit SExprList() noexcept = default;
    [[nodiscard]] explicit SExprList(
        std::vector<SExprLocRef>&& elements) noexcept
        : elem(std::move(elements)) { };
};

export inline constexpr bool operator==(
    const SExprList& lhs, const SExprList& rhs) noexcept {
    return lhs.elem == rhs.elem;
}

export struct SExprVector {
    std::vector<SExprLocRef> elem;
    [[nodiscard]] explicit SExprVector() noexcept = default;
    [[nodiscard]] explicit SExprVector(
        std::vector<SExprLocRef>&& elements) noexcept
        : elem(std::move(elements)) { };
};

export inline constexpr bool operator==(
    const SExprVector& lhs, const SExprVector& rhs) noexcept {
    return lhs.elem == rhs.elem;
}

export class SExpr
    : public TaggedUnion<LispNil, LispIdent, LispString, LispNumber, LispChar,
          LispBool, SExprList, SExprVector> {
public:
    using TaggedUnion = TaggedUnion<LispNil, LispIdent, LispString, LispNumber,
        LispChar, LispBool, SExprList, SExprVector>;

    template <typename... Args>
    [[nodiscard]] explicit SExpr(Args&&... args) noexcept
        : TaggedUnion(std::forward<Args>(args)...) {
    }
};

export inline constexpr bool operator==(
    const SExprLocRef& lhs, const SExprLocRef& rhs) noexcept {
    return lhs.expr_ref() == rhs.expr_ref() && lhs.loc_ref() == rhs.loc_ref();
}

export using ParseResult = std::optional<std::vector<SExprLocRef>>;

class SExprArena : Arena<SExpr, std::uint32_t> {
private:
    LocationArena _loc_arena;

public:
    using SExprRef = Arena<SExpr, std::uint32_t>::elem_ref;

    explicit SExprArena(LocationArena&& loc_arena)
        : _loc_arena(std::move(loc_arena)) { };

    inline void reserve(std::size_t size) {
        Arena::reserve(size);
    }

    [[nodiscard]] const SExpr& operator[](SExprLocRef ref) const& {
        return at(ref);
    }

    template <typename... Args>
    SExprLocRef emplace(LocRef loc, Args&&... args) {
        return SExprLocRef(Arena::emplace(std::forward<Args>(args)...), loc);
    }

    [[nodiscard]] inline Location location(SExprLocRef ref) const noexcept {
        return _loc_arena.at(ref.loc_ref());
    }

    [[nodiscard]] inline Location location(LocRef ref) const noexcept {
        return _loc_arena.at(ref);
    }

    [[nodiscard]] const SExpr& at(SExprRef ref) const&;
    [[nodiscard]] const SExpr& at(SExprLocRef ref) const&;

    [[nodiscard]] SExprLocRef nil(LocRef loc) noexcept;
    [[nodiscard]] SExprLocRef get_boolean(LocRef loc, bool value) noexcept;
    [[nodiscard]] SExprLocRef get_variable(
        LocRef loc, std::string&& name) noexcept;

    [[nodiscard]] std::string dump_root(SExprRef root) const;
    [[nodiscard]] std::string dump(SExprRef ref) const;

private:
    SExprRef _nil_node;
    std::unordered_map<std::string, SExprRef> _variables;
    std::pair<SExprRef, SExprRef> _boolean_nodes;
};

export class Cursor {
private:
    const std::vector<Token>& _tokens;
    bool _failed = false;
    std::vector<Token>::const_iterator _token;
    SExprArena& _arena;

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
        const std::vector<Token>& tokens, SExprArena& arena) noexcept
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
        if (!_failed) {
            auto loc = _arena.location(this->loc());
            lpc::utils::Error("Unexpected token \"{}\" at {}",
                loc.lexeme(), loc.source_location());
            _failed = true;
        }
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
        return _token->loc();
    }

    [[nodiscard]] inline constexpr auto value() const& noexcept {
        return _token->value();
    }

    [[nodiscard]] inline constexpr SExprArena& arena() & noexcept {
        return _arena;
    }

    [[nodiscard]] inline constexpr SExprArena& arena() const& noexcept {
        return _arena;
    }

    template <TokenType T>
    [[nodiscard]] constexpr bool is() const noexcept {
        return type() == T;
    }

    [[nodiscard]] SExprLocRef get_ident() const noexcept;
    [[nodiscard]] SExprLocRef get_constant() const noexcept;
};

} // namespace lpc::frontend
