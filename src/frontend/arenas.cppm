export module lpc.frontend.arenas;

import std;

export import lpc.frontend.refs;
import lpc.frontend.ast;
import lpc.frontend.span;
import lpc.frontend.span;
import lpc.utils.arena;

namespace lpc::frontend {

using lpc::utils::Arena;

export class LocationArena;
export class SExprArena;
export class ScopeArena;
export class SpanArena;

class ScopeArena : Arena<ScopeSetTag, std::set<ScopeID>, std::uint32_t> {
private:
    std::map<std::set<ScopeID>, ScopeSetRef> _interned;

public:
    explicit ScopeArena() noexcept = default;

    [[nodiscard]] ScopeSetRef intern(std::set<ScopeID>&& scopes) {
        if (auto it = _interned.find(scopes); it != _interned.end())
            return it->second;
        auto ref = Arena::emplace(std::move(scopes));
        _interned[at(ref)] = ref;
        return ref;
    }

    [[nodiscard]] ScopeSetRef intern(const std::set<ScopeID>& scopes) {
        if (auto it = _interned.find(scopes); it != _interned.end())
            return it->second;
        auto ref = Arena::emplace(scopes);
        _interned[at(ref)] = ref;
        return ref;
    }

    [[nodiscard]] const std::set<ScopeID>& at(ScopeSetRef ref) const& {
        return Arena::at(ref);
    }

    [[nodiscard]] ScopeSetRef empty_set() {
        return intern(std::set<ScopeID> {});
    }
};

class LocationArena
    : Arena<LocTag,
          std::tuple<std::uint32_t, std::uint32_t, std::uint32_t, std::string>,
          std::uint32_t> {
private:
    std::deque<std::string> _files;

public:
    explicit LocationArena() noexcept = default;

    std::uint32_t add_file(std::string file) {
        _files.push_back(std::move(file));
        return static_cast<std::uint32_t>(_files.size() - 1);
    }

    [[nodiscard]] inline LocRef emplace(std::uint32_t file_idx,
        std::uint32_t line, std::uint32_t column, std::string&& lexeme) {
        return Arena::emplace(file_idx, line, column, std::move(lexeme));
    }

    [[nodiscard]] inline std::string_view file(
        std::uint32_t idx) const noexcept {
        return _files[idx];
    }

    [[nodiscard]] inline Location operator[](LocRef ref) const& {
        return at(ref);
    }

    [[nodiscard]] inline Location at(LocRef ref) const& {
        auto [file_idx, line, column, lexeme] = Arena::at(ref);
        return Location(_files[file_idx], line, column, std::string(lexeme));
    }
};

export class SExprArena : Arena<SExprTag, SExpr, std::uint32_t> {
public:
    explicit SExprArena() noexcept = default;

    [[nodiscard]] inline SExprRef emplace(SExpr&& expr) {
        return Arena::emplace(std::move(expr));
    }

    [[nodiscard]] const SExpr& at(SExprRef ref) const&;

    [[nodiscard]] SExprRef nil() noexcept;
    [[nodiscard]] SExprRef get_bool(bool value) noexcept;
    [[nodiscard]] SExprRef get_ident(const std::string& name) noexcept;

private:
    SExprRef _nil_node;
    std::pair<SExprRef, SExprRef> _bool_nodes;
    std::map<std::string, SExprRef> _ident_nodes;
};

export class SpanArena : Arena<SpanTag, Span, std::uint32_t> {
private:
    SExprArena _expr_arena;
    LocationArena _loc_arena;
    ScopeArena _scope_arena;

public:
    explicit SpanArena(SExprArena&& expr_arena, LocationArena&& loc_arena)
        : _expr_arena(std::move(expr_arena))
        , _loc_arena(std::move(loc_arena)) { };

    SpanArena(const SpanArena&) = delete;
    SpanArena(SpanArena&&) = delete;
    SpanArena& operator=(const SpanArena&) = delete;
    SpanArena& operator=(SpanArena&&) = delete;

    [[nodiscard]] SExprArena& expr_arena() noexcept {
        return _expr_arena;
    }

    [[nodiscard]] const SExprArena& expr_arena() const noexcept {
        return _expr_arena;
    }

    [[nodiscard]] LocationArena& location_arena() noexcept {
        return _loc_arena;
    }

    [[nodiscard]] const LocationArena& location_arena() const noexcept {
        return _loc_arena;
    }

    [[nodiscard]] ScopeArena& scope_arena() noexcept {
        return _scope_arena;
    }

    [[nodiscard]] const ScopeArena& scope_arena() const noexcept {
        return _scope_arena;
    }

    template <typename... Args>
    [[nodiscard]] SpanRef from_loc(LocRef loc, Args&&... args);
    template <typename... Args>
    [[nodiscard]] SpanRef expand(
        LocRef loc, SpanRef parent, ScopeSetRef scopes, Args&&... args);

    [[nodiscard]] const Span& at(SpanRef ref) const&;
    [[nodiscard]] inline const Span& operator[](SpanRef ref) const& {
        return at(ref);
    }

    [[nodiscard]] Location loc(SpanRef ref) const noexcept;
    [[nodiscard]] Location loc(LocRef ref) const noexcept;

    [[nodiscard]] LocRef loc_ref(SpanRef ref) const noexcept;

    [[nodiscard]] const SExpr& expr(SpanRef ref) const noexcept;
    [[nodiscard]] const SExpr& expr(SExprRef ref) const noexcept;

    [[nodiscard]] SExprRef expr_ref(SpanRef ref) const noexcept;

    [[nodiscard]] const std::set<ScopeID>& scopes(SpanRef ref) const noexcept;
    [[nodiscard]] ScopeSetRef scope_ref(SpanRef ref) const noexcept;

    [[nodiscard]] SpanRef nil(LocRef loc, SpanRef parent = SpanRef::invalid(),
        ScopeSetRef scopes = ScopeSetRef::invalid()) noexcept;
    [[nodiscard]] SpanRef get_bool(LocRef loc, bool value,
        SpanRef parent = SpanRef::invalid(),
        ScopeSetRef scopes = ScopeSetRef::invalid()) noexcept;
    [[nodiscard]] SpanRef get_ident(LocRef loc, const std::string& name,
        SpanRef parent = SpanRef::invalid(),
        ScopeSetRef scopes = ScopeSetRef::invalid()) noexcept;

    [[nodiscard]] bool is_core_binding(SpanRef ref) const noexcept;

    void walk(SpanRef ref, const std::function<void(SpanRef)>& f);

    [[nodiscard]] std::string dump_root(SpanRef root) const;
    [[nodiscard]] std::string dump(SpanRef ref) const;

    template <typename T>
    [[nodiscard]] inline bool isa(SpanRef ref) const noexcept {
        return expr(ref).isa<T>();
    }

    [[nodiscard]] inline bool is_nil(SpanRef ref) const noexcept {
        return expr(ref).isa<LispNil>();
    }

    [[nodiscard]] inline bool is_ident(SpanRef ref) const noexcept {
        return expr(ref).isa<LispIdent>();
    }

    [[nodiscard]] inline bool is_list(SpanRef ref) const noexcept {
        return expr(ref).isa<SExprList>();
    }

    template <typename T>
    [[nodiscard]] inline const T* get(SpanRef ref) const noexcept {
        return expr(ref).get<T>();
    }
};

template <typename... Args>
SpanRef SpanArena::from_loc(LocRef loc, Args&&... args) {
    auto expr_ref = _expr_arena.emplace(SExpr(std::forward<Args>(args)...));
    return emplace(loc, expr_ref, SpanRef::invalid(), _scope_arena.empty_set());
}

template <typename... Args>
SpanRef SpanArena::expand(
    LocRef loc, SpanRef parent, ScopeSetRef scopes, Args&&... args) {
    if (!scopes.is_valid())
        scopes = parent.is_valid() ? at(parent).scopes()
                                   : _scope_arena.empty_set();
    auto expr_ref = _expr_arena.emplace(SExpr(std::forward<Args>(args)...));
    return emplace(loc, expr_ref, parent, scopes);
}

} // namespace lpc::frontend