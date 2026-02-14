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
export class SpanArena;

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

    [[nodiscard]] inline std::uint32_t file_idx(LocRef ref) const noexcept {
        return std::get<0>(Arena::at(ref));
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

private:
    SExprRef _nil_node;
    std::pair<SExprRef, SExprRef> _bool_nodes;
};

export class SpanArena : Arena<SpanTag, Span, std::uint32_t> {
private:
    SExprArena _expr_arena;
    LocationArena _loc_arena;

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

    [[nodiscard]] SpanRef from_loc(LocRef loc, SExpr&& expr);
    [[nodiscard]] SpanRef expand(LocRef loc, SExpr&& expr, SpanRef parent);

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

    [[nodiscard]] SpanRef nil(
        LocRef loc, SpanRef parent = SpanRef::invalid()) noexcept;
    [[nodiscard]] SpanRef get_bool(
        LocRef loc, bool value, SpanRef parent = SpanRef::invalid()) noexcept;

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

    template <typename T>
    [[nodiscard]] inline const T* get(SpanRef ref) const noexcept {
        return expr(ref).get<T>();
    }
};

} // namespace lpc::frontend