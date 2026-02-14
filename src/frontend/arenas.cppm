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
};

export class SExprArena : Arena<SExprTag, SExpr, std::uint32_t> {
private:
    LocationArena& _loc_arena;

public:
    explicit SExprArena(LocationArena& loc_arena)
        : _loc_arena(loc_arena) { };

    [[nodiscard]] inline LocationArena& location_arena() noexcept {
        return _loc_arena;
    }

    [[nodiscard]] inline const LocationArena& location_arena() const noexcept {
        return _loc_arena;
    }

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

    [[nodiscard]] std::size_t size() const noexcept;

    [[nodiscard]] SExprLocRef nil(LocRef loc) noexcept;
    [[nodiscard]] SExprLocRef get_boolean(LocRef loc, bool value) noexcept;

    [[nodiscard]] std::string dump_root(SExprRef root) const;
    [[nodiscard]] std::string dump(SExprRef ref) const;

private:
    SExprRef _nil_node;
    std::pair<SExprRef, SExprRef> _boolean_nodes;
};

} // namespace lpc::frontend