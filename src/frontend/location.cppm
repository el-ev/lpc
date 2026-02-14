export module lpc.frontend.location;

import std;
import lpc.utils.arena;

namespace lpc::frontend {

using lpc::utils::Arena;

export struct Location;
export class LocationArena;

struct Location {
private:
    std::string_view _file;
    std::uint32_t _line;
    std::uint32_t _column;
    std::string _lexeme;

public:
    explicit constexpr Location(std::string_view file, std::uint32_t line,
        std::uint32_t column, std::string&& lexeme) noexcept
        : _file(file)
        , _line(line)
        , _column(column)
        , _lexeme(std::move(lexeme)) {
    }

    [[nodiscard]] inline constexpr Location operator-(
        this Location lhs, std::uint32_t offset) {
        lhs._column -= offset;
        return lhs;
    }

    [[nodiscard]] constexpr auto file() const noexcept -> std::string_view {
        return _file;
    }

    [[nodiscard]] constexpr auto line() const noexcept -> std::uint32_t {
        return _line;
    }

    [[nodiscard]] constexpr auto column() const noexcept -> std::uint32_t {
        return _column;
    }

    [[nodiscard]] constexpr auto lexeme() const noexcept -> std::string_view {
        return _lexeme;
    }

    [[nodiscard]] auto source_location() const noexcept -> std::string {
        return std::format("{}:{}:{}", _file, _line, _column);
    }
};

export class LocationArena
    : Arena<std::tuple<std::uint32_t, std::uint32_t, std::uint32_t, std::string>,
          std::uint32_t> {
private:
    std::deque<std::string> _files;

public:
    using LocRef = Arena::elem_ref;
    explicit LocationArena() noexcept = default;

    std::uint32_t add_file(std::string file) {
        _files.push_back(std::move(file));
        return static_cast<std::uint32_t>(_files.size() - 1);
    }

    [[nodiscard]] inline LocRef emplace(std::uint32_t file_idx,
        std::uint32_t line, std::uint32_t column, std::string&& lexeme) {
        return Arena::emplace(file_idx, line, column, std::move(lexeme));
    }

    [[nodiscard]] inline std::string_view file(std::uint32_t idx) const noexcept {
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

export using LocRef = LocationArena::LocRef;

} // namespace lpc::frontend
