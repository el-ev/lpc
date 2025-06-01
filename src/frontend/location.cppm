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
    std::string_view _lexeme;

public:
    explicit constexpr Location(std::string_view file, std::uint32_t line,
        std::uint32_t column, std::string_view lexeme) noexcept
        : _file(file)
        , _line(line)
        , _column(column)
        , _lexeme(lexeme) {
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

class LocationArena
    : Arena<std::tuple<std::uint32_t, std::uint32_t, std::string>,
          std::uint32_t> {
private:
    std::string _file;

public:
    using LocRef = Arena::elem_ref;
    explicit LocationArena(std::string&& file) noexcept
        : _file(std::move(file)) { };

    [[nodiscard]] inline LocRef emplace(
        std::uint32_t line, std::uint32_t column, std::string&& lexeme) {
        return Arena::emplace(line, column, std::move(lexeme));
    }

    [[nodiscard]] inline std::string_view file() const noexcept {
        return _file;
    }

    [[nodiscard]] inline Location operator[](LocRef ref) const& {
        return at(ref);
    }

    [[nodiscard]] inline Location at(LocRef ref) const& {
        auto [line, column, lexeme] = Arena::at(ref);
        return Location(_file, line, column, lexeme);
    }
};

export using LocRef = LocationArena::LocRef;

} // namespace lpc::frontend
