export module lpc.frontend.location;

import std;
import lpc.utils.arena;

namespace lpc::frontend {

export struct Location;
export class LocationArena;

struct Location {
private:
    std::string_view _file;
    std::uint32_t _line;
    std::uint32_t _column;

public:
    explicit constexpr Location(std::string_view file, std::uint32_t line,
        std::uint32_t column) noexcept
        : _file(file)
        , _line(line)
        , _column(column) {
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

    [[nodiscard]] auto to_string() const noexcept -> std::string {
        return std::format("{}:{}:{}", _file, _line, _column);
    }
};

class LocationArena
    : utils::Arena<std::pair<std::uint32_t, std::uint32_t>, std::uint32_t> {
private:
    std::string _file;

public:
    using Arena::Arena;
    using elem_ref = utils::Arena<std::pair<std::uint32_t, std::uint32_t>,
        std::uint32_t>::elem_ref;
    using LocRef = elem_ref;
    explicit LocationArena(std::string&& file) noexcept
        : _file(std::move(file)) {
    }

    [[nodiscard]] inline LocRef insert(
        std::uint32_t line, std::uint32_t column) {
        if (!Arena::empty() && Arena::back().first == line
            && Arena::back().second == column) {
            return Arena::back_ref();
        }
        return Arena::insert({ line, column });
    }

    [[nodiscard]] inline Location operator[](LocRef ref) const {
        auto [line, column] = Arena::at(ref);
        return Location(_file, line, column);
    }

    [[nodiscard]] inline Location at(LocRef ref) const {
        auto [line, column] = Arena::at(ref);
        return Location(_file, line, column);
    }
};

export using LocRef = LocationArena::LocRef;

} // namespace lpc::frontend
