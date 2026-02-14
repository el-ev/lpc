export module lpc.frontend.span;

import std;

namespace lpc::frontend {

export struct Location {
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

} // namespace lpc::frontend