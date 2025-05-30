export module lpc.session;

import std;

namespace lpc {

enum PrintPasses : std::uint8_t {
    None = 0,
    Token = 1u << 0u,
    SExpr = 1u << 1u,
    All = 0xFF,
};

inline constexpr bool operator&(PrintPasses lhs, PrintPasses rhs) noexcept {
    return static_cast<bool>(
        static_cast<std::underlying_type_t<PrintPasses>>(lhs)
        & static_cast<std::underlying_type_t<PrintPasses>>(rhs));
}

inline constexpr PrintPasses& operator|=(PrintPasses& lhs, PrintPasses rhs) noexcept {
    lhs = static_cast<PrintPasses>(
        static_cast<std::underlying_type_t<PrintPasses>>(lhs)
        | static_cast<std::underlying_type_t<PrintPasses>>(rhs));
    return lhs;
}

export class Session {
private:
    std::string_view _output_file_path;
    std::vector<std::string_view> _input_file_paths;
    PrintPasses _print_passes = PrintPasses::None;

public:
    explicit Session() = default;
    explicit Session(const Session&) = delete;
    Session& operator=(const Session&) = delete;

    Session(Session&&) = default;
    Session& operator=(Session&&) = default;

    void set_output_file(std::string_view path) {
        _output_file_path = path;
    }

    void set_input_files(std::vector<std::string_view>&& input_file_paths) {
        _input_file_paths = std::move(input_file_paths);
    }

    void set_print_passes(std::string_view passes_str) {
        _print_passes = PrintPasses::None;

        auto split_view = std::views::split(passes_str, ',')
            | std::views::transform([](auto&& subrange) {
                  return std::string_view(subrange.begin(), subrange.end());
              });

        std::ranges::for_each(split_view, [this](std::string_view pass) {
            if (pass == "token") {
                _print_passes |= PrintPasses::Token;
            } else if (pass == "sexpr") {
                _print_passes |= PrintPasses::SExpr;
            } else if (pass == "all") {
                _print_passes = PrintPasses::All;
            } else {
                std::cerr << "Unknown print pass: " << pass << '\n';
            }
        });
    }

    [[nodiscard]] int run() noexcept;
};

} // namespace lpc
