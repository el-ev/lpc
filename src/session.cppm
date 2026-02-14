export module lpc.session;

import std;

import lpc.context;

namespace lpc {

export class Session {
private:
    std::vector<std::string> _input_file_paths;
    std::string _output_file_path;
    CompilerOptions _options;

public:
    explicit Session() = default;
    explicit Session(const Session&) = delete;
    Session& operator=(const Session&) = delete;

    Session(Session&&) = default;
    Session& operator=(Session&&) = default;

    void set_output_file(std::string&& path) {
        _output_file_path = std::move(path);
    }

    void set_input_files(std::vector<std::string>&& input_file_paths) {
        _input_file_paths = std::move(input_file_paths);
    }

    void set_print_passes(std::string_view passes_str) {
        auto split_view = std::views::split(passes_str, ',')
            | std::views::transform([](auto&& subrange) {
                  return std::string_view(subrange.begin(), subrange.end());
              });

        std::ranges::for_each(split_view, [this](std::string_view pass) {
            _options.print_passes.emplace_back(pass);
        });
    }

    bool set_backend(std::string_view backend) noexcept {
        if (backend != "interp") {
            std::println(std::cerr, "Unsupported backend: {}", backend);
            return false;
        }
        _options.backend = std::string(backend);
        return true;
    }

    void set_show_core_expansion(bool v) noexcept {
        _options.show_core_expansion = v;
    }

    void set_max_expansion_depth(std::uint32_t v) noexcept {
        _options.max_expansion_depth = v;
    }

    [[nodiscard]] int run() noexcept;
};

} // namespace lpc
