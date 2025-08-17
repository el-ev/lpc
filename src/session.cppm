export module lpc.session;

import std;

namespace lpc {

export class Session {
private:
    std::string_view _output_file_path;
    std::vector<std::string_view> _input_file_paths;
    std::vector<std::string> _print_passes;
    std::string _backend;

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
        auto split_view = std::views::split(passes_str, ',')
            | std::views::transform([](auto&& subrange) {
                  return std::string_view(subrange.begin(), subrange.end());
              });

        std::ranges::for_each(split_view, [this](std::string_view pass) {
            _print_passes.emplace_back(pass);
        });
    }

    void set_backend(std::string_view backend) {
        if (backend != "interp")
            throw std::invalid_argument(
                "Unsupported backend: " + std::string(backend));
        _backend = std::string(backend);
    }

    [[nodiscard]] int run() noexcept;
};

} // namespace lpc
