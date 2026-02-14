export module lpc.context;

import std;

import lpc.frontend.arenas;

namespace lpc {

export struct CompilerOptions {
    bool show_core_expansion = false;
    std::uint32_t max_expansion_depth = 1000;
    std::vector<std::string> print_passes;
    std::string backend = "interp";

    [[nodiscard]] bool should_print(std::string_view pass) const noexcept {
        if (std::ranges::find(print_passes, "all") != print_passes.end())
            return true;
        return std::ranges::find(print_passes, pass) != print_passes.end();
    }
};

export class CompilerContext {
private:
    CompilerOptions _options;
    frontend::SpanArena _arena;

public:
    explicit CompilerContext(
        CompilerOptions&& options, frontend::SpanArena&& arena)
        : _options(std::move(options))
        , _arena(std::move(arena)) {
    }

    [[nodiscard]] const CompilerOptions& options() const noexcept {
        return _options;
    }

    [[nodiscard]] frontend::SpanArena& arena() noexcept {
        return _arena;
    }

    [[nodiscard]] const frontend::SpanArena& arena() const noexcept {
        return _arena;
    }
};

} // namespace lpc