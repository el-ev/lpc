export module lpc.context;

import std;

import lpc.syntax.arenas;
import lpc.analysis.core_form;

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
    syntax::SpanArena _span_arena;
    analysis::CoreExprArena _core_arena;

public:
    explicit CompilerContext(
        CompilerOptions&& options, syntax::SpanArena&& span_arena)
        : _options(std::move(options))
        , _span_arena(std::move(span_arena)) {
    }

    [[nodiscard]] const CompilerOptions& options() const noexcept {
        return _options;
    }

    [[nodiscard]] syntax::SpanArena& span_arena() noexcept {
        return _span_arena;
    }

    [[nodiscard]] analysis::CoreExprArena& core_arena() noexcept {
        return _core_arena;
    }
};

} // namespace lpc
