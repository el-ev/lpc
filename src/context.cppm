export module lpc.context;

import std;

import lpc.sema.core_form;
import lpc.cps.ir;
import lpc.syntax.arenas;

namespace lpc {

export struct CompilerOptions {
    bool show_core_expansion = false;
    std::uint32_t max_expansion_depth = 1000;
    std::vector<std::string> print_passes;
    std::string stop_after;
    std::string backend = "interp";

    [[nodiscard]] bool should_print(std::string_view pass) const noexcept {
        if (std::ranges::find(print_passes, "all") != print_passes.end())
            return true;
        return std::ranges::find(print_passes, pass) != print_passes.end();
    }

    [[nodiscard]] bool should_stop(std::string_view pass) const noexcept {
        return stop_after == pass;
    }
};

export class CompilerContext {
private:
    CompilerOptions _options;
    std::string _path;
    std::string _source;
    syntax::SpanArena _span_arena;
    sema::CoreExprArena _core_arena;
    cps::CpsArena _cps_arena;

public:
    explicit CompilerContext(CompilerOptions&& options, std::string&& path,
        std::string&& source, syntax::SpanArena&& span_arena)
        : _options(std::move(options))
        , _path(std::move(path))
        , _source(std::move(source))
        , _span_arena(std::move(span_arena)) {
    }

    [[nodiscard]] const CompilerOptions& options() const noexcept {
        return _options;
    }

    [[nodiscard]] std::string_view path() const noexcept {
        return _path;
    }

    [[nodiscard]] std::string_view source() const noexcept {
        return _source;
    }

    [[nodiscard]] syntax::SpanArena& span_arena() noexcept {
        return _span_arena;
    }

    [[nodiscard]] sema::CoreExprArena& core_arena() noexcept {
        return _core_arena;
    }

    [[nodiscard]] cps::CpsArena& cps_arena() noexcept {
        return _cps_arena;
    }
};

} // namespace lpc
