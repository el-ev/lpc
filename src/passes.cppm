export module lpc.passes;

import std;

import lpc.context;
import lpc.syntax.arenas;
import lpc.syntax.ast;
import lpc.syntax.refs;
import lpc.utils.logging;

namespace lpc {

using lpc::utils::Debug;
using lpc::utils::Error;

export template <typename In, typename Out>
class Pass {
public:
    using in_type = In;
    using out_type = Out;

    [[nodiscard]] virtual std::string name() const noexcept = 0;
    [[nodiscard]] virtual out_type run(
        in_type input, CompilerContext& ctx) noexcept
        = 0;
    [[nodiscard]] virtual std::string dump(
        const out_type& result, CompilerContext& ctx) const noexcept
        = 0;
    [[nodiscard]] virtual bool is_failed() const noexcept = 0;
};

template <typename... T>
struct last_type {
    using type = typename decltype((std::type_identity<T> {}, ...))::type;
};

export template <typename In, typename... Passes>
class Pipeline {
public:
    [[nodiscard]] auto run(In input, CompilerContext& ctx) noexcept {
        return run_impl<Passes...>(std::move(input), ctx);
    }

private:
    template <typename... Rest>
    requires(sizeof...(Rest) == 0)
    auto run_impl(auto&& input, CompilerContext& /* ctx */) noexcept {
        return std::forward<decltype(input)>(input);
    }

    template <typename P, typename... Rest>
    auto run_impl(auto&& input, CompilerContext& ctx) noexcept {
        P pass;
        auto result = pass.run(std::forward<decltype(input)>(input), ctx);

        if (pass.is_failed()) {
            Error("Pass {} failed", pass.name());
            using FinalOut = typename last_type<In, Passes...>::type::out_type;
            if constexpr (std::is_same_v<FinalOut, syntax::SpanRef>) {
                return syntax::SpanRef::invalid();
            } else {
                return FinalOut {};
            }
        }

        Debug("Pass {} completed successfully", pass.name());
        if (ctx.options().should_print(pass.name()))
            std::print("{}", pass.dump(result, ctx));

        return run_impl<Rest...>(std::move(result), ctx);
    }
};

export template <typename In, typename... Passes>
class PipelineBuilder {
public:
    template <typename P>
    auto add() const noexcept {
        if constexpr (sizeof...(Passes) == 0) {
            static_assert(std::is_same_v<In, typename P::in_type>);
        } else {
            using PrevOut = typename last_type<Passes...>::type::out_type;
            static_assert(std::is_same_v<PrevOut, typename P::in_type>);
        }
        return PipelineBuilder<In, Passes..., P>();
    }

    auto build() const noexcept {
        return Pipeline<In, Passes...>();
    }
};

export template <typename In>
auto builder() noexcept {
    return PipelineBuilder<In>();
}

} // namespace lpc
