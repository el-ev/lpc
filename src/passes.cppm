export module lpc.passes;

import std;

import lpc.syntax.ast;
import lpc.core.arenas;
import lpc.context;
import lpc.core.refs;
import lpc.utils.logging;

namespace lpc {

using namespace lpc::core;
using namespace lpc::syntax;

using lpc::utils::Debug;
using lpc::utils::Error;

export class Pass {
public:
    virtual ~Pass() = default;
    [[nodiscard]] virtual std::string name() const noexcept = 0;
    [[nodiscard]] virtual SpanRef run(
        SpanRef root, CompilerContext& ctx) noexcept
        = 0;
};

export class PassManager {
private:
    std::vector<std::unique_ptr<Pass>> _passes;

public:
    explicit PassManager() = default;

    PassManager(const PassManager&) = delete;
    PassManager& operator=(const PassManager&) = delete;

    PassManager(PassManager&&) = default;
    PassManager& operator=(PassManager&&) = default;

    template <typename T>
        requires std::is_base_of_v<Pass, T>
    PassManager& add() {
        _passes.emplace_back(std::make_unique<T>());
        return *this;
    }

    [[nodiscard]] SpanRef run_all(SpanRef root, CompilerContext& ctx) noexcept {
        SpanRef result = root;
        auto& arena = ctx.arena();

        for (const auto& pass : _passes) {
            Debug("Running pass: {}", pass->name());

            result = pass->run(result, ctx);
            if (!result.is_valid()) {
                Error("Pass failed: {}", pass->name());
                return result;
            }

            Debug("Pass {} completed successfully", pass->name());

            if (ctx.options().should_print(pass->name())) {
                std::print("{}", arena.dump_root(result));
            }
        }

        return result;
    }
};

} // namespace lpc
