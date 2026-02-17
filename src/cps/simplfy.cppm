export module lpc.cps.simplify;

import std;
import lpc.context;
import lpc.cps.ir;
import lpc.passes;

namespace lpc::cps {

export class SimplifyPass final : public Pass<CpsExprRef, CpsExprRef> {
public:
    [[nodiscard]] std::string name() const noexcept final {
        return "simplify";
    }

    [[nodiscard]] CpsExprRef run(
        CpsExprRef expr, CompilerContext& ctx) noexcept final;

    [[nodiscard]] std::string dump(
        const CpsExprRef& expr, CompilerContext& ctx) const noexcept final;

    [[nodiscard]] bool is_failed() const noexcept final {
        return false;
    }

    explicit SimplifyPass() noexcept = default;
};

} // namespace lpc::cps
