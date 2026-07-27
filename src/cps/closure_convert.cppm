export module lpc.cps.closure_convert;

import std;

import lpc.context;
import lpc.cps.ir;
import lpc.passes;

namespace lpc::cps {

export class ClosureConvertPass final : public Pass<CpsExprRef, CpsExprRef> {
public:
    [[nodiscard]] std::string name() const noexcept final {
        return "closure-convert";
    }

    [[nodiscard]] CpsExprRef run(
        CpsExprRef expr, CompilerContext& ctx) final;

    [[nodiscard]] std::string dump(
        const CpsExprRef& expr, CompilerContext& ctx) const final;

    [[nodiscard]] bool is_failed() const noexcept final {
        return false;
    }

    explicit ClosureConvertPass() noexcept = default;
};

} // namespace lpc::cps
