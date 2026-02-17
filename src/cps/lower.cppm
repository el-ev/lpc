export module lpc.cps.lower;

import std;

import lpc.context;
import lpc.passes;
import lpc.sema.core_form;
import lpc.cps.ir;

namespace lpc::cps {

using namespace lpc::sema;

export class LowerPass final : public Pass<CoreExprRef, CpsExprRef> {
private:
    bool _failed = false;

public:
    [[nodiscard]] std::string name() const noexcept final {
        return "lower";
    }

    [[nodiscard]] CpsExprRef run(
        CoreExprRef root, CompilerContext& ctx) noexcept final;

    [[nodiscard]] std::string dump(
        const CpsExprRef& expr, CompilerContext& ctx) const noexcept final;

    [[nodiscard]] bool is_failed() const noexcept final {
        return _failed;
    }

    explicit LowerPass() noexcept = default;
};

} // namespace lpc::cps
