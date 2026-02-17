export module lpc.sema.mutability;

import std;

import lpc.context;
import lpc.passes;
import lpc.sema.core_form;

namespace lpc::sema {

export class MutabilityPass final : public Pass<CoreExprRef, CoreExprRef> {
private:
    std::set<CoreVar> _mutated;

public:
    [[nodiscard]] std::string name() const noexcept final {
        return "mutability_analysis";
    }

    [[nodiscard]] CoreExprRef run(
        CoreExprRef root, CompilerContext& ctx) noexcept final;

    [[nodiscard]] std::string dump(
        const CoreExprRef& result, CompilerContext& ctx) const noexcept final;

    void visit(CoreExprRef ref, CompilerContext& ctx);

    [[nodiscard]] bool is_failed() const noexcept final {
        return false;
    }
};

} // namespace lpc::sema
