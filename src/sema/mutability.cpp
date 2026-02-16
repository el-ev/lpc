module lpc.sema.mutability;

import std;
import lpc.sema.core_form;
import lpc.context;

namespace lpc::sema {

namespace {
    template <typename... Ts>
    struct Visitor : Ts... {
        using Ts::operator()...;
    };
} // namespace

CoreExprRef MutabilityPass::run(
    CoreExprRef root, CompilerContext& ctx) noexcept {
    visit(root, ctx);
    ctx.core_arena().set_mutated_vars(_mutated);
    return root;
}

std::string MutabilityPass::dump(
    const CoreExprRef& /* root */, CompilerContext& /* ctx */) const noexcept {
    std::string out = "Mutated:\n";
    for (const auto& var : _mutated) {
        out += std::format("  {}\n", var.id.debug_name);
    }
    return out;
}

void MutabilityPass::visit(CoreExprRef ref, CompilerContext& ctx) {
    if (!ref.is_valid())
        return;

    ctx.core_arena().at(ref).visit(Visitor {
        [&](const CoreSet& s) {
            _mutated.insert(s.target);
            visit(s.value, ctx);
        },
        [&](const CoreDefine& d) { visit(d.value, ctx); },
        [&](const CoreLambda& l) { visit(l.body, ctx); },
        [&](const CoreIf& i) {
            visit(i.condition, ctx);
            visit(i.then_branch, ctx);
            visit(i.else_branch, ctx);
        },
        [&](const CoreSeq& s) {
            for (const auto& e : s.exprs)
                visit(e, ctx);
        },
        [&](const CoreApply& a) {
            visit(a.func, ctx);
            for (const auto& arg : a.args)
                visit(arg, ctx);
        },
        [&](const CoreVar&) {},
        [&](const CoreConstant&) {},
    });
}

} // namespace lpc::sema
