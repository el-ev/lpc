module lpc.cps.closure_convert;

import std;

import lpc.context;
import lpc.cps.ir;
import lpc.passes;
import lpc.sema.core_form;
import lpc.syntax.arenas;
import lpc.syntax.ast;
import lpc.syntax.refs;
import lpc.utils.error_handler;
import lpc.utils.tagged_union;

namespace lpc::cps {

using namespace lpc::sema;
using lpc::utils::Assert;

namespace {
    class ClosureConverter {
    public:
        explicit ClosureConverter(CompilerContext& ctx) noexcept
            : _ctx(ctx)
            , _arena(ctx.cps_arena()) {
        }

        [[nodiscard]] CpsExprRef run(CpsExprRef root) {
            _next_var_id = find_max_var_id() + 1;
            static_cast<void>(analyze(root));
            CpsExprRef body = rewrite(root);
            if (_lambdas.empty())
                return body;
            return _arena.emplace(CpsFix { .functions = std::move(_lambdas),
                .body = body });
        }

    private:
        CompilerContext& _ctx;
        CpsArena& _arena;
        std::uint32_t _next_var_id = 0;

        std::map<CpsExprRef, std::vector<CpsVar>> _captures;
        std::vector<CpsExprRef> _lambdas;

        [[nodiscard]] CpsVar fresh_var(std::string_view debug_name) {
            auto id = _next_var_id++;
            return CpsVar(VarId(id, std::format("{}.{}", debug_name, id)));
        }

        [[nodiscard]] syntax::SpanRef make_int(std::int64_t value) {
            return _ctx.span_arena().from_loc(syntax::LocRef::invalid(),
                static_cast<syntax::LispNumber>(value));
        }

        [[nodiscard]] CpsAtom int_atom(std::int64_t value) {
            return CpsAtom(CpsConstant { make_int(value) });
        }

        static void collect_vars_from_atom(
            const CpsAtom& atom, std::unordered_set<CpsVar>& out) {
            if (const auto* var = atom.get<CpsVar>())
                out.insert(*var);
        }

        [[nodiscard]] std::uint32_t find_max_var_id() const {
            std::uint32_t max_id = 0;
            auto scan_atom = [&](const CpsAtom& atom) {
                if (const auto* var = atom.get<CpsVar>())
                    max_id = std::max(max_id, var->var.id);
            };
            for (const auto& expr : _arena) {
                expr.visit(utils::overloaded {
                    [&](const CpsApp& app) {
                        scan_atom(app.func);
                        for (const auto& arg : app.args)
                            scan_atom(arg);
                    },
                    [&](const CpsLet& let) {
                        max_id = std::max(max_id, let.target.var.id);
                        for (const auto& arg : let.args)
                            scan_atom(arg);
                    },
                    [&](const CpsIf& iff) { scan_atom(iff.condition); },
                    [&](const CpsFix&) { },
                    [&](const CpsHalt& halt) { scan_atom(halt.value); },
                    [&](const CpsLambda& lambda) {
                        max_id = std::max(max_id, lambda.name.var.id);
                        for (const auto& param : lambda.params)
                            max_id = std::max(max_id, param.var.id);
                    } });
            }
            return max_id;
        }

        [[nodiscard]] std::unordered_set<CpsVar> analyze(CpsExprRef ref) {
            const auto& expr = _arena.get(ref);

            if (const auto* app = expr.get<CpsApp>()) {
                std::unordered_set<CpsVar> free_vars;
                collect_vars_from_atom(app->func, free_vars);
                for (const auto& arg : app->args)
                    collect_vars_from_atom(arg, free_vars);
                return free_vars;
            }

            if (const auto* let = expr.get<CpsLet>()) {
                auto free_vars = analyze(let->body);
                free_vars.erase(let->target);
                for (const auto& arg : let->args)
                    collect_vars_from_atom(arg, free_vars);
                return free_vars;
            }

            if (const auto* iff = expr.get<CpsIf>()) {
                auto free_vars = analyze(iff->then_branch);
                auto else_free_vars = analyze(iff->else_branch);
                free_vars.insert(else_free_vars.begin(), else_free_vars.end());
                collect_vars_from_atom(iff->condition, free_vars);
                return free_vars;
            }

            if (const auto* lambda = expr.get<CpsLambda>()) {
                auto free_vars = analyze(lambda->body);
                for (const auto& param : lambda->params)
                    free_vars.erase(param);

                std::vector<CpsVar> captures;
                captures.reserve(free_vars.size());
                for (const auto& var : free_vars)
                    captures.push_back(var);
                std::ranges::sort(captures);
                _captures.emplace(ref, std::move(captures));
                return free_vars;
            }

            if (const auto* fix = expr.get<CpsFix>()) {
                std::unordered_set<CpsVar> free_vars;
                std::unordered_set<CpsVar> names;
                for (const auto& function_ref : fix->functions) {
                    auto function_free_vars = analyze(function_ref);
                    free_vars.insert(function_free_vars.begin(),
                        function_free_vars.end());
                    const auto* lambda
                        = _arena.get(function_ref).get<CpsLambda>();
                    Assert(lambda != nullptr);
                    names.insert(lambda->name);
                }
                auto body_free_vars = analyze(fix->body);
                free_vars.insert(body_free_vars.begin(), body_free_vars.end());
                for (const auto& name : names)
                    free_vars.erase(name);
                return free_vars;
            }

            if (const auto* halt = expr.get<CpsHalt>()) {
                std::unordered_set<CpsVar> free_vars;
                collect_vars_from_atom(halt->value, free_vars);
                return free_vars;
            }

            return { };
        }

        void rewrite_lambda(CpsExprRef function_ref) {
            const auto* lambda = _arena.get(function_ref).get<CpsLambda>();
            Assert(lambda != nullptr);
            const CpsExprRef body = lambda->body;
            std::vector<CpsVar> params = lambda->params;

            CpsExprRef new_body = rewrite(body);

            const CpsVar env = fresh_var("env");
            const auto& captures = _captures.at(function_ref);
            for (auto i = captures.size(); i > 0; --i) {
                new_body = _arena.emplace(CpsLet { .target = captures[i - 1],
                    .op = PrimOp::Load,
                    .args = { CpsAtom(env), int_atom(static_cast<std::int64_t>(i)) },
                    .body = new_body });
            }

            params.insert(params.begin(), env);
            auto* mutable_lambda = _arena.get(function_ref).get<CpsLambda>();
            mutable_lambda->params = std::move(params);
            mutable_lambda->body = new_body;
            _lambdas.push_back(function_ref);
        }

        [[nodiscard]] CpsExprRef rewrite_fix(
            std::span<const CpsExprRef> functions, CpsExprRef body) {
            struct LetStep {
                CpsVar target;
                PrimOp op;
                std::vector<CpsAtom> args;
            };
            std::vector<LetStep> steps;

            for (const auto& function_ref : functions) {
                const auto* lambda = _arena.get(function_ref).get<CpsLambda>();
                Assert(lambda != nullptr);
                const auto& captures = _captures.at(function_ref);

                std::vector<CpsAtom> alloc_args;
                alloc_args.reserve(captures.size() + 3);
                alloc_args.push_back(int_atom(CLOSURE_TAG));
                alloc_args.push_back(
                    int_atom(static_cast<std::int64_t>(captures.size() + 1)));
                for (std::size_t i = 0; i <= captures.size(); ++i)
                    alloc_args.emplace_back(CpsUnit());
                steps.push_back({ lambda->name, PrimOp::Alloc,
                    std::move(alloc_args) });
            }

            for (const auto& function_ref : functions) {
                const auto* lambda = _arena.get(function_ref).get<CpsLambda>();
                Assert(lambda != nullptr);
                const auto& captures = _captures.at(function_ref);

                steps.push_back({ fresh_var("_"), PrimOp::Store,
                    { CpsAtom(lambda->name), int_atom(0),
                        CpsAtom(CpsLabel { function_ref }) } });
                for (std::size_t i = 0; i < captures.size(); ++i) {
                    steps.push_back({ fresh_var("_"), PrimOp::Store,
                        { CpsAtom(lambda->name),
                            int_atom(static_cast<std::int64_t>(i + 1)),
                            CpsAtom(captures[i]) } });
                }
            }

            CpsExprRef result = body;
            for (auto i = steps.size(); i > 0; --i) {
                result = _arena.emplace(CpsLet { .target = steps[i - 1].target,
                    .op = steps[i - 1].op,
                    .args = std::move(steps[i - 1].args),
                    .body = result });
            }
            return result;
        }

        [[nodiscard]] CpsExprRef rewrite(CpsExprRef ref) {
            const auto& expr = _arena.get(ref);

            if (const auto* app = expr.get<CpsApp>()) {
                const auto* func_var = app->func.get<CpsVar>();
                if (func_var == nullptr)
                    return ref;

                const CpsAtom func_atom = app->func;
                std::vector<CpsAtom> args;
                args.reserve(app->args.size() + 1);
                args.push_back(func_atom);
                args.insert(args.end(), app->args.begin(), app->args.end());

                const CpsVar code = fresh_var("code");
                auto new_app = _arena.emplace(
                    CpsApp { CpsAtom(code), std::move(args) });
                return _arena.emplace(CpsLet { .target = code,
                    .op = PrimOp::Load,
                    .args = { func_atom, int_atom(0) },
                    .body = new_app });
            }

            if (const auto* let = expr.get<CpsLet>()) {
                const CpsExprRef body = let->body;
                auto new_body = rewrite(body);
                _arena.get(ref).get<CpsLet>()->body = new_body;
                return ref;
            }

            if (const auto* iff = expr.get<CpsIf>()) {
                const CpsExprRef then_branch = iff->then_branch;
                const CpsExprRef else_branch = iff->else_branch;
                auto new_then = rewrite(then_branch);
                auto new_else = rewrite(else_branch);
                auto* mutable_iff = _arena.get(ref).get<CpsIf>();
                mutable_iff->then_branch = new_then;
                mutable_iff->else_branch = new_else;
                return ref;
            }

            if (const auto* fix = expr.get<CpsFix>()) {
                std::vector<CpsExprRef> functions = fix->functions;
                const CpsExprRef body = fix->body;
                for (const auto& function_ref : functions)
                    rewrite_lambda(function_ref);
                auto new_body = rewrite(body);
                if (functions.empty())
                    return new_body;
                return rewrite_fix(functions, new_body);
            }

            // CpsHalt: nothing to do. Bare CpsLambda nodes never appear
            // outside CpsFix in LowerPass output.
            return ref;
        }
    };

} // namespace

[[nodiscard]] CpsExprRef ClosureConvertPass::run(
    CpsExprRef expr, CompilerContext& ctx) {
    ClosureConverter converter(ctx);
    return converter.run(expr);
}

[[nodiscard]] std::string ClosureConvertPass::dump(
    const CpsExprRef& expr, CompilerContext& ctx) const {
    CpsDumpVisitor visitor {
        .arena = ctx.cps_arena(), .span_arena = ctx.span_arena(), .indent = "  "
    };
    return std::format("{}\n", visitor.dump(expr, ""));
}

} // namespace lpc::cps
