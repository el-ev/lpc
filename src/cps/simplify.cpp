module lpc.cps.simplify;

import std;

import lpc.context;
import lpc.cps.ir;
import lpc.passes;
import lpc.utils.logging;
import lpc.utils.tagged_union;

namespace lpc::cps {

namespace {

    class Flattener {
    private:
        CpsArena& _arena;
        bool _changed = false;

    public:
        [[nodiscard]] explicit Flattener(CpsArena& arena) noexcept
            : _arena(arena) {
        }

        void operator()(CpsApp&) const {
        }
        void operator()(CpsHalt&) const {
        }

        void operator()(CpsLet& l) {
            run_impl(l.body);
        }

        void operator()(CpsIf& i) {
            run_impl(i.then_branch);
            run_impl(i.else_branch);
        }

        void operator()(CpsLambda& l) {
            run_impl(l.body);
        }

        void operator()(CpsFix& f) {
            for (auto& func : f.functions)
                run_impl(func);
            run_impl(f.body);

            int iterations = 0;
            while (true) {
                auto& body_expr = _arena.get(f.body);
                if (auto* nested = body_expr.get<CpsFix>()) {
                    f.functions.append_range(nested->functions);
                    f.body = nested->body;
                    _changed = true;
                    if (++iterations > 1000) {
                        break;
                    }
                } else {
                    break;
                }
            }
        }

        void run_impl(CpsExprRef expr) {
            _arena.get(expr).visit(*this);
        }

        [[nodiscard]] bool run(CpsExprRef expr) {
            _changed = false;
            run_impl(expr);
            return _changed;
        }
    };

    class EtaReducer final {
    private:
        bool _changed = false;
        CpsArena& _arena;
        std::unordered_map<CpsVar, CpsAtom> _replacements;

    public:
        [[nodiscard]] explicit EtaReducer(CpsArena& arena) noexcept
            : _arena(arena) {
        }

        [[nodiscard]] CpsAtom resolve(const CpsAtom& atom) const {
            if (const auto* v = atom.get<CpsVar>())
                if (auto it = _replacements.find(*v); it != _replacements.end())
                    return resolve(it->second);
            return atom;
        }

        void resolve_in_place(CpsAtom& atom) const {
            atom = resolve(atom);
        }

        void resolve_list(std::vector<CpsAtom>& atoms) const {
            for (auto& atom : atoms)
                resolve_in_place(atom);
        }

        [[nodiscard]] std::optional<CpsAtom> try_reduce(
            const CpsLambda& lam) const {
            if (!lam.body.is_valid())
                return std::nullopt;
            const auto& body_expr = _arena.get(lam.body);
            if (const auto* app = body_expr.get<CpsApp>()) {
                if (app->args.size() != lam.params.size())
                    return std::nullopt;
                for (std::size_t i = 0; i < lam.params.size(); ++i) {
                    const auto* arg_var = app->args[i].get<CpsVar>();
                    if (arg_var == nullptr || arg_var->var != lam.params[i].var)
                        return std::nullopt;
                }
                if (const auto* func_var = app->func.get<CpsVar>())
                    for (const auto& param : lam.params)
                        if (param.var == func_var->var)
                            return std::nullopt;
                return app->func;
            }
            return std::nullopt;
        }

        void operator()(CpsApp& a) const {
            resolve_in_place(a.func);
            resolve_list(a.args);
        }
        void operator()(CpsHalt& h) const {
            resolve_in_place(h.value);
        }
        void operator()(CpsLet& l) {
            resolve_list(l.args);
            run_impl(l.body);
        }
        void operator()(CpsIf& i) {
            resolve_in_place(i.condition);
            run_impl(i.then_branch);
            run_impl(i.else_branch);
        }
        void operator()(CpsLambda& l) {
            run_impl(l.body);
        }
        void operator()(CpsFix& f) {
            std::unordered_map<CpsVar, CpsAtom> local;
            for (const auto& func_ref : f.functions) {
                const auto& lam = _arena.get(func_ref).get<CpsLambda>();
                if (auto target = try_reduce(*lam)) {
                    local.emplace(lam->name, resolve(*target));
                }
            }

            std::vector<CpsExprRef> kept;
            std::vector<std::pair<CpsVar, CpsAtom>> new_replacements;

            for (const auto& func_ref : f.functions) {
                const auto& lam = _arena.get(func_ref).get<CpsLambda>();
                auto vid = lam->name;

                if (auto it = local.find(vid); it != local.end()) {
                    CpsAtom curr = it->second;
                    std::unordered_set<CpsVar> visited;
                    visited.insert(vid);

                    bool cycle = false;
                    while (const auto* v = curr.get<CpsVar>()) {
                        if (visited.contains(*v)) {
                            cycle = true;
                            break;
                        }
                        visited.insert(*v);

                        if (auto next_it = local.find(*v);
                            next_it != local.end()) {
                            curr = next_it->second;
                        } else {
                            break;
                        }
                    }

                    if (!cycle) {
                        new_replacements.emplace_back(vid, curr);
                        _changed = true;
                        continue;
                    }
                }
                kept.push_back(func_ref);
            }

            f.functions = std::move(kept);
            for (const auto& [v, atom] : new_replacements) {
                _replacements[v] = atom;
            }

            for (auto& func : f.functions)
                run_impl(func);
            run_impl(f.body);
        }

        void run_impl(CpsExprRef expr) {
            _arena.get(expr).visit(*this);
        }

        [[nodiscard]] bool run(CpsExprRef expr) {
            _changed = false;
            run_impl(expr);
            return _changed;
        }
    };

    class EmptyFixRemover {
    private:
        bool _changed = false;
        CpsArena& _arena;

    public:
        [[nodiscard]] explicit EmptyFixRemover(CpsArena& arena) noexcept
            : _arena(arena) {
        }

        void run_impl(CpsExprRef ref) {
            auto* expr = &_arena.get(ref);
            if (auto* f = expr->get<CpsFix>()) {
                if (f->functions.empty()) {
                    auto next_ref = f->body;
                    auto cloned = _arena.get(next_ref).visit(
                        [](const auto& val) { return CpsExpr(val); });
                    *expr = std::move(cloned);
                    _changed = true;
                    run_impl(ref);
                    return;
                }
            }
            expr->visit(*this);
        }

        [[nodiscard]] bool run(CpsExprRef expr) {
            _changed = false;
            run_impl(expr);
            return _changed;
        }

        void operator()(CpsApp&) const {
        }
        void operator()(CpsHalt&) const {
        }

        void operator()(CpsLet& l) {
            run_impl(l.body);
        }

        void operator()(CpsIf& i) {
            run_impl(i.then_branch);
            run_impl(i.else_branch);
        }

        void operator()(CpsLambda& l) {
            run_impl(l.body);
        }

        void operator()(CpsFix& f) {
            for (auto& func : f.functions)
                run_impl(func);
            run_impl(f.body);
        }
    };

} // namespace

[[nodiscard]] CpsExprRef SimplifyPass::run(
    CpsExprRef expr, CompilerContext& ctx) noexcept {
    Flattener flattener { ctx.cps_arena() };
    EtaReducer eta_reducer { ctx.cps_arena() };
    EmptyFixRemover empty_fix_remover { ctx.cps_arena() };

    bool changed = false;
    do {
        changed = false;
        changed |= flattener.run(expr);
        changed |= eta_reducer.run(expr);
        changed |= empty_fix_remover.run(expr);
    } while (changed);
    return expr;
}

[[nodiscard]] std::string SimplifyPass::dump(
    const CpsExprRef& expr, CompilerContext& ctx) const noexcept {
    CpsDumpVisitor visitor {
        .arena = ctx.cps_arena(), .span_arena = ctx.span_arena(), .indent = "  "
    };
    return visitor.dump(expr, "");
}
} // namespace lpc::cps
