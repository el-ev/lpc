module lpc.cps.lower;

import std;

import lpc.context;
import lpc.cps.ir;
import lpc.sema.core_form;
import lpc.syntax.arenas;
import lpc.syntax.ast;
import lpc.syntax.refs;
import lpc.syntax.token;
import lpc.utils.logging;
import lpc.utils.tagged_union;

namespace lpc::cps {

using namespace lpc::sema;
using lpc::utils::overloaded;

using Continuation = const std::function<CpsExprRef(CpsAtom)>&;

class CpsConverter {
public:
    explicit CpsConverter(CompilerContext& ctx)
        : _ctx(ctx)
        , _arena(ctx.cps_arena())
        , _core_arena(ctx.core_arena()) {
    }

    template <typename T>
    [[nodiscard]] CpsExprRef convert(const T&, Continuation);

    [[nodiscard]] CpsExprRef lower_program(CoreExprRef root);

    [[nodiscard]] CpsExprRef convert(CoreExprRef ref, Continuation k);

private:
    [[nodiscard]] CpsExprRef convert_seq(
        std::span<const CoreExprRef> seq, Continuation k);

    [[nodiscard]] CpsExprRef convert_args(std::span<const CoreExprRef> args,
        std::vector<CpsAtom> acc,
        const std::function<CpsExprRef(std::vector<CpsAtom>)>& k);

    [[nodiscard]] std::optional<CpsAtom> try_atom(CoreExprRef ref) const {
        const auto& expr = _core_arena[ref];
        if (expr.isa<CoreConstant>())
            return CpsAtom(CpsConstant { expr.get<CoreConstant>()->value });
        if (expr.isa<CoreVar>()) {
            const auto* var = expr.get<CoreVar>();
            if (var->kind == CoreVarKind::Builtin)
                return lookup(*var);
        }
        return std::nullopt;
    }

public:
    CpsVar next_var(std::string_view debug_name = "") {
        auto id = _next_var_id++;
        if (debug_name.empty()) {
            return CpsVar(VarId(id, std::format("v.{}", id)));
        }
        auto& count = _name_counts[std::string(debug_name)];
        if (count++ == 0) {
            return CpsVar(VarId(id, std::string(debug_name)));
        }
        return CpsVar(VarId(id, std::format("{}.{}", debug_name, count - 1)));
    }

private:
    CompilerContext& _ctx;
    CpsArena& _arena;
    CoreExprArena& _core_arena;
    std::unordered_map<CoreVar, CpsAtom> mapping;
    std::unordered_map<CoreVar, PrimOp> _prim_mapping;
    std::unordered_map<std::string, std::uint32_t> _name_counts;
    std::uint32_t _next_var_id = 0;
    std::optional<CpsVar> _forced_lambda_var;

    [[nodiscard]] CpsAtom lookup(const CoreVar& source_id) const {
        return mapping.at(source_id);
    }

    [[nodiscard]] CpsExprRef try_builtin(
        const std::string& name, std::vector<CpsAtom> args, Continuation k) {
#ifndef PRIM
#define PRIM(str, min, max, op)                                                \
    if (name == (str))                                                         \
        return emit_primop(PrimOp::op, std::move(args), k);
#endif
#ifndef BUILTIN
#define BUILTIN(str, min, max)
#endif
#include "../sema/builtins.def"
#undef BUILTIN
#undef PRIM

        if (name == "__alloc")
            return emit_alloc(PrimOp::Alloc, -1, std::move(args), k);

        if (name == "make-vector") {
            // Unimplemented for now
            return k(CpsAtom(CpsUnit()));
        }

        if (name == "__void")
            return k(CpsAtom(CpsUnit()));

        if (name == "__cons")
            return emit_alloc(
                PrimOp::Alloc, 0 /* pair tag */, std::move(args), k);
        if (name == "__car")
            return emit_primop(PrimOp::Load,
                { args[0], CpsAtom(CpsConstant { make_int(0) }) }, k);
        if (name == "__cdr")
            return emit_primop(PrimOp::Load,
                { args[0], CpsAtom(CpsConstant { make_int(1) }) }, k);
        if (name == "__vector-ref")
            return emit_primop(PrimOp::Load, std::move(args), k);
        if (name == "__vector-set!")
            return emit_primop(PrimOp::Store, std::move(args), k);
        if (name == "__eq?")
            return emit_primop(PrimOp::Eq, std::move(args), k);
        if (name == "__pair?")
            return emit_primop(PrimOp::IsPair, std::move(args), k);
        if (name == "__symbol?")
            return emit_primop(PrimOp::IsSymbol, std::move(args), k);
        if (name == "__vector?")
            return emit_primop(PrimOp::IsVector, std::move(args), k);

        return k(CpsAtom(CpsUnit()));
    }

    [[nodiscard]] CpsExprRef emit_primop(
        PrimOp op, std::vector<CpsAtom> args, Continuation k) {
        auto rv = next_var("prim_res");
        return _arena.emplace(CpsLet { .target = rv,
            .op = op,
            .args = std::move(args),
            .body = k(CpsAtom(rv)) });
    }

    [[nodiscard]] CpsExprRef emit_alloc(
        PrimOp op, int tag, std::vector<CpsAtom> args, Continuation k) {
        if (tag != -1)
            args.insert(args.begin(), CpsAtom(CpsConstant { make_int(tag) }));
        args.insert(args.begin() + 1,
            CpsAtom(CpsConstant {
                make_int(static_cast<syntax::LispNumber>(args.size() - 1)) }));

        auto rv = next_var("alloc_res");
        return _arena.emplace(CpsLet { .target = rv,
            .op = op,
            .args = std::move(args),
            .body = k(CpsAtom(rv)) });
    }

    syntax::SpanRef make_int(syntax::LispNumber value) {
        // FIXME Location
        return _ctx.span_arena().from_loc(syntax::LocRef::invalid(), value);
    }

    void extend(const CoreVar& source_id, CpsAtom&& cps_val) {
        mapping[source_id] = std::move(cps_val);
    }
};

template <>
CpsExprRef CpsConverter::convert<CoreConstant>(
    const CoreConstant& c, Continuation k) {
    return k(CpsAtom(CpsConstant { c.value }));
}

template <>
CpsExprRef CpsConverter::convert<CoreSeq>(const CoreSeq& seq, Continuation k) {
    return convert_seq(seq.exprs, k);
}

template <>
CpsExprRef CpsConverter::convert<CoreIf>(const CoreIf& c, Continuation k) {
    auto kv = next_var("k_join");
    auto rv = next_var("res");
    auto k_lambda = _arena.emplace(CpsLambda(kv, { rv }, k(CpsAtom(rv))));

    auto join_k = [this, kv](const CpsAtom& res) {
        return _arena.emplace(CpsApp(CpsAtom(kv), { res }));
    };

    auto iff = convert(c.condition, [&](CpsAtom condition) {
        auto then_branch = convert(c.then_branch, join_k);
        CpsExprRef else_branch;
        if (c.else_branch.is_valid()) {
            else_branch = convert(c.else_branch, join_k);
        } else {
            else_branch = join_k(CpsAtom(CpsUnit()));
        }
        return _arena.emplace(CpsIf { .condition = std::move(condition),
            .then_branch = then_branch,
            .else_branch = else_branch });
    });

    return _arena.emplace(CpsFix({ k_lambda }, iff));
}

template <>
CpsExprRef CpsConverter::convert<CoreLambda>(
    const CoreLambda& c, Continuation k) {
    auto forced_name = std::exchange(_forced_lambda_var, std::nullopt);

    std::vector<CpsVar> cps_params;
    std::vector<std::tuple<VarId, CpsVar, CpsVar>> boxed;

    auto scope = [&](const CoreVar& var, const CpsVar& p) {
        if (_ctx.core_arena().is_mutated(var)) {
            auto box = next_var(var.id.debug_name + "_box");
            extend(var, CpsAtom(box));
            boxed.emplace_back(var.id, p, box);
        } else {
            extend(var, CpsAtom(p));
        }
    };

    for (const auto& param : c.params) {
        auto p = next_var(param.id.debug_name);
        cps_params.push_back(p);
        scope(param, p);
    }

    if (c.rest_param) {
        auto p = next_var(c.rest_param->id.debug_name);
        cps_params.push_back(p);
        scope(*c.rest_param, p);
    }

    CpsVar k_dyn = next_var("k");
    cps_params.push_back(k_dyn);

    auto body_cont = [&](const CpsAtom& result) {
        return _arena.emplace(CpsApp(CpsAtom(k_dyn), { result }));
    };

    auto body = convert(c.body, body_cont);

    for (auto i = boxed.size(); i > 0; --i) {
        const auto& [id, p, box] = boxed[i - 1];
        body = _arena.emplace(CpsLet { .target = box,
            .op = PrimOp::Box,
            .args = { CpsAtom(p) },
            .body = body });
    }

    auto lambda_var = forced_name ? *forced_name : next_var("lam");

    auto lambda = _arena.emplace(CpsLambda { .name = lambda_var,
        .params = std::move(cps_params),
        .body = body,
        .is_variadic = c.rest_param.has_value() });
    auto rest = k(CpsAtom(lambda_var));

    return _arena.emplace(CpsFix({ lambda }, rest));
}

template <>
CpsExprRef CpsConverter::convert<CoreVar>(const CoreVar& c, Continuation k) {
    auto val = lookup(c);
    // builtins are const by default
    if (!_ctx.core_arena().is_mutated(c))
        return k(val);

    CpsVar unboxed = next_var(c.id.debug_name + "_val");
    return _arena.emplace(CpsLet { .target = unboxed,
        .op = PrimOp::BoxGet,
        .args = { val },
        .body = k(CpsAtom(unboxed)) });
}

template <>
CpsExprRef CpsConverter::convert<CoreDefine>(
    const CoreDefine& c, Continuation k) {
    if (_ctx.core_arena().is_mutated(c.target)) {
        auto box = next_var(c.target.id.debug_name + "_box");
        extend(c.target, CpsAtom(box));
        return convert(c.value, [&](const CpsAtom& val) {
            return _arena.emplace(CpsLet { .target = box,
                .op = PrimOp::Box,
                .args = { val },
                .body = k(CpsAtom(CpsUnit())) });
        });
    }

    if (_core_arena[c.value].isa<CoreLambda>()) {
        auto lambda_var = next_var(c.target.id.debug_name);
        extend(c.target, CpsAtom(lambda_var));

        auto old_forced = std::exchange(_forced_lambda_var, lambda_var);
        auto res = convert(c.value, [&](const CpsAtom& /* val */) {
            _forced_lambda_var = old_forced;
            return k(CpsAtom(CpsUnit()));
        });
        return res;
    }

    return convert(c.value, [&](const CpsAtom& val) {
        extend(c.target, CpsAtom(val));
        return k(CpsAtom(CpsUnit()));
    });
}

template <>
CpsExprRef CpsConverter::convert<CoreApply>(
    const CoreApply& c, Continuation k) {
    const auto& func_expr = _core_arena[c.func];
    if (const auto* var = func_expr.get<CoreVar>()) {
        if (var->kind == CoreVarKind::Builtin) {
            return convert_args(
                c.args, {}, [this, var, k](std::vector<CpsAtom> args) {
                    return try_builtin(var->id.debug_name, std::move(args), k);
                });
        }
    }

    return convert(c.func, [&](const CpsAtom& func) {
        return convert_args(c.args, {}, [&](std::vector<CpsAtom> args) {
            auto kv = next_var("k");
            auto rv = next_var("res");
            auto k_lambda
                = _arena.emplace(CpsLambda(kv, { rv }, k(CpsAtom(rv))));
            CpsAtom k_atom(kv);
            args.emplace_back(std::move(k_atom));
            auto app = _arena.emplace(CpsApp(func, std::move(args)));
            return _arena.emplace(CpsFix({ k_lambda }, app));
        });
    });
}

template <>
CpsExprRef CpsConverter::convert<CoreSet>(const CoreSet& c, Continuation k) {
    return convert(c.value, [&](const CpsAtom& val) {
        auto box = lookup(c.target);
        return _arena.emplace(CpsLet { .target = next_var("_"),
            .op = PrimOp::BoxSet,
            .args = { box, val },
            .body = k(CpsAtom(CpsUnit())) });
    });
}

CpsExprRef CpsConverter::convert(CoreExprRef ref, Continuation k) {
    return _ctx.core_arena().at(ref).visit(
        overloaded { [this, k](const auto& c) { return convert(c, k); } });
}

CpsExprRef CpsConverter::convert_seq(
    std::span<const CoreExprRef> seq, Continuation k) {
    if (seq.empty())
        return k(CpsAtom(CpsUnit()));
    if (seq.size() == 1)
        return convert(seq.front(), k);
    auto convert_rest = [this, seq, k](const CpsAtom& /* ignored */) {
        return convert_seq(seq.subspan(1), k);
    };
    return convert(seq.front(), convert_rest);
}

CpsExprRef CpsConverter::convert_args(std::span<const CoreExprRef> args,
    std::vector<CpsAtom> acc,
    const std::function<CpsExprRef(std::vector<CpsAtom>)>& k) {
    if (args.empty())
        return k(std::move(acc));
    if (auto atom = try_atom(args.front())) {
        acc.push_back(*atom);
        return convert_args(args.subspan(1), std::move(acc), k);
    }
    return convert(args.front(), [&](const CpsAtom& cps_head) {
        acc.push_back(cps_head);
        return convert_args(args.subspan(1), std::move(acc), k);
    });
}

CpsExprRef CpsConverter::lower_program(CoreExprRef root) {
    auto k = [this](CpsAtom result) -> CpsExprRef {
        return _arena.emplace(CpsHalt { std::move(result) });
    };
    return _ctx.core_arena().at(root).visit(
        overloaded { [this, k](const auto& c) { return convert(c, k); } });
}

CpsExprRef LowerPass::run(CoreExprRef root, CompilerContext& ctx) noexcept {
    CpsConverter lowerer(ctx);
    auto entry = lowerer.lower_program(root);
    if (entry == CpsExprRef::invalid())
        _failed = true;
    return entry;
}

std::string LowerPass::dump(
    const CpsExprRef& expr, CompilerContext& ctx) const noexcept {
    CpsDumpVisitor visitor {
        .arena = ctx.cps_arena(), .span_arena = ctx.span_arena(), .indent = "  "
    };
    return visitor.dump(expr, "");
}

} // namespace lpc::cps
