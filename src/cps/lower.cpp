module lpc.cps.lower;

import std;

import lpc.context;
import lpc.cps.ir;
import lpc.sema.core_form;
import lpc.syntax.arenas;
import lpc.syntax.ast;
import lpc.syntax.refs;
import lpc.syntax.token;
import lpc.utils.error_handler;
import lpc.utils.logging;
import lpc.utils.tagged_union;

namespace lpc::cps {

using namespace lpc::sema;
using lpc::utils::overloaded;
using lpc::utils::Assert;

using Continuation = const std::function<CpsExprRef(CpsAtom)>&;

class CpsConverter {
public:
    explicit CpsConverter(CompilerContext& ctx)
        : _ctx(ctx)
        , _arena(ctx.cps_arena())
        , _core_arena(ctx.core_arena()) {
#define X(name, str) _prim_mapping[str] = PrimOp::name;
#include "primops.def"
#undef X
#define PRIM(str, min, max, op) _builtin_arity[str] = max;
#define BUILTIN(str, min, max) _builtin_arity[str] = max;
#include "../sema/builtins.def"
#undef PRIM
#undef BUILTIN
    }

    template <typename T>
    [[nodiscard]] CpsExprRef convert(const T&, Continuation);

    [[nodiscard]] CpsExprRef convert(CoreExprRef ref, Continuation k);
    [[nodiscard]] CpsExprRef lower_program(CoreExprRef root);

    CpsVar next_var(std::string_view debug_name = "") {
        auto id = _next_var_id++;
        if (debug_name.empty())
            return CpsVar(VarId(id, std::format("v.{}", id)));
        if (debug_name == "_")
            return CpsVar(VarId(id, "_"));
        auto& count = _name_counts[std::string(debug_name)];
        if (count++ == 0)
            return CpsVar(VarId(id, std::string(debug_name)));
        return CpsVar(VarId(id, std::format("{}.{}", debug_name, count - 1)));
    }

private:
    CompilerContext& _ctx;
    CpsArena& _arena;
    CoreExprArena& _core_arena;
    std::unordered_map<CoreVar, CpsAtom> _mapping;
    std::unordered_map<std::string, PrimOp> _prim_mapping;
    std::unordered_map<std::string, std::uint32_t> _builtin_arity;
    std::unordered_map<std::string, std::uint32_t> _name_counts;
    std::uint32_t _next_var_id = 0;
    std::optional<CpsVar> _forced_lambda_var;

    [[nodiscard]] CpsAtom lookup(const CoreVar& var) {
        if (auto it = _mapping.find(var); it != _mapping.end())
            return it->second;

        Assert(var.kind == CoreVarKind::Global
            || var.kind == CoreVarKind::Builtin);
        auto fresh = CpsAtom(next_var(var.id.debug_name));
        _mapping.emplace(var, fresh);
        return fresh;
    }

    void extend(const CoreVar& var, CpsAtom cps_val) {
        _mapping[var] = std::move(cps_val);
    }

    [[nodiscard]] std::optional<CpsAtom> try_atom(CoreExprRef ref) {
        const auto& expr = _core_arena[ref];
        if (expr.isa<CoreConstant>())
            return CpsAtom(CpsConstant { expr.get<CoreConstant>()->value });
        return std::nullopt;
    }

    [[nodiscard]] CpsExprRef eta_expand_builtin(
        const std::string& name, Continuation k);

    [[nodiscard]] CpsExprRef try_builtin(
        const std::string& name, std::vector<CpsAtom> args, Continuation k) {
        if (name == "__alloc")
            return emit_alloc(PrimOp::Alloc, -1, std::move(args), k);

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
        if (name == "__set-car!")
            return emit_primop(PrimOp::Store,
                { args[0], CpsAtom(CpsConstant { make_int(0) }), args[1] }, k);
        if (name == "__set-cdr!")
            return emit_primop(PrimOp::Store,
                { args[0], CpsAtom(CpsConstant { make_int(1) }), args[1] }, k);

        if (name == "__call/cc") {
            auto escape_var = next_var("escape");
            auto val_var = next_var("val");
            auto discard_k = next_var("_k");
            auto escape_body = k(CpsAtom(val_var));
            auto escape_lambda = _arena.emplace(
                CpsLambda(escape_var, { val_var, discard_k }, escape_body));

            auto kv = next_var("k");
            auto rv = next_var("res");
            auto k_lambda
                = _arena.emplace(CpsLambda(kv, { rv }, k(CpsAtom(rv))));

            auto app = _arena.emplace(
                CpsApp(args[0], { CpsAtom(escape_var), CpsAtom(kv) }));
            auto with_k = _arena.emplace(CpsFix({ k_lambda }, app));
            return _arena.emplace(CpsFix({ escape_lambda }, with_k));
        }

        if (name == "__apply") {
            auto kv = next_var("k");
            auto rv = next_var("res");
            auto k_lambda
                = _arena.emplace(CpsLambda(kv, { rv }, k(CpsAtom(rv))));

            auto apply = _arena.emplace(CpsLet { .target = next_var("_"),
                .op = PrimOp::Apply,
                .args = { args[0], args[1], CpsAtom(kv) },
                .body = _arena.emplace(CpsHalt { CpsAtom(CpsUnit()) }) });
            return _arena.emplace(CpsFix({ k_lambda }, apply));
        }

        if (auto it = _prim_mapping.find(name); it != _prim_mapping.end())
            return emit_primop(it->second, std::move(args), k);

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
        return _ctx.span_arena().from_loc(syntax::LocRef::invalid(), value);
    }

    [[nodiscard]] CpsExprRef convert_seq(
        std::span<const CoreExprRef> seq, Continuation k);

    void collect_global_defines(CoreExprRef ref, std::set<CoreVar>& out);

    [[nodiscard]] CpsExprRef convert_args(std::span<const CoreExprRef> args,
        std::vector<CpsAtom> acc,
        const std::function<CpsExprRef(std::vector<CpsAtom>)>& k);
};

template <>
CpsExprRef CpsConverter::convert<CoreConstant>(
    const CoreConstant& c, Continuation k) {
    return k(CpsAtom(CpsConstant { c.value }));
}

CpsExprRef CpsConverter::eta_expand_builtin(
    const std::string& name, Continuation k) {
    // A builtin referenced as a value becomes a closure that forwards its
    // arguments to the builtin: (lambda (a0 ... an k) (builtin a0 ... an k))
    const auto it = _builtin_arity.find(name);
    Assert(it != _builtin_arity.end());

    std::vector<CpsVar> params;
    std::vector<CpsAtom> args;
    params.reserve(it->second + 1);
    args.reserve(it->second);
    for (std::uint32_t index = 0; index < it->second; ++index) {
        auto param = next_var("a");
        params.push_back(param);
        args.emplace_back(param);
    }

    CpsVar k_dyn = next_var("k");
    params.push_back(k_dyn);

    auto body_cont = [&](const CpsAtom& result) {
        return _arena.emplace(CpsApp(CpsAtom(k_dyn), { result }));
    };
    auto body = try_builtin(name, std::move(args), body_cont);

    auto lambda_var = next_var(name);
    auto lambda = _arena.emplace(CpsLambda { .name = lambda_var,
        .params = std::move(params),
        .body = body });
    return _arena.emplace(CpsFix({ lambda }, k(CpsAtom(lambda_var))));
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
    std::vector<std::pair<CpsVar, CpsVar>> boxed;

    auto scope = [&](const CoreVar& var, const CpsVar& p) {
        if (_ctx.core_arena().is_mutated(var)) {
            auto box = next_var(var.id.debug_name + "_box");
            extend(var, CpsAtom(box));
            boxed.emplace_back(p, box);
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
        const auto& [p, box] = boxed[i - 1];
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
    if (c.kind == CoreVarKind::Builtin)
        return eta_expand_builtin(c.id.debug_name, k);

    auto val = lookup(c);
    if (c.kind != CoreVarKind::Global && !_ctx.core_arena().is_mutated(c))
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
    if (c.target.kind == CoreVarKind::Global) {
        auto box = lookup(c.target);
        return convert(c.value, [&](const CpsAtom& val) {
            return _arena.emplace(CpsLet { .target = next_var("_"),
                .op = PrimOp::BoxSet,
                .args = { box, val },
                .body = k(CpsAtom(CpsUnit())) });
        });
    }

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
    if (const auto* var = func_expr.get<CoreVar>())
        if (var->kind == CoreVarKind::Builtin)
            return convert_args(
                c.args, { }, [this, var, k](std::vector<CpsAtom> args) {
                    return try_builtin(var->id.debug_name, std::move(args), k);
                });

    return convert(c.func, [&](const CpsAtom& func) {
        return convert_args(c.args, { }, [&](std::vector<CpsAtom> args) {
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

void CpsConverter::collect_global_defines(
    CoreExprRef ref, std::set<CoreVar>& out) {
    if (!ref.is_valid())
        return;

    _core_arena[ref].visit(overloaded {
        [&](const CoreDefine& d) {
            if (d.target.kind == CoreVarKind::Global)
                out.insert(d.target);
            collect_global_defines(d.value, out);
        },
        [&](const CoreSet& s) { collect_global_defines(s.value, out); },
        [&](const CoreLambda& l) { collect_global_defines(l.body, out); },
        [&](const CoreIf& i) {
            collect_global_defines(i.condition, out);
            collect_global_defines(i.then_branch, out);
            if (i.else_branch.is_valid())
                collect_global_defines(i.else_branch, out);
        },
        [&](const CoreSeq& s) {
            for (const auto& e : s.exprs)
                collect_global_defines(e, out);
        },
        [&](const CoreApply& a) {
            collect_global_defines(a.func, out);
            for (const auto& arg : a.args)
                collect_global_defines(arg, out);
        },
        [&](const CoreVar&) { },
        [&](const CoreConstant&) { },
    });
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
    std::set<CoreVar> globals;
    collect_global_defines(root, globals);

    std::vector<CpsVar> global_boxes;
    global_boxes.reserve(globals.size());
    for (const auto& global : globals) {
        auto box = next_var(global.id.debug_name + "_box");
        extend(global, CpsAtom(box));
        global_boxes.push_back(box);
    }

    auto k = [this](CpsAtom result) -> CpsExprRef {
        return _arena.emplace(CpsHalt { std::move(result) });
    };
    auto body = convert(root, k);

    for (auto i = global_boxes.size(); i > 0; --i) {
        auto box = global_boxes[i - 1];
        body = _arena.emplace(CpsLet { .target = box,
            .op = PrimOp::Box,
            .args = { CpsAtom(CpsUnit()) },
            .body = body });
    }

    return body;
}

CpsExprRef LowerPass::run(CoreExprRef root, CompilerContext& ctx) {
    CpsConverter lowerer(ctx);
    auto entry = lowerer.lower_program(root);
    Assert(entry.is_valid());
    return entry;
}

std::string LowerPass::dump(
    const CpsExprRef& expr, CompilerContext& ctx) const {
    CpsDumpVisitor visitor {
        .arena = ctx.cps_arena(), .span_arena = ctx.span_arena(), .indent = "  "
    };
    return std::format("{}\n", visitor.dump(expr, ""));
}

} // namespace lpc::cps
