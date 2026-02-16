module lpc.cps.lower;

import std;
import lpc.context;
import lpc.cps.ir;
import lpc.sema.core_form;
import lpc.syntax.refs;
import lpc.syntax.arenas;
import lpc.utils.logging;

namespace lpc::cps {

using namespace lpc::sema;

namespace {
    template <typename... Ts>
    struct CoreExprVisitor : Ts... {
        using Ts::operator()...;
    };
}

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
                return lookup(var->id);
        }
        return std::nullopt;
    }

public:
    CpsVar next_var(std::string debug_name = "") {
        auto id = _next_var_id++;
        return CpsVar(VarId(id,
            debug_name.empty() ? std::format("v_{}", id)
                               : std::move(debug_name)));
    }

private:
    CompilerContext& _ctx;
    CpsArena& _arena;
    CoreExprArena& _core_arena;
    std::unordered_map<VarId, CpsAtom> mapping;
    std::uint32_t _next_var_id = 0;

    [[nodiscard]] CpsAtom lookup(const VarId& source_id) const {
        return mapping.at(source_id);
    }

    void extend(const VarId& source_id, CpsAtom&& cps_val) {
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
    auto rv = next_var("r");
    auto k_lambda = _arena.emplace(CpsLambda(kv, { rv }, k(CpsAtom(rv))));

    auto join_k = [this, kv](const CpsAtom& res) {
        return _arena.emplace(CpsApp(CpsAtom(kv), { res }));
    };

    auto iff = convert(c.condition, [&](CpsAtom condition) {
        auto then_branch = convert(c.then_branch, join_k);
        auto else_branch = convert(c.else_branch, join_k);
        return _arena.emplace(CpsIf { .condition = std::move(condition),
            .then_branch = then_branch,
            .else_branch = else_branch });
    });

    return _arena.emplace(CpsFix({ k_lambda }, iff));
}

template <>
CpsExprRef CpsConverter::convert<CoreLambda>(
    const CoreLambda& c, Continuation k) {
    std::vector<CpsVar> cps_params;
    std::vector<std::tuple<VarId, CpsVar, CpsVar>> boxed;

    auto scope = [&](const CoreVar& var, const CpsVar& p) {
        if (_ctx.core_arena().is_mutated(var)) {
            auto box = next_var(var.id.debug_name + "_box");
            extend(var.id, CpsAtom(box));
            boxed.emplace_back(var.id, p, box);
        } else {
            extend(var.id, CpsAtom(p));
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

    CpsVar k_dyn = next_var("k_dyn");
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

    auto lambda_var = next_var();
    auto lambda
        = _arena.emplace(CpsLambda(lambda_var, std::move(cps_params), body));
    auto rest = k(CpsAtom(lambda_var));

    return _arena.emplace(CpsFix({ lambda }, rest));
}

template <>
CpsExprRef CpsConverter::convert<CoreVar>(const CoreVar& c, Continuation k) {
    auto val = lookup(c.id);
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
    return convert(c.value, [&](const CpsAtom& val) {
        if (!_ctx.core_arena().is_mutated(c.target)) {
            extend(c.target.id, CpsAtom(val));
            return k(CpsAtom(CpsUnit()));
        }

        auto box = next_var(c.target.id.debug_name + "_box");
        extend(c.target.id, CpsAtom(box));
        return _arena.emplace(CpsLet { .target = box,
            .op = PrimOp::Box,
            .args = { val },
            .body = k(CpsAtom(CpsUnit())) });
    });
}

template <>
CpsExprRef CpsConverter::convert<CoreApply>(
    const CoreApply& c, Continuation k) {
    return convert(c.func, [&](const CpsAtom& func) {
        return convert_args(c.args, {}, [&](std::vector<CpsAtom> args) {
            auto kv = next_var("k_ret");
            auto rv = next_var("r");
            auto k_lambda
                = _arena.emplace(CpsLambda(kv, { rv }, k(CpsAtom(rv))));
            args.push_back(CpsAtom(kv));
            auto app = _arena.emplace(CpsApp(func, std::move(args)));
            return _arena.emplace(CpsFix({ k_lambda }, app));
        });
    });
}

template <>
CpsExprRef CpsConverter::convert<CoreSet>(const CoreSet& c, Continuation k) {
    return convert(c.value, [&](const CpsAtom& val) {
        auto box = lookup(c.target.id);
        return _arena.emplace(CpsLet { .target = next_var("unused"),
            .op = PrimOp::BoxSet,
            .args = { box, val },
            .body = k(CpsAtom(CpsUnit())) });
    });
}

CpsExprRef CpsConverter::convert(CoreExprRef ref, Continuation k) {
    return _ctx.core_arena().at(ref).visit(
        CoreExprVisitor { [this, k](const auto& c) { return convert(c, k); } });
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
        CoreExprVisitor { [this, k](const auto& c) { return convert(c, k); } });
}

namespace {
    std::string primop_to_string(PrimOp op) {
        switch (op) {
        case PrimOp::Add:
            return "+";
        case PrimOp::Sub:
            return "-";
        case PrimOp::Mul:
            return "*";
        case PrimOp::Div:
            return "/";
        case PrimOp::Eq:
            return "=";
        case PrimOp::Lt:
            return "<";
        case PrimOp::Gt:
            return ">";
        case PrimOp::Le:
            return "<=";
        case PrimOp::Ge:
            return ">=";
        case PrimOp::Cons:
            return "cons";
        case PrimOp::Car:
            return "car";
        case PrimOp::Cdr:
            return "cdr";
        case PrimOp::Box:
            return "box";
        case PrimOp::BoxGet:
            return "box-get";
        case PrimOp::BoxSet:
            return "box-set";
        }
        return "unknown";
    }

    struct CpsLispDumpVisitor {
        const CpsArena& arena;
        const syntax::SpanArena& span_arena;

        [[nodiscard]] std::string atom_to_string(const CpsAtom& atom) const {
            return atom.visit(CoreExprVisitor {
                [](const CpsVar& v) { return v.var.debug_name; },
                [&](const CpsConstant& c) {
                    // FIXME: what?
                    return std::format("{}", span_arena.dump(c.value));
                },
                [](const CpsUnit&) { return std::string("#<unit>"); },
            });
        }

        std::string operator()(const CpsApp& app) const {
            std::string out = "(" + atom_to_string(app.func);
            for (const auto& arg : app.args) {
                out += " " + atom_to_string(arg);
            }
            out += ")";
            return out;
        }

        std::string operator()(const CpsLet& l) const {
            std::string out = std::format("(let (({} ({} ",
                l.target.var.debug_name, primop_to_string(l.op));
            for (std::size_t i = 0; i < l.args.size(); ++i) {
                if (i > 0)
                    out += " ";
                out += atom_to_string(l.args[i]);
            }
            out += "))) " + dump(l.body) + ")";
            return out;
        }

        std::string operator()(const CpsIf& i) const {
            return std::format("(if {} {} {})", atom_to_string(i.condition),
                dump(i.then_branch), dump(i.else_branch));
        }

        std::string operator()(const CpsLambda& l) const {
            std::string out = "(lambda (";
            for (std::size_t i = 0; i < l.params.size(); ++i) {
                if (i > 0)
                    out += " ";
                out += l.params[i].var.debug_name;
            }
            out += ") " + dump(l.body) + ")";
            return out;
        }

        std::string operator()(const CpsFix& f) const {
            std::string out = "(fix (";
            for (std::size_t i = 0; i < f.functions.size(); ++i) {
                if (i > 0)
                    out += " ";
                const auto& func = arena.get(f.functions[i]).get<CpsLambda>();
                out += std::format(
                    "({} {})", func->name.var.debug_name, dump(f.functions[i]));
            }
            out += ") " + dump(f.body) + ")";
            return out;
        }

        std::string operator()(const CpsHalt& h) const {
            return "(halt " + atom_to_string(h.value) + ")";
        }

        [[nodiscard]] std::string dump(CpsExprRef ref) const {
            return arena.get(ref).visit(*this);
        }
    };

    struct CpsDumpVisitor {
        const CpsArena& arena;
        const syntax::SpanArena& span_arena;
        std::string indent;

        [[nodiscard]] std::string atom_to_string(const CpsAtom& atom) const {
            return atom.visit(CoreExprVisitor {
                [](const CpsVar& v) { return v.var.debug_name; },
                [&](const CpsConstant& c) {
                    // FIXME: what?
                    return std::format("{}", span_arena.dump(c.value));
                },
                [](const CpsUnit&) { return std::string("#<unit>"); },
            });
        }

        std::string operator()(const CpsApp& app) const {
            std::string out = "(" + atom_to_string(app.func);
            for (const auto& arg : app.args)
                out += " " + atom_to_string(arg);
            out += ")";
            return out;
        }

        std::string operator()(const CpsLet& l) const {
            std::string out = "let " + l.target.var.debug_name + " = ";
            out += std::format("#prim#{}(", primop_to_string(l.op));
            for (std::size_t i = 0; i < l.args.size(); ++i) {
                if (i > 0)
                    out += ", ";
                out += atom_to_string(l.args[i]);
            }
            out += ") in\n" + indent + dump(l.body, indent);
            return out;
        }

        std::string operator()(const CpsIf& i) const {
            std::string out = "if " + atom_to_string(i.condition) + " then\n";
            out += indent + "  " + dump(i.then_branch, indent + "  ") + "\n";
            out += indent + "else\n";
            out += indent + "  " + dump(i.else_branch, indent + "  ");
            return out;
        }

        std::string operator()(const CpsLambda& l) const {
            std::string out = "lambda " + l.name.var.debug_name + "(";
            for (std::size_t i = 0; i < l.params.size(); ++i) {
                if (i > 0)
                    out += ", ";
                out += l.params[i].var.debug_name;
            }
            out += ") =\n" + indent + "  " + dump(l.body, indent + "  ");
            return out;
        }

        std::string operator()(const CpsFix& f) const {
            std::string out = "fix\n";
            for (const auto& func : f.functions)
                out += indent + "  " + dump(func, indent + "  ") + "\n";
            out += indent + "in " + dump(f.body, indent);
            return out;
        }

        std::string operator()(const CpsHalt& h) const {
            return "halt " + atom_to_string(h.value);
        }

        [[nodiscard]] std::string dump(
            CpsExprRef ref, std::string next_indent) const {
            CpsDumpVisitor visitor { .arena = arena,
                .span_arena = span_arena,
                .indent = std::move(next_indent) };
            return arena.get(ref).visit(visitor);
        }
    };
}

CpsExprRef LowerPass::run(CoreExprRef root, CompilerContext& ctx) noexcept {
    CpsConverter lowerer(ctx);
    auto entry = lowerer.lower_program(root);
    if (entry == CpsExprRef::invalid()) {
        _failed = true;
    }
    return entry;
}

std::string LowerPass::dump(
    const CpsExprRef& result, CompilerContext& ctx) const noexcept {
    if (result == CpsExprRef::invalid())
        return "<invalid>";
    CpsDumpVisitor visitor {
        .arena = ctx.cps_arena(), .span_arena = ctx.span_arena(), .indent = "  "
    };
    return visitor.dump(result, "");
}

} // namespace lpc::cps
