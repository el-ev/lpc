module lpc.sema.sema;

import std;

import lpc.sema.core_form;
import lpc.context;
import lpc.syntax.ast;
import lpc.utils.logging;

namespace lpc::sema {

using namespace lpc::syntax;
using lpc::utils::Error;

void SymbolTable::init_builtins() {
#ifndef PRIM
#define PRIM(str, min, max, op) BUILTIN(str, min, max)
#endif
#ifndef BUILTIN
#define BUILTIN(str, min, max) define_builtin(str, Arity { min, max });
#endif
#include "builtins.def"
#undef PRIM
#undef BUILTIN
}

void Lowerer::report_error(SpanRef ref, std::string_view msg) {
    _had_error = true;
    (void)_spans.report_error(ref, msg);
}

const std::string* Lowerer::head_name(const SExprList& list) const {
    if (list.elem.empty())
        return nullptr;
    if (const auto* id = _spans.get<LispIdent>(list.elem[0]))
        return &id->name;
    return nullptr;
}

CoreExprRef Lowerer::lower(SpanRef ref) {
    if (!ref.is_valid())
        return CoreExprRef::invalid();

    const auto& sexpr = _spans.expr(ref);

    if (const auto* ident = sexpr.get<LispIdent>())
        return lower_variable(ref, *ident);

    if (const auto* list = sexpr.get<SExprList>())
        return lower_list(ref, *list);

    if (sexpr.isa<LispNumber>() || sexpr.isa<LispString>()
        || sexpr.isa<LispChar>() || sexpr.isa<LispBool>())
        return lower_literal(ref);

    if (sexpr.isa<LispNil>())
        return lower_literal(ref);

    if (sexpr.isa<SExprVector>())
        return lower_literal(ref);

    report_error(ref, "sema: unexpected expression type");
    return CoreExprRef::invalid();
}

CoreExprRef Lowerer::lower_variable(SpanRef ref, const LispIdent& id) {
    auto resolved = _syms.resolve(id.name);
    if (!resolved)
        resolved = _syms.declare_ref(id.name);
    _syms.mark_referenced(*resolved);
    return _core.emplace(ref, *resolved);
}

CoreExprRef Lowerer::lower_literal(SpanRef ref) {
    return _core.emplace(ref, CoreConstant { .value = ref });
}

CoreExprRef Lowerer::lower_list(SpanRef ref, const SExprList& list) {
    if (list.elem.empty()) {
        report_error(ref, "sema: empty list, should not happen");
        return CoreExprRef::invalid();
    }

    const auto* name = head_name(list);
    if (name != nullptr) {
        if (*name == "lambda")
            return lower_lambda(ref, list);
        if (*name == "if")
            return lower_if(ref, list);
        if (*name == "set!")
            return lower_set(ref, list);
        if (*name == "define")
            return lower_define(ref, list);
        if (*name == "quote")
            return lower_quote(ref, list);
        if (*name == "begin")
            return lower_begin(ref, list);
    }

    return lower_application(ref, list);
}

CoreExprRef Lowerer::lower_lambda(SpanRef ref, const SExprList& list) {
    // (lambda (params...) body...)
    // After expansion: list.elem = [lambda, formals, body1, ..., bodyN, nil]
    _syms.push_scope();

    std::vector<CoreVar> params;
    std::optional<CoreVar> rest_param;

    auto formals_ref = list.elem[1];
    if (const auto* param_list = _spans.get<SExprList>(formals_ref)) {
        for (std::size_t i = 0; i < param_list->elem.size(); ++i) {
            auto p = param_list->elem[i];
            if (_spans.is_nil(p))
                continue;

            if (const auto* pid = _spans.get<LispIdent>(p)) {
                bool is_last_elem = (i == param_list->elem.size() - 1);
                bool has_nil_sentinel = !param_list->elem.empty()
                    && _spans.is_nil(param_list->elem.back());
                bool is_rest = is_last_elem && !has_nil_sentinel;

                auto var = _syms.define(pid->name);
                if (is_rest)
                    rest_param = var;
                else
                    params.push_back(var);
            }
        }
    } else if (const auto* rest_id = _spans.get<LispIdent>(formals_ref)) {
        // (lambda x body...) — single rest parameter
        rest_param = _syms.define(rest_id->name);
    }

    std::vector<CoreExprRef> body_exprs;
    bool seen_command = false;
    for (std::size_t i = 2; i < list.elem.size(); ++i) {
        auto expr_ref = list.elem[i];
        if (_spans.is_nil(expr_ref))
            continue;

        bool is_def = false;
        if (const auto* l = _spans.get<SExprList>(expr_ref))
            if (const auto* hn = head_name(*l))
                if (*hn == "define")
                    is_def = true;

        if (is_def && seen_command) {
            report_error(expr_ref,
                "sema: internal definitions must appear before commands");
            _syms.pop_scope();
            return CoreExprRef::invalid();
        }
        if (!is_def)
            seen_command = true;

        auto lowered = lower(expr_ref);
        if (!lowered.is_valid()) {
            _syms.pop_scope();
            return CoreExprRef::invalid();
        }
        body_exprs.push_back(lowered);
    }

    _syms.pop_scope();

    CoreExprRef body;
    if (body_exprs.size() == 1)
        body = body_exprs[0];
    else
        body = _core.emplace(ref, CoreSeq { .exprs = std::move(body_exprs) });

    return _core.emplace(ref,
        CoreLambda {
            .params = std::move(params),
            .rest_param = rest_param,
            .body = body,
        });
}

CoreExprRef Lowerer::lower_if(SpanRef ref, const SExprList& list) {
    // (if cond then else?)
    // list.elem = [if, cond, then, else?, nil]
    auto cond = lower(list.elem[1]);
    if (!cond.is_valid())
        return CoreExprRef::invalid();

    auto then_br = lower(list.elem[2]);
    if (!then_br.is_valid())
        return CoreExprRef::invalid();

    CoreExprRef else_br = CoreExprRef::invalid();
    if (list.elem.size() >= 5 && !_spans.is_nil(list.elem[3])) {
        else_br = lower(list.elem[3]);
        if (!else_br.is_valid())
            return CoreExprRef::invalid();
    }

    return _core.emplace(ref,
        CoreIf {
            .condition = cond,
            .then_branch = then_br,
            .else_branch = else_br,
        });
}

CoreExprRef Lowerer::lower_set(SpanRef ref, const SExprList& list) {
    // (set! var expr)
    // list.elem = [set!, var, expr, nil]
    const auto* target_id = _spans.get<LispIdent>(list.elem[1]);
    auto resolved = _syms.resolve(target_id->name);
    if (!resolved) {
        report_error(list.elem[1], "sema: set! on undefined variable: {}",
            target_id->name);
        return CoreExprRef::invalid();
    }

    // FIXME: bad & no longer works
    // if (resolved->kind == CoreVarKind::Builtin) {
    //     report_error(list.elem[1], "sema: cannot mutate builtin procedure: {}",
    //         target_id->name);
    //     return CoreExprRef::invalid();
    // }

    auto value = lower(list.elem[2]);
    if (!value.is_valid())
        return CoreExprRef::invalid();

    return _core.emplace(ref,
        CoreSet {
            .target = *resolved,
            .value = value,
        });
}

CoreExprRef Lowerer::lower_define(SpanRef ref, const SExprList& list) {
    // (define var expr)
    // list.elem = [define, var, expr, nil]
    const auto* target_id = _spans.get<LispIdent>(list.elem[1]);

    auto var = _syms.define(target_id->name);

    auto value = lower(list.elem[2]);
    if (!value.is_valid())
        return CoreExprRef::invalid();

    return _core.emplace(ref,
        CoreDefine {
            .target = var,
            .value = value,
        });
}

CoreExprRef Lowerer::lower_quote(SpanRef ref, const SExprList& list) {
    // (quote datum)
    // list.elem = [quote, datum, nil]
    return _core.emplace(ref, CoreConstant { .value = list.elem[1] });
}

CoreExprRef Lowerer::lower_begin(SpanRef ref, const SExprList& list) {
    // (begin expr...)
    // list.elem = [begin, expr1, ..., exprN, nil]
    std::vector<CoreExprRef> exprs;
    for (std::size_t i = 1; i < list.elem.size(); ++i) {
        if (_spans.is_nil(list.elem[i]))
            continue;
        auto lowered = lower(list.elem[i]);
        if (!lowered.is_valid())
            return CoreExprRef::invalid();
        exprs.push_back(lowered);
    }

    if (exprs.size() == 1)
        return exprs[0];

    return _core.emplace(ref, CoreSeq { .exprs = std::move(exprs) });
}

CoreExprRef Lowerer::lower_application(SpanRef ref, const SExprList& list) {
    // (func arg...)
    // list.elem = [func, arg1, ..., argN, nil]
    auto func = lower(list.elem[0]);
    if (!func.is_valid())
        return CoreExprRef::invalid();

    std::vector<CoreExprRef> args;
    for (std::size_t i = 1; i < list.elem.size(); ++i) {
        if (_spans.is_nil(list.elem[i]))
            continue;
        auto lowered = lower(list.elem[i]);
        if (!lowered.is_valid())
            return CoreExprRef::invalid();
        args.push_back(lowered);
    }

    if (const auto* v = _core.at(func).get<CoreVar>()) {
        if (auto arity = _syms.get_builtin_arity(*v)) {
            if (!arity->is_applicable(
                    static_cast<std::uint32_t>(args.size()))) {
                if (arity->is_variadic()) {
                    report_error(ref,
                        "sema: builtin '{}' requires at least {} arguments, "
                        "got {}",
                        v->id.debug_name, arity->min_args, args.size());
                } else if (arity->min_args == arity->max_args) {
                    report_error(ref,
                        "sema: builtin '{}' requires {} arguments, got {}",
                        v->id.debug_name, arity->min_args, args.size());
                } else {
                    report_error(ref,
                        "sema: builtin '{}' requires between {} and {} "
                        "arguments, got {}",
                        v->id.debug_name, arity->min_args, arity->max_args,
                        args.size());
                }
                // We still emplace it but we marked it as error
            }
        }
    }

    return _core.emplace(ref,
        CoreApply {
            .func = func,
            .args = std::move(args),
        });
}

CoreExprRef Lowerer::lower_program(SpanRef root) {
    _core.init_builtins(_syms.get_builtins());
    const auto* top_list = _spans.get<SExprList>(root);
    std::vector<CoreExprRef> forms;
    bool seen_command = false;
    for (const auto& form : top_list->elem) {
        if (_spans.is_nil(form))
            continue;

        bool is_def = false;
        if (const auto* l = _spans.get<SExprList>(form))
            if (const auto* hn = head_name(*l))
                if (*hn == "define")
                    is_def = true;

        if (is_def && seen_command)
            report_error(form, "sema: definitions must appear before commands");

        if (!is_def)
            seen_command = true;

        auto lowered = lower(form);
        if (!lowered.is_valid())
            continue;
        forms.push_back(lowered);
    }

    auto undefined = _syms.get_undefined_globals();
    for (const auto& name : undefined)
        report_error(root, "sema: undefined variable: {}", name);

    if (_had_error)
        return CoreExprRef::invalid();

    if (forms.empty())
        return _core.emplace(root, CoreConstant { .value = root });

    if (forms.size() == 1)
        return forms[0];

    return _core.emplace(root, CoreSeq { .exprs = std::move(forms) });
}

static std::string dump_core(
    const CoreExprArena& core, const SpanArena& spans, CoreExprRef ref) {
    if (!ref.is_valid())
        return "<invalid>";

    const auto& expr = core.at(ref);

    if (const auto* var = expr.get<CoreVar>())
        return var->id.debug_name;

    if (const auto* lit = expr.get<CoreConstant>())
        return "'" + spans.dump(lit->value);

    if (const auto* lam = expr.get<CoreLambda>()) {
        std::string out = "(lambda (";
        for (std::size_t i = 0; i < lam->params.size(); ++i) {
            if (i > 0)
                out += " ";
            out += lam->params[i].id.debug_name;
        }
        if (lam->rest_param) {
            if (!lam->params.empty())
                out += " . ";
            out += lam->rest_param->id.debug_name;
        }
        out += ") " + dump_core(core, spans, lam->body) + ")";
        return out;
    }

    if (const auto* iff = expr.get<CoreIf>()) {
        std::string out = "(if " + dump_core(core, spans, iff->condition) + " "
            + dump_core(core, spans, iff->then_branch);
        if (iff->else_branch.is_valid())
            out += " " + dump_core(core, spans, iff->else_branch);
        return out + ")";
    }

    if (const auto* set = expr.get<CoreSet>())
        return "(set! " + set->target.id.debug_name + " "
            + dump_core(core, spans, set->value) + ")";

    if (const auto* def = expr.get<CoreDefine>())
        return "(define " + def->target.id.debug_name + " "
            + dump_core(core, spans, def->value) + ")";

    if (const auto* seq = expr.get<CoreSeq>()) {
        std::string out = "(begin";
        for (const auto& e : seq->exprs)
            out += " " + dump_core(core, spans, e);
        return out + ")";
    }

    if (const auto* app = expr.get<CoreApply>()) {
        std::string out = "(" + dump_core(core, spans, app->func);
        for (const auto& a : app->args)
            out += " " + dump_core(core, spans, a);
        return out + ")";
    }

    return "<unknown>";
}

static std::string dump_program(
    const CoreExprArena& core, const SpanArena& spans, CoreExprRef ref) {
    if (!ref.is_valid())
        return "";

    const auto& expr = core.at(ref);
    if (const auto* seq = expr.get<CoreSeq>()) {
        std::string out;
        for (const auto& e : seq->exprs) {
            if (spans.is_core_binding(core.at(e).origin))
                continue;
            out += dump_core(core, spans, e) + "\n";
        }
        return out;
    }

    if (spans.is_core_binding(expr.origin))
        return "";

    return dump_core(core, spans, ref) + "\n";
}

CoreExprRef SemaPass::run(SpanRef root, CompilerContext& ctx) noexcept {
    Lowerer lowerer(
        ctx.span_arena(), ctx.core_arena(), ctx.options().show_core_expansion);
    auto result = lowerer.lower_program(root);
    _failed = lowerer.had_error();
    return result;
}

std::string SemaPass::dump(
    const CoreExprRef& result, CompilerContext& ctx) const noexcept {
    return dump_program(ctx.core_arena(), ctx.span_arena(), result);
}

} // namespace lpc::sema
