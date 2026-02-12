module lpc.frontend.expand;

import std;
import lpc.utils.logging;
import lpc.frontend.lexer;
import lpc.frontend.syntax;

namespace lpc::frontend {

using lpc::utils::Error;

struct ExpCtx {
    LexEnv& env;
    SExprArena& arena;
    ExpansionStack& stack;
    ExpStackRef parent;
    bool is_core;
    bool show_core;
    bool is_top_level;
    bool& had_error;
    // FIXME: this is just stupid
    std::optional<ScopeID> output_excluded_scope;
};

[[nodiscard]] static SExprLocRef add_scope(
    SExprLocRef expr, ScopeID scope, SExprArena& arena) {
    if (!expr.is_valid())
        return expr;
    const auto& sexpr = arena.at(expr);
    if (sexpr.isa<LispIdent>()) {
        auto ident = sexpr.get_unchecked<LispIdent>();
        ident.scopes.insert(scope);
        return arena.emplace(expr.loc_ref(), std::move(ident));
    }
    if (sexpr.isa<SExprList>()) {
        auto elems = sexpr.get_unchecked<SExprList>().elem; // copy
        std::vector<SExprLocRef> v;
        v.reserve(elems.size());
        for (const auto& el : elems)
            v.push_back(add_scope(el, scope, arena));
        return arena.emplace(expr.loc_ref(), SExprList(std::move(v)));
    }
    if (sexpr.isa<SExprVector>()) {
        auto elems = sexpr.get_unchecked<SExprVector>().elem; // copy
        std::vector<SExprLocRef> v;
        v.reserve(elems.size());
        for (const auto& el : elems)
            v.push_back(add_scope(el, scope, arena));
        return arena.emplace(expr.loc_ref(), SExprVector(std::move(v)));
    }
    return expr;
}

struct ScopeKey {
    std::string name;
    std::set<ScopeID> scopes;
    auto operator<=>(const ScopeKey&) const = default;
};

static void collect_idents(SExprLocRef root, SExprArena& arena,
    std::map<std::string, std::set<std::set<ScopeID>>>& groups) {
    if (!root.is_valid())
        return;
    const auto& expr = arena.at(root);
    if (expr.isa<LispIdent>()) {
        const auto& id = expr.get_unchecked<LispIdent>();
        groups[id.name].insert(id.scopes);
    } else if (expr.isa<SExprList>()) {
        for (const auto& el : expr.get_unchecked<SExprList>().elem)
            collect_idents(el, arena, groups);
    } else if (expr.isa<SExprVector>()) {
        for (const auto& el : expr.get_unchecked<SExprVector>().elem)
            collect_idents(el, arena, groups);
    }
}

static SExprLocRef apply_names(SExprLocRef root, SExprArena& arena,
    const std::map<ScopeKey, std::string>& name_map) {
    if (!root.is_valid())
        return root;
    const auto& expr = arena.at(root);
    if (expr.isa<LispIdent>()) {
        const auto& id = expr.get_unchecked<LispIdent>();
        auto it
            = name_map.find(ScopeKey { .name = id.name, .scopes = id.scopes });
        if (it != name_map.end())
            return arena.emplace(root.loc_ref(), LispIdent(it->second));
        return root;
    }
    if (expr.isa<SExprList>()) {
        const auto& list = expr.get_unchecked<SExprList>();
        std::vector<SExprLocRef> v;
        v.reserve(list.elem.size());
        for (const auto& el : list.elem)
            v.push_back(apply_names(el, arena, name_map));
        return arena.emplace(root.loc_ref(), SExprList(std::move(v)));
    }
    if (expr.isa<SExprVector>()) {
        const auto& vec = expr.get_unchecked<SExprVector>();
        std::vector<SExprLocRef> v;
        v.reserve(vec.elem.size());
        for (const auto& el : vec.elem)
            v.push_back(apply_names(el, arena, name_map));
        return arena.emplace(root.loc_ref(), SExprVector(std::move(v)));
    }
    return root;
}

[[nodiscard]] static SExprLocRef resolve_names(
    SExprLocRef root, SExprArena& arena) {
    std::map<std::string, std::set<std::set<ScopeID>>> groups;
    collect_idents(root, arena, groups);

    std::map<ScopeKey, std::string> name_map;
    for (const auto& [name, scope_sets] : groups) {
        if (scope_sets.size() == 1) {
            name_map[{ name, *scope_sets.begin() }] = name;
            continue;
        }
        bool bare_assigned = false;
        int suffix = 0;
        for (const auto& scopes : scope_sets) {
            if (scopes.empty() && !bare_assigned) {
                name_map[{ name, scopes }] = name;
                bare_assigned = true;
            } else {
                name_map[{ name, scopes }]
                    = name + "." + std::to_string(suffix++);
            }
        }
        if (!bare_assigned) {
            auto first_it = scope_sets.begin();
            name_map[{ name, *first_it }] = name;
        }
    }
    return apply_names(root, arena, name_map);
}

[[nodiscard]] static SExprLocRef make_canonical(
    LocRef loc, const std::string& name, SExprArena& arena) {
    return arena.emplace(loc, LispIdent(name));
}

[[nodiscard]] static SExprLocRef expand(SExprLocRef root, ExpCtx ctx);

static SExprLocRef expand_lambda(
    const SExprList& list, SExprLocRef root, ExpCtx ctx) {
    // (lambda (params…) body…)   or   (lambda rest-arg body…)
    if (list.elem.size() < 3)
        return root;

    ScopeID scope = ctx.env.new_scope();
    auto scoped_params = add_scope(list.elem[1], scope, ctx.arena);

    auto bind = [&](SExprLocRef p) {
        if (ctx.arena.at(p).isa<LispIdent>()) {
            auto id = ctx.arena.at(p).get_unchecked<LispIdent>();
            ctx.env.add_binding(id, Binding(VarBinding { id }));
        }
    };
    const auto& p_expr = ctx.arena.at(scoped_params);
    if (p_expr.isa<SExprList>()) {
        for (const auto& p : p_expr.get_unchecked<SExprList>().elem)
            bind(p);
    } else if (p_expr.isa<LispIdent>()) {
        bind(scoped_params);
    }

    std::vector<SExprLocRef> out;
    out.push_back(make_canonical(list.elem[0].loc_ref(), "lambda", ctx.arena));
    out.push_back(scoped_params);
    for (std::size_t i = 2; i < list.elem.size(); ++i) {
        auto body_ctx = ctx;
        body_ctx.is_top_level = false;
        auto r = expand(add_scope(list.elem[i], scope, ctx.arena), body_ctx);
        if (!r.is_valid())
            return r;
        out.push_back(r);
    }
    return ctx.arena.emplace(root.loc_ref(), SExprList(std::move(out)));
}

static SExprLocRef expand_quote(
    const SExprList& list, SExprLocRef root, SExprArena& arena) {
    if (list.elem.size() < 2)
        return root;
    std::vector<SExprLocRef> out;
    out.push_back(make_canonical(list.elem[0].loc_ref(), "quote", arena));
    out.push_back(list.elem[1]);
    out.push_back(arena.emplace(root.loc_ref(), LispNil()));
    return arena.emplace(root.loc_ref(), SExprList(std::move(out)));
}

static SExprLocRef expand_simple(const std::string& kw, const SExprList& list,
    SExprLocRef root, ExpCtx ctx) {
    std::vector<SExprLocRef> out;
    out.push_back(make_canonical(list.elem[0].loc_ref(), kw, ctx.arena));
    for (std::size_t i = 1; i < list.elem.size(); ++i) {
        auto sub_ctx = ctx;
        auto r = expand(list.elem[i], sub_ctx);
        if (!r.is_valid())
            return r;
        out.push_back(r);
    }
    return ctx.arena.emplace(root.loc_ref(), SExprList(std::move(out)));
}

static SExprLocRef expand_set(
    const SExprList& list, SExprLocRef root, ExpCtx ctx) {
    // (set! variable expression)
    if (list.elem.size() < 3)
        return root;
    std::vector<SExprLocRef> out;
    out.push_back(make_canonical(list.elem[0].loc_ref(), "set!", ctx.arena));
    for (std::size_t i = 1; i < list.elem.size(); ++i) {
        auto val_ctx = ctx;
        val_ctx.is_top_level = false;
        auto r = expand(list.elem[i], val_ctx);
        if (!r.is_valid())
            return r;
        out.push_back(r);
    }
    return ctx.arena.emplace(root.loc_ref(), SExprList(std::move(out)));
}

static SExprLocRef expand_define(
    const SExprList& list, SExprLocRef root, ExpCtx ctx) {
    if (list.elem.size() < 3)
        return root;

    auto var = list.elem[1];

    if (ctx.arena.at(var).isa<SExprList>()) {
        const auto& var_list
            = ctx.arena.at(var).get_unchecked<SExprList>().elem;
        if (var_list.empty())
            return root;

        auto func_name = var_list[0];

        std::vector<SExprLocRef> params;
        std::size_t var_logical = var_list.size();
        if (!var_list.empty() && ctx.arena.at(var_list.back()).isa<LispNil>())
            var_logical--;
        for (std::size_t i = 1; i < var_logical; ++i)
            params.push_back(var_list[i]);
        params.push_back(ctx.arena.emplace(var.loc_ref(), LispNil()));
        auto params_node
            = ctx.arena.emplace(var.loc_ref(), SExprList(std::move(params)));

        std::vector<SExprLocRef> lam;
        lam.push_back(
            make_canonical(list.elem[0].loc_ref(), "lambda", ctx.arena));
        lam.push_back(params_node);
        for (std::size_t i = 2; i < list.elem.size(); ++i)
            lam.push_back(list.elem[i]);
        auto lam_node
            = ctx.arena.emplace(var.loc_ref(), SExprList(std::move(lam)));

        std::vector<SExprLocRef> def;
        def.push_back(
            make_canonical(list.elem[0].loc_ref(), "define", ctx.arena));
        def.push_back(func_name);
        def.push_back(lam_node);
        def.push_back(ctx.arena.emplace(root.loc_ref(), LispNil()));
        auto desugared
            = ctx.arena.emplace(root.loc_ref(), SExprList(std::move(def)));
        return expand(desugared, ctx);
    }

    // (define var expr)
    if (ctx.arena.at(var).isa<LispIdent>()) {
        ctx.env.add_binding(ctx.arena.at(var).get_unchecked<LispIdent>(),
            Binding(
                VarBinding { ctx.arena.at(var).get_unchecked<LispIdent>() }));
    }

    std::vector<SExprLocRef> out;
    out.push_back(make_canonical(list.elem[0].loc_ref(), "define", ctx.arena));
    out.push_back(var);
    for (std::size_t i = 2; i < list.elem.size(); ++i) {
        auto val_ctx = ctx;
        val_ctx.is_top_level = false;
        auto r = expand(list.elem[i], val_ctx);
        if (!r.is_valid())
            return r;
        out.push_back(r);
    }
    return ctx.arena.emplace(root.loc_ref(), SExprList(std::move(out)));
}

static void report_syntax_error(
    const std::string& message, SExprLocRef expr, const ExpCtx& ctx) {
    ctx.had_error = true;
    Error("{}", message);

    auto loc = ctx.arena.location(expr.loc_ref());

    auto cur = ctx.parent;
    if (cur != ExpansionStack::INVALID) {
        std::println(std::cerr, "  in: {}", ctx.arena.dump(expr.expr_ref()));
        std::println(std::cerr, "  at {}", loc.source_location());
    } else {
        std::println(std::cerr, "  at {}", loc.source_location());
    }

    int core_omitted = 0;
    while (cur != ExpansionStack::INVALID) {
        const auto& f = ctx.stack.at(cur);
        if (f.is_core && !ctx.show_core) {
            core_omitted++;
            cur = f.parent;
            continue;
        }
        if (core_omitted > 0) {
            std::println(std::cerr, "  ({} frames omitted)", core_omitted);
            core_omitted = 0;
        }
        auto r = resolve_names(f.expr, ctx.arena);
        std::println(
            std::cerr, "  in expansion of: {}", ctx.arena.dump(r.expr_ref()));
        cur = f.parent;
    }
    if (core_omitted > 0)
        std::println(std::cerr, "  ({} frames omitted)", core_omitted);
}

[[nodiscard]] static std::optional<std::unique_ptr<Transformer>>
parse_syntax_rules(SExprLocRef transformer_spec, ExpCtx& ctx,
    std::string_view form_prefix = "define-syntax") {
    if (!ctx.arena.at(transformer_spec).isa<SExprList>()) {
        report_syntax_error(
            std::string(form_prefix) + ": expected (syntax-rules ...)",
            transformer_spec, ctx);
        return std::nullopt;
    }
    const auto& spec_list
        = ctx.arena.at(transformer_spec).get<SExprList>()->get().elem;
    if (spec_list.empty()) {
        report_syntax_error(
            std::string(form_prefix) + ": expected (syntax-rules ...)",
            transformer_spec, ctx);
        return std::nullopt;
    }
    if (!ctx.arena.at(spec_list[0]).isa<LispIdent>()) {
        report_syntax_error(
            std::string(form_prefix) + ": expected syntax-rules keyword",
            spec_list[0], ctx);
        return std::nullopt;
    }
    if (ctx.arena.at(spec_list[0]).get<LispIdent>()->get().name
        != "syntax-rules") {
        report_syntax_error(
            std::string(form_prefix) + ": expected syntax-rules keyword",
            spec_list[0], ctx);
        return std::nullopt;
    }
    std::vector<std::string> literals;
    if (spec_list.size() >= 2) {
        if (ctx.arena.at(spec_list[1]).isa<SExprList>()) {
            const auto& lit_list
                = ctx.arena.at(spec_list[1]).get_unchecked<SExprList>().elem;
            for (const auto& lit : lit_list) {
                if (ctx.arena.at(lit).isa<LispNil>())
                    continue;
                if (ctx.arena.at(lit).isa<LispIdent>()) {
                    literals.push_back(
                        ctx.arena.at(lit).get_unchecked<LispIdent>().name);
                } else {
                    report_syntax_error(std::string(form_prefix)
                            + ": Invalid syntax-rule: not an identifier",
                        lit, ctx);
                }
            }
        } else {
            report_syntax_error(std::string(form_prefix)
                    + ": Invalid syntax-rule: capture list is not a list",
                spec_list[1], ctx);
            return std::nullopt;
        }
    }
    std::vector<Transformer::SyntaxRule> rules;
    for (std::size_t i = 2; i < spec_list.size(); ++i) {
        if (!ctx.arena.at(spec_list[i]).isa<SExprList>()) {
            if (i == spec_list.size() - 1
                && ctx.arena.at(spec_list[i]).isa<LispNil>())
                continue;
            report_syntax_error(
                std::string(form_prefix) + ": Invalid syntax-rule: not a list",
                spec_list[i], ctx);
            continue;
        }
        const auto& rule_parts
            = ctx.arena.at(spec_list[i]).get_unchecked<SExprList>().elem;
        if (rule_parts.size() == 3
            && ctx.arena.at(rule_parts[2]).isa<LispNil>())
            rules.push_back({ rule_parts[0], rule_parts[1] });
        else
            report_syntax_error(std::string(form_prefix)
                    + ": Invalid syntax-rule: unexpected rule format",
                spec_list[i], ctx);
    }
    // FIXME: transformer validity not checked
    return std::make_unique<Transformer>(
        std::move(rules), std::move(literals), ctx.arena);
}

static SExprLocRef expand_define_syntax(
    const SExprList& list, SExprLocRef root, ExpCtx ctx) {
    // (define-syntax name (syntax-rules (literals…) (pattern template) …))
    if (!ctx.is_top_level) {
        report_syntax_error(
            "define-syntax allowed only at top level", root, ctx);
        return root;
    }
    if (list.elem.size() < 3) {
        report_syntax_error(
            "define-syntax: bad syntax (missing name or transformer path)",
            root, ctx);
        return root;
    }

    auto name_ex = list.elem[1];
    auto transformer_spec = list.elem[2];

    if (!ctx.arena.at(name_ex).isa<LispIdent>()) {
        report_syntax_error(
            "define-syntax: expected identifier for macro name", name_ex, ctx);
        return root;
    }
    auto macro_name = ctx.arena.at(name_ex).get<LispIdent>()->get();

    auto transformer = parse_syntax_rules(transformer_spec, ctx);
    if (!transformer)
        return root;
    ctx.env.add_binding(macro_name,
        Binding(MacroBinding { .transformer = std::move(*transformer),
            .is_core = ctx.is_core,
            .output_excluded_scope = std::nullopt }));
    // FIXME: cleanup nils
    return ctx.arena.emplace(root.loc_ref(), LispNil());
}

static SExprLocRef expand_let_letrec_syntax(
    const SExprList& list, SExprLocRef root, ExpCtx ctx, bool is_letrec) {
    std::string let_syntax_name = is_letrec ? "letrec-syntax" : "let-syntax";
    // (let-syntax ((name transformer) ...) body ...)
    if (list.elem.size() < 3) {
        report_syntax_error(
            let_syntax_name + ": bad syntax (missing bindings or body)", root,
            ctx);
        return root;
    }

    const auto& bindings_expr = ctx.arena.at(list.elem[1]);
    if (!bindings_expr.isa<SExprList>()) {
        report_syntax_error(
            let_syntax_name + ": expected list of bindings", list.elem[1], ctx);
        return root;
    }

    ScopeID scope = ctx.env.new_scope();
    const auto& bindings = bindings_expr.get_unchecked<SExprList>().elem;

    for (const auto& binding : bindings) {
        const auto& binding_expr = ctx.arena.at(binding);
        if (binding_expr.isa<LispNil>())
            continue;
        if (!binding_expr.isa<SExprList>()) {
            report_syntax_error(let_syntax_name
                    + ": expected (name transformer) for each binding",
                binding, ctx);
            return root;
        }
        const auto& pair = binding_expr.get_unchecked<SExprList>().elem;
        if (pair.size() < 2) {
            report_syntax_error(let_syntax_name
                    + ": expected (name transformer) for each binding",
                binding, ctx);
            return root;
        }

        auto name_ex = pair[0];
        if (!ctx.arena.at(name_ex).isa<LispIdent>()) {
            report_syntax_error(
                "let-syntax: expected identifier for macro name", name_ex, ctx);
            return root;
        }
        auto macro_name = ctx.arena.at(name_ex).get_unchecked<LispIdent>().name;
        auto scoped_name = add_scope(name_ex, scope, ctx.arena);
        auto macro_ident = ctx.arena.at(scoped_name).get_unchecked<LispIdent>();

        auto transformer = parse_syntax_rules(pair[1], ctx, "let-syntax");
        if (!transformer)
            return root;

        ctx.env.add_binding(macro_ident,
            Binding(MacroBinding { .transformer = std::move(*transformer),
                .is_core = ctx.is_core,
                .output_excluded_scope
                = is_letrec ? std::nullopt : std::optional(scope) }));
    }

    std::vector<SExprLocRef> out;
    out.push_back(make_canonical(list.elem[0].loc_ref(), "begin", ctx.arena));
    for (std::size_t i = 2; i < list.elem.size(); ++i) {
        auto body_ctx = ctx;
        body_ctx.is_top_level = false;
        auto r = expand(add_scope(list.elem[i], scope, ctx.arena), body_ctx);
        if (!r.is_valid())
            return r;
        out.push_back(r);
    }
    return ctx.arena.emplace(root.loc_ref(), SExprList(std::move(out)));
}

static void report_expansion_error(
    SExprLocRef failed_expr, ExpCtx ctx, std::string_view msg) {

    auto resolved = resolve_names(failed_expr, ctx.arena);
    auto failed_str = ctx.arena.dump(resolved.expr_ref());

    Error("macro expansion failed: {}", msg);
    std::println(std::cerr, "  for: {}", failed_str);

    auto cur = ctx.parent;
    int core_omitted = 0;
    while (cur != ExpansionStack::INVALID) {
        const auto& f = ctx.stack.at(cur);
        if (f.is_core && !ctx.show_core) {
            core_omitted++;
            cur = f.parent;
            continue;
        }
        if (core_omitted > 0) {
            std::println(std::cerr, "  ({} frames omitted)", core_omitted);
            core_omitted = 0;
        }
        auto r = resolve_names(f.expr, ctx.arena);
        std::println(
            std::cerr, "  in expansion of: {}", ctx.arena.dump(r.expr_ref()));
        cur = f.parent;
    }
    if (core_omitted > 0)
        std::println(std::cerr, "  ({} frames omitted)", core_omitted);

    auto loc = ctx.arena.location(failed_expr.loc_ref());
    std::println(std::cerr, "  at {}", loc.source_location());
}

static SExprLocRef expand_macro(
    SExprLocRef root, const MacroBinding& macro, ExpCtx ctx) {
    ScopeID intro = ctx.env.new_scope();
    auto scoped_in = add_scope(root, intro, ctx.arena);
    auto result = macro.transformer->transcribe(scoped_in, ctx.arena);
    if (!result.is_valid()) {
        report_expansion_error(root, ctx, "no syntax-rules pattern matched");
        return root;
    }
    auto new_ctx = ctx;
    new_ctx.is_core = macro.is_core;
    new_ctx.output_excluded_scope = macro.output_excluded_scope;
    return expand(add_scope(result, intro, ctx.arena), new_ctx);
}

[[nodiscard]] static SExprLocRef expand(SExprLocRef root, ExpCtx ctx) {
    if (!root.is_valid())
        return root;

    const auto& sexpr = ctx.arena.at(root);

    if (sexpr.isa<LispIdent>()) {
        const auto& ident = sexpr.get_unchecked<LispIdent>();
        auto binding = ctx.env.find_binding(ident, ctx.output_excluded_scope);
        if (!binding) {
            auto clean = ident;
            clean.scopes.clear();
            return ctx.arena.emplace(root.loc_ref(), std::move(clean));
        }
        if (binding->get().isa<VarBinding>())
            return ctx.arena.emplace(
                root.loc_ref(), binding->get().get_unchecked<VarBinding>().id);
        return root;
    }

    if (sexpr.isa<SExprList>()) {
        auto list = sexpr.get_unchecked<SExprList>();
        if (list.elem.empty())
            return root;

        const auto& head_expr = ctx.arena.at(list.elem[0]);
        if (head_expr.isa<LispIdent>()) {
            auto head_id = head_expr.get_unchecked<LispIdent>();
            auto binding
                = ctx.env.find_binding(head_id, ctx.output_excluded_scope);

            if (binding && binding->get().isa<CoreBinding>()) {
                const auto& name = head_id.name;
                if (name == "lambda")
                    return expand_lambda(list, root, ctx);
                if (name == "quote")
                    return expand_quote(list, root, ctx.arena);
                if (name == "if")
                    return expand_simple(name, list, root, ctx);
                if (name == "set!")
                    return expand_set(list, root, ctx);
                if (name == "define")
                    return expand_define(list, root, ctx);
                if (name == "define-syntax")
                    return expand_define_syntax(list, root, ctx);
                if (name == "let-syntax")
                    return expand_let_letrec_syntax(list, root, ctx, false);
                if (name == "letrec-syntax")
                    return expand_let_letrec_syntax(list, root, ctx, true);
                if (name == "syntax-error") {
                    std::string msg;
                    if (list.elem.size() >= 3) {
                        auto msg_expr = ctx.arena.at(list.elem[2]);
                        if (msg_expr.isa<LispString>()) {
                            msg = msg_expr.get_unchecked<LispString>();
                        }
                    }
                    report_expansion_error(root, ctx, msg);
                    return root;
                }
            }

            if (binding && binding->get().isa<MacroBinding>()) {
                bool is_core
                    = binding->get().get_unchecked<MacroBinding>().is_core;
                ExpStackRef frame = ctx.stack.push(root, is_core, ctx.parent);
                auto macro_ctx = ctx;
                macro_ctx.parent = frame;
                return expand_macro(
                    root, binding->get().get<MacroBinding>()->get(), macro_ctx);
            }
        }

        std::vector<SExprLocRef> out;
        out.reserve(list.elem.size());
        for (const auto& el : list.elem) {
            auto sub_ctx = ctx;
            sub_ctx.is_top_level = false;
            auto r = expand(el, sub_ctx);
            if (!r.is_valid())
                return r;
            out.push_back(r);
        }
        return ctx.arena.emplace(root.loc_ref(), SExprList(std::move(out)));
    }

    return root;
}

#include "../core.scm"

void ExpandPass::load_core(SExprArena& /* user_arena */) {
    Lexer lexer("<core>", CORE_SOURCE);
    if (lexer.is_failed())
        return;

    Parser parser(lexer.tokens(), lexer.loc_arena());
    if (parser.is_failed())
        return;

    auto core_root = parser.root();
    _core_arena = std::make_unique<SExprArena>(std::move(parser.arena()));

    const auto& root_expr = _core_arena->at(core_root);
    if (root_expr.isa<SExprList>()) {
        bool dummy_error = false;
        for (const auto& form : root_expr.get_unchecked<SExprList>().elem) {
            ExpCtx ctx {
                .env = _env,
                .arena = *_core_arena,
                .stack = _exp_stack,
                .parent = ExpansionStack::INVALID,
                .is_core = true,
                .show_core = _show_core_expansion,
                .is_top_level = true,
                .had_error = dummy_error,
                .output_excluded_scope = std::nullopt,
            };
            (void)expand(form, ctx);
        }
    }
    _core_loaded = true;
}

[[nodiscard]] SExprLocRef ExpandPass::run(
    SExprLocRef root, SExprArena& arena) noexcept {
    if (!_core_loaded)
        load_core(arena);

    _had_error = false;
    ExpCtx ctx {
        .env = _env,
        .arena = arena,
        .stack = _exp_stack,
        .parent = ExpansionStack::INVALID,
        .is_core = false,
        .show_core = _show_core_expansion,
        .is_top_level = true,
        .had_error = _had_error,
        .output_excluded_scope = std::nullopt,
    };

    if (arena.at(root).isa<SExprList>()) {
        const auto& list = arena.at(root).get_unchecked<SExprList>();
        std::vector<SExprLocRef> begin_flattened;
        auto is_begin = [&](SExprLocRef el) {
            if (auto list = arena[el].get<SExprList>())
                if (list->get().elem.size() > 0)
                    // FIXME: begin could be overrided somewhere
                    if (auto first
                        = arena[list->get().elem[0]].get<LispIdent>())
                        if (first->get().name == "begin")
                            return true;
            return false;
        };

        const std::function<void(SExprLocRef, std::vector<SExprLocRef>&)>
            expand_begin = [&](SExprLocRef el, std::vector<SExprLocRef>& out) {
                auto list = arena[el].get_unchecked<SExprList>();
                auto list_it = list.elem.begin() + 1;
                // FIXME: should check for improper list here
                auto list_end = list.elem.end() - 1;
                while (list_it != list_end) {
                    if (is_begin(*list_it)) {
                        expand_begin(*list_it, out);
                    } else
                        out.push_back(*list_it);
                    ++list_it;
                }
            };

        for (auto el : list.elem) {
            if (is_begin(el)) {
                expand_begin(el, begin_flattened);
            } else {
                begin_flattened.push_back(el);
            }
        }

        std::vector<SExprLocRef> out;
        out.reserve(begin_flattened.size());
        for (const auto& el : begin_flattened) {
            auto r = expand(el, ctx);
            if (!r.is_valid()) {
                _had_error = true;
                return SExprLocRef::invalid();
            }
            out.push_back(r);
        }
        auto expanded
            = arena.emplace(root.loc_ref(), SExprList(std::move(out)));
        if (_had_error)
            return SExprLocRef::invalid();
        return resolve_names(expanded, arena);
    }
    __builtin_unreachable();
}

ExpandPass::ExpandPass(bool show_core_expansion) noexcept
    : _show_core_expansion(show_core_expansion) {
    _env.define_core_syntax("lambda");
    _env.define_core_syntax("quote");
    _env.define_core_syntax("if");
    _env.define_core_syntax("set!");
    // _env.define_core_syntax("begin");
    _env.define_core_syntax("define");
    _env.define_core_syntax("define-syntax");
    _env.define_core_syntax("let-syntax");
    _env.define_core_syntax("letrec-syntax");
    _env.define_core_syntax("syntax-error");
}

} // namespace lpc::frontend
