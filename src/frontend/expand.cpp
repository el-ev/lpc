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
    std::uint32_t max_depth;
    std::uint32_t current_depth;
    // FIXME: this is just stupid
    // TODO: get rid of this
    std::optional<ScopeID> output_excluded_scope;
};

[[nodiscard]] static SExprLocRef add_scope(
    SExprLocRef expr, ScopeID scope, SExprArena& arena);

template <typename F>
SExprLocRef transform_sexpr(SExprLocRef expr, SExprArena& arena, F&& f) {
    if (!expr.is_valid())
        return expr;

    const auto& sexpr = arena.at(expr);

    SExprLocRef new_expr = expr;

    if (sexpr.isa<SExprList>()) {
        auto elems = sexpr.get_unchecked<SExprList>().elem; // copy
        std::vector<SExprLocRef> v;
        v.reserve(elems.size());
        for (const auto& el : elems)
            v.push_back(transform_sexpr(el, arena, std::forward<F>(f)));
        new_expr = arena.emplace(expr.loc_ref(), SExprList(std::move(v)));
    } else if (sexpr.isa<SExprVector>()) {
        auto elems = sexpr.get_unchecked<SExprVector>().elem; // copy
        std::vector<SExprLocRef> v;
        v.reserve(elems.size());
        for (const auto& el : elems)
            v.push_back(transform_sexpr(el, arena, std::forward<F>(f)));
        new_expr = arena.emplace(expr.loc_ref(), SExprVector(std::move(v)));
    }

    return std::forward<F>(f)(new_expr, arena);
}

[[nodiscard]] static SExprLocRef add_scope(
    SExprLocRef expr, ScopeID scope, SExprArena& arena) {
    return transform_sexpr(expr, arena, [scope](SExprLocRef e, SExprArena& a) {
        const auto& sexpr = a.at(e);
        if (sexpr.isa<LispIdent>()) {
            auto ident = sexpr.get_unchecked<LispIdent>();
            if (ident.scopes.contains(scope)) {
                ident.scopes.erase(scope);
            } else {
                ident.scopes.insert(scope);
            }
            return a.emplace(e.loc_ref(), std::move(ident));
        }
        return e;
    });
}

struct ScopeKey {
    std::string name;
    std::set<ScopeID> scopes;
    auto operator<=>(const ScopeKey&) const = default;
};

[[nodiscard]] static SExprLocRef make_canonical(
    LocRef loc, const std::string& name, SExprArena& arena) {
    return arena.emplace(loc, LispIdent(name));
}

// TODO: better?
[[nodiscard]] static std::pair<std::size_t, std::size_t> find_cycle_suffix(
    std::span<const std::string> seq) {
    const auto n = seq.size();
    if (n < 2)
        return { 0, 0 };
    for (std::size_t L = 1; L <= n / 2; ++L) {
        const auto pattern = std::span(seq).last(L);
        std::size_t k = 1;
        for (std::size_t j = 1; j * L < n; ++j) {
            const auto start = n - ((j + 1) * L);
            if (start + L > n)
                break;
            if (!std::equal(seq.begin() + (std::size_t)start,
                    seq.begin() + (std::size_t)start + L, pattern.begin(),
                    pattern.end()))
                break;
            k = j + 1;
        }
        if (k >= 2)
            return { L, k };
    }
    return { 0, 0 };
}

static void flush_expansion_segment(std::vector<std::string>& segment) {
    if (segment.empty())
        return;
    const auto [cycle_len, num_reps] = find_cycle_suffix(segment);
    const std::size_t cycle_start = cycle_len > 0 && num_reps >= 2
        ? segment.size() - (cycle_len * num_reps)
        : segment.size();
    for (std::size_t i = 0; i < cycle_start;) {
        std::size_t run = 1;
        while (i + run < cycle_start && segment[i + run] == segment[i])
            ++run;
        std::println(std::cerr, "  in expansion of: {}", segment[i]);
        if (run > 1)
            std::println(std::cerr, "  ({} identical frames omitted)", run - 1);
        i += run;
    }
    if (cycle_len > 0 && num_reps >= 2) {
        for (std::size_t i = 0; i < cycle_len; ++i)
            std::println(
                std::cerr, "  in expansion of: {}", segment[cycle_start + i]);
        const auto to_omit = (num_reps - 1) * cycle_len;
        if (cycle_len == 1)
            std::println(std::cerr, "  ({} identical frames omitted)", to_omit);
        else
            std::println(std::cerr, "  ({} similar frames omitted)", to_omit);
    }
    segment.clear();
}

static void report_error(
    SExprLocRef failed_expr, ExpCtx ctx, std::string_view msg) {
    ctx.had_error = true;

    auto failed_str = ctx.arena.dump(failed_expr.expr_ref());

    Error("{}", msg);
    std::println(std::cerr, "  for: {}", failed_str);

    struct Frame {
        int core_omitted;
        std::string dump;
    };
    std::vector<Frame> frames;
    frames.reserve(64);
    int core_omitted = 0;
    auto cur = ctx.parent;
    while (cur != ExpansionStack::INVALID) {
        const auto& f = ctx.stack.at(cur);
        if (f.is_core && !ctx.show_core) {
            core_omitted++;
            cur = f.parent;
            continue;
        }
        frames.push_back({ core_omitted, ctx.arena.dump(f.expr.expr_ref()) });
        core_omitted = 0;
        cur = f.parent;
    }
    if (core_omitted > 0)
        std::println(std::cerr, "  ({} frames omitted)", core_omitted);

    std::vector<std::string> segment;
    for (const auto& [core, dump] : frames) {
        if (core > 0) {
            flush_expansion_segment(segment);
            std::println(std::cerr, "  ({} frames omitted)", core);
        }
        segment.push_back(dump);
    }
    flush_expansion_segment(segment);

    auto loc = ctx.arena.location(failed_expr.loc_ref());
    std::println(std::cerr, "  at {}", loc.source_location());
}

// include the indentifier itself in the count
[[nodiscard]] static bool check_arity(
    SExprLocRef el, ExpCtx ctx, std::size_t min_arity, std::size_t max_arity) {
    // max_arity == 0 means no maximum
    const auto& expr = ctx.arena.at(el);
    if (!expr.isa<SExprList>()) {
        return false;
    }
    const auto& list = expr.get_unchecked<SExprList>();
    if (list.elem.size() < min_arity + 1) {
        report_error(el, ctx,
            std::format("arity mismatch: expected{} {} arguments, got {}",
                max_arity == 0 ? " at least" : "", min_arity,
                list.elem.size() - 1));
        ctx.had_error = true;
        return false;
    }
    if (max_arity == min_arity && list.elem.size() != min_arity + 1) {
        report_error(el, ctx,
            std::format("arity mismatch: expected {} arguments, got {}",
                min_arity, list.elem.size()));
        ctx.had_error = true;
        return false;
    }
    if (max_arity != 0 && list.elem.size() > max_arity + 1) {
        report_error(el, ctx,
            std::format("arity mismatch: expected at most {} arguments, got {}",
                max_arity, list.elem.size()));
        ctx.had_error = true;
        return false;
    }
    auto tail = ctx.arena[list.elem.back()];
    if (!tail.isa<LispNil>()) {
        report_error(el, ctx, "arity mismatch: improper list");
        ctx.had_error = true;
        return false;
    }
    return true;
}

[[nodiscard]] static std::vector<SExprLocRef> expand(
    SExprLocRef root, ExpCtx ctx);

static bool is_identifier_active(const LispIdent& id, const LexEnv& env,
    ExpStackRef parent, const ExpansionStack& stack, SExprArena& arena) {
    auto id_binding = env.find_binding(id);
    if (!id_binding)
        return false;

    auto matches = [&](SExprLocRef k) {
        if (!k.is_valid())
            return false;
        const auto& k_expr = arena.at(k);
        if (k_expr.isa<SExprList>()) {
            const auto& list = k_expr.get_unchecked<SExprList>();
            if (!list.elem.empty()) {
                const auto& head = arena.at(list.elem[0]);
                if (head.isa<LispIdent>()) {
                    auto head_binding
                        = env.find_binding(head.get_unchecked<LispIdent>());
                    if (!head_binding)
                        return false;
                    return &head_binding->get() == &id_binding->get();
                }
            }
        }
        return false;
    };

    auto cur = parent;
    while (cur != ExpansionStack::INVALID) {
        const auto& frame = stack.at(cur);
        if (matches(frame.expr))
            return true;
        cur = frame.parent;
    }
    return false;
}

static std::vector<SExprLocRef> expand_lambda(
    const SExprList& list, SExprLocRef root, ExpCtx ctx) {
    // (lambda formals body...)
    if (!check_arity(root, ctx, 2, 0))
        return { SExprLocRef::invalid() };

    ScopeID scope = ctx.env.new_scope();
    auto scoped_params = add_scope(list.elem[1], scope, ctx.arena);

    std::vector<SExprLocRef> params;
    auto bind = [&](SExprLocRef p) {
        if (ctx.arena.at(p).isa<LispIdent>()) {
            auto id = ctx.arena.at(p).get_unchecked<LispIdent>();
            auto resolved = ctx.env.unique_name(id.name);
            auto resolved_id = LispIdent(resolved);
            ctx.env.add_binding(id, Binding(VarBinding(resolved_id)));
            params.push_back(
                ctx.arena.emplace(p.loc_ref(), std::move(resolved_id)));
        } else if (ctx.arena.at(p).isa<LispNil>()) {
            params.push_back(p);
        }
    };

    const auto& p_expr = ctx.arena.at(scoped_params);
    if (p_expr.isa<SExprList>()) {
        // FIXME: check for
        // 1) duplicate parameters
        // 2) All indents, last one is Nil or parameter
        for (const auto& p : p_expr.get_unchecked<SExprList>().elem)
            bind(p);
    } else if (p_expr.isa<LispIdent>()) {
        bind(scoped_params);
        // canonicalize to list of identifiers
        // TODO is this correct?
    } else {
        report_error(scoped_params, ctx, "lambda: expected list of identifier");
        return { SExprLocRef::invalid() };
    }
    auto final_params = ctx.arena.emplace(
        scoped_params.loc_ref(), SExprList(std::move(params)));

    std::vector<SExprLocRef> out;
    out.push_back(make_canonical(list.elem[0].loc_ref(), "lambda", ctx.arena));
    out.push_back(final_params);
    for (std::size_t i = 2; i < list.elem.size(); ++i) {
        auto body_ctx = ctx;
        body_ctx.is_top_level = false;
        auto r = expand(add_scope(list.elem[i], scope, ctx.arena), body_ctx);
        if (r.size() != 1 || !r[0].is_valid())
            return { SExprLocRef::invalid() };
        out.insert(out.end(), r.begin(), r.end());
    }
    return { ctx.arena.emplace(root.loc_ref(), SExprList(std::move(out))) };
}

static std::vector<SExprLocRef> expand_quote(
    const SExprList& list, SExprLocRef root, ExpCtx ctx) {
    if (!check_arity(root, ctx, 2, 2))
        return { SExprLocRef::invalid() };
    std::vector<SExprLocRef> out;
    out.push_back(make_canonical(list.elem[0].loc_ref(), "quote", ctx.arena));
    out.push_back(list.elem[1]);
    out.push_back(ctx.arena.nil(root.loc_ref()));
    return { ctx.arena.emplace(root.loc_ref(), SExprList(std::move(out))) };
}

static std::vector<SExprLocRef> expand_if(
    const SExprList& list, SExprLocRef root, ExpCtx ctx) {
    // (if condition then-clause else-clause?)
    if (!check_arity(root, ctx, 3, 4))
        return { SExprLocRef::invalid() };

    std::vector<SExprLocRef> out;
    out.push_back(make_canonical(list.elem[0].loc_ref(), "if", ctx.arena));
    for (std::size_t i = 1; i < list.elem.size(); ++i) {
        auto sub_ctx = ctx;
        sub_ctx.is_top_level = false;
        auto r = expand(list.elem[i], sub_ctx);
        if (r.size() != 1 || !r[0].is_valid())
            return { SExprLocRef::invalid() };
        out.push_back(r[0]);
    }
    return { ctx.arena.emplace(root.loc_ref(), SExprList(std::move(out))) };
}

static std::vector<SExprLocRef> expand_begin(
    const SExprList& list, SExprLocRef root, ExpCtx ctx) {
    if (!check_arity(root, ctx, 1, 0))
        return { SExprLocRef::invalid() };

    if (!ctx.is_top_level) {
        // call macro __begin
        auto new_list = list;
        new_list.elem[0] = make_canonical(root.loc_ref(), "__begin", ctx.arena);
        auto ref
            = ctx.arena.emplace(root.loc_ref(), SExprList(std::move(new_list)));
        auto r = expand(ref, ctx);
        if (r.size() != 1 || !r[0].is_valid())
            return { SExprLocRef::invalid() };
        return { r[0] };
    }

    std::vector<SExprLocRef> out;
    for (std::size_t i = 1; i < list.elem.size(); ++i) {
        auto r = expand(list.elem[i], ctx);
        if (std::ranges::any_of(r, [](const auto& r) { return !r.is_valid(); }))
            return { SExprLocRef::invalid() };
        out.insert(out.end(), r.begin(), r.end());
    }
    return out;
}

static std::vector<SExprLocRef> expand_set(
    const SExprList& list, SExprLocRef root, ExpCtx ctx) {
    // (set! variable expression)
    if (!check_arity(root, ctx, 3, 3))
        return { SExprLocRef::invalid() };

    std::vector<SExprLocRef> out;
    out.push_back(make_canonical(list.elem[0].loc_ref(), "set!", ctx.arena));
    for (std::size_t i = 1; i < list.elem.size(); ++i) {
        auto val_ctx = ctx;
        val_ctx.is_top_level = false;
        auto r = expand(list.elem[i], val_ctx);
        if (r.size() != 1 || !r[0].is_valid())
            return { SExprLocRef::invalid() };
        out.push_back(r[0]);
    }
    return { ctx.arena.emplace(root.loc_ref(), SExprList(std::move(out))) };
}

static std::vector<SExprLocRef> expand_define(
    const SExprList& list, SExprLocRef root, ExpCtx ctx) {
    if (!check_arity(root, ctx, 3, 0))
        return { SExprLocRef::invalid() };

    auto var = list.elem[1];

    // TODO canonicalize to lambda form
    if (ctx.arena.at(var).isa<SExprList>()) {
        const auto& var_list
            = ctx.arena.at(var).get_unchecked<SExprList>().elem;
        if (var_list.empty())
            return { SExprLocRef::invalid() };

        auto func_name = var_list[0];

        if (!ctx.arena.at(func_name).isa<LispIdent>()) {
            report_error(func_name, ctx,
                "define: expected identifier for function name");
            return { SExprLocRef::invalid() };
        }
        auto func_name_id = ctx.arena.at(func_name).get_unchecked<LispIdent>();
        auto resolved = ctx.env.unique_name(func_name_id.name);
        auto resolved_id = LispIdent(resolved);
        ctx.env.add_binding(func_name_id, Binding(VarBinding(resolved_id)));
        auto resolved_func_name
            = ctx.arena.emplace(func_name.loc_ref(), std::move(resolved_id));

        std::vector<SExprLocRef> params;
        std::size_t var_logical = var_list.size();
        if (!var_list.empty() && ctx.arena.at(var_list.back()).isa<LispNil>())
            var_logical--;
        for (std::size_t i = 1; i < var_logical; ++i)
            params.push_back(var_list[i]);
        params.push_back(ctx.arena.nil(var.loc_ref()));
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
        def.push_back(resolved_func_name);
        def.push_back(lam_node);
        def.push_back(ctx.arena.nil(root.loc_ref()));
        auto desugared
            = ctx.arena.emplace(root.loc_ref(), SExprList(std::move(def)));
        return { expand(desugared, ctx) };
    }

    // (define var expr)
    if (ctx.arena.at(var).isa<LispIdent>()) {
        auto id = ctx.arena.at(var).get_unchecked<LispIdent>();
        if (is_identifier_active(
                id, ctx.env, ctx.parent, ctx.stack, ctx.arena)) {
            report_error(root, ctx, "define: invalid context for definition");
            return { SExprLocRef::invalid() };
        }

        const auto exact = ctx.env.find_exact_binding(id);
        if (exact && exact->get().isa<VarBinding>()) {
            var = ctx.arena.emplace(
                var.loc_ref(), exact->get().get_unchecked<VarBinding>().id);
        } else {
            auto resolved = ctx.env.unique_name(id.name);
            auto resolved_id = LispIdent(resolved);
            ctx.env.add_binding(id, Binding(VarBinding(resolved_id)));
            var = ctx.arena.emplace(var.loc_ref(), std::move(resolved_id));
        }
    }

    std::vector<SExprLocRef> out;
    out.push_back(make_canonical(list.elem[0].loc_ref(), "define", ctx.arena));
    out.push_back(var);
    for (std::size_t i = 2; i < list.elem.size(); ++i) {
        auto val_ctx = ctx;
        val_ctx.is_top_level = false;
        auto r = expand(list.elem[i], val_ctx);
        if (r.size() != 1 || !r[0].is_valid())
            return { SExprLocRef::invalid() };
        out.push_back(r[0]);
    }
    return { ctx.arena.emplace(root.loc_ref(), SExprList(std::move(out))) };
}

[[nodiscard]] static std::optional<std::unique_ptr<Transformer>>
parse_syntax_rules(SExprLocRef transformer_spec, ExpCtx& ctx,
    std::string_view form_prefix = "define-syntax") {
    if (!ctx.arena.at(transformer_spec).isa<SExprList>()) {
        report_error(transformer_spec, ctx,
            std::format("{}: expected (syntax-rules ...)", form_prefix));
        return std::nullopt;
    }
    const auto& spec_list
        = ctx.arena.at(transformer_spec).get<SExprList>()->get().elem;
    if (spec_list.empty()) {
        report_error(transformer_spec, ctx,
            std::format("{}: expected (syntax-rules ...)", form_prefix));
        return std::nullopt;
    }
    if (!ctx.arena.at(spec_list[0]).isa<LispIdent>()) {
        report_error(transformer_spec, ctx,
            std::format("{}: expected syntax-rules keyword", form_prefix));
        return std::nullopt;
    }
    if (ctx.arena.at(spec_list[0]).get<LispIdent>()->get().name
        != "syntax-rules") {
        report_error(transformer_spec, ctx,
            std::format("{}: expected syntax-rules keyword", form_prefix));
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
                    report_error(lit, ctx,
                        std::format(
                            "{}: invalid syntax-rule: not an identifier",
                            form_prefix));
                }
            }
        } else {
            report_error(transformer_spec, ctx,
                std::format(
                    "{}: invalid syntax-rule: capture list is not a list",
                    form_prefix));
            return std::nullopt;
        }
    }
    std::vector<Transformer::SyntaxRule> rules;
    for (std::size_t i = 2; i < spec_list.size(); ++i) {
        if (!ctx.arena.at(spec_list[i]).isa<SExprList>()) {
            if (i == spec_list.size() - 1
                && ctx.arena.at(spec_list[i]).isa<LispNil>())
                continue;
            report_error(transformer_spec, ctx,
                std::format(
                    "{}: invalid syntax-rule: not a list", form_prefix));
            continue;
        }
        const auto& rule_parts
            = ctx.arena.at(spec_list[i]).get_unchecked<SExprList>().elem;
        if (rule_parts.size() == 3
            && ctx.arena.at(rule_parts[2]).isa<LispNil>())
            rules.push_back({ rule_parts[0], rule_parts[1] });
        else
            report_error(transformer_spec, ctx,
                std::format("{}: invalid syntax-rule: unexpected rule format",
                    form_prefix));
    }
    return std::make_unique<Transformer>(
        std::move(rules), std::move(literals), ctx.arena);
}

static std::vector<SExprLocRef> expand_define_syntax(
    const SExprList& list, SExprLocRef root, ExpCtx ctx) {
    // (define-syntax name (syntax-rules (literals…) (pattern template) …))
    if (!ctx.is_top_level) {
        report_error(root, ctx, "define-syntax allowed only at top level");
        return { SExprLocRef::invalid() };
    }
    if (list.elem.size() < 3) {
        report_error(
            root, ctx, "define-syntax: missing name or transformer path");
        return { SExprLocRef::invalid() };
    }

    auto name_ex = list.elem[1];
    auto transformer_spec = list.elem[2];

    if (!ctx.arena.at(name_ex).isa<LispIdent>()) {
        report_error(
            name_ex, ctx, "define-syntax: expected identifier for macro name");
        return { SExprLocRef::invalid() };
    }
    auto macro_name = ctx.arena.at(name_ex).get<LispIdent>()->get();

    if (is_identifier_active(
            macro_name, ctx.env, ctx.parent, ctx.stack, ctx.arena)) {
        report_error(
            root, ctx, "define-syntax: invalid context for definition");
        return { SExprLocRef::invalid() };
    }

    auto transformer = parse_syntax_rules(transformer_spec, ctx);
    if (!transformer)
        return { SExprLocRef::invalid() };
    ctx.env.add_binding(macro_name,
        Binding(MacroBinding { .transformer = std::move(*transformer),
            .is_core = ctx.is_core,
            .output_excluded_scope = std::nullopt }));
    return {};
}

static std::vector<SExprLocRef> expand_let_letrec_syntax(
    const SExprList& list, SExprLocRef root, ExpCtx ctx, bool is_letrec) {
    std::string let_syntax_name = is_letrec ? "letrec-syntax" : "let-syntax";
    // (let-syntax ((name transformer) ...) body ...)
    if (list.elem.size() < 3) {
        report_error(root, ctx,
            std::format("{}: missing bindings or body", let_syntax_name));
        return { SExprLocRef::invalid() };
    }

    const auto& bindings_expr = ctx.arena.at(list.elem[1]);
    if (!bindings_expr.isa<SExprList>()) {
        report_error(list.elem[1], ctx,
            std::format("{}: expected list of bindings", let_syntax_name));
        return { SExprLocRef::invalid() };
    }

    ScopeID scope = ctx.env.new_scope();
    auto bindings = bindings_expr.get_unchecked<SExprList>().elem;

    for (const auto& binding : bindings) {
        const auto& binding_expr = ctx.arena.at(binding);
        if (binding_expr.isa<LispNil>())
            continue;
        if (!binding_expr.isa<SExprList>()) {
            report_error(binding, ctx,
                std::format("{}: expected (name transformer) for each binding",
                    let_syntax_name));
            return { SExprLocRef::invalid() };
        }
        auto pair = binding_expr.get_unchecked<SExprList>().elem;
        if (pair.size() < 2) {
            report_error(binding, ctx,
                std::format("{}: expected (name transformer) for each binding",
                    let_syntax_name));
            return { SExprLocRef::invalid() };
        }

        auto name_ex = pair[0];
        if (!ctx.arena.at(name_ex).isa<LispIdent>()) {
            report_error(
                name_ex, ctx, "let-syntax: expected identifier for macro name");
            return { SExprLocRef::invalid() };
        }
        auto macro_name = ctx.arena.at(name_ex).get_unchecked<LispIdent>().name;
        auto scoped_name = add_scope(name_ex, scope, ctx.arena);
        auto macro_ident = ctx.arena.at(scoped_name).get_unchecked<LispIdent>();

        auto transformer = parse_syntax_rules(pair[1], ctx, is_letrec? "letrec-syntax" : "let-syntax");
        if (!transformer)
            return { SExprLocRef::invalid() };

        ctx.env.add_binding(macro_ident,
            Binding(MacroBinding { .transformer = std::move(*transformer),
                .is_core = ctx.is_core,
                .output_excluded_scope
                = is_letrec ? std::nullopt : std::optional(scope) }));
    }

    std::vector<SExprLocRef> out;
    out.push_back(make_canonical(list.elem[0].loc_ref(), "begin", ctx.arena));
    for (std::size_t i = 2; i < list.elem.size(); ++i) {
        out.push_back(list.elem[i]);
    }
    auto new_ctx = ctx;
    auto new_out = ctx.arena.emplace(list.elem[0].loc_ref(), SExprList(std::move(out)));
    return expand(add_scope(new_out, scope, ctx.arena), new_ctx);
}

static std::vector<SExprLocRef> expand_macro(
    SExprLocRef root, const MacroBinding& macro, ExpCtx ctx) {
    ScopeID intro = ctx.env.new_scope();
    auto scoped_in = add_scope(root, intro, ctx.arena);
    auto result = macro.transformer->transcribe(scoped_in, ctx.arena);
    if (!result.is_valid()) {
        report_error(root, ctx,
            "macro expansion failed: no syntax-rules pattern matched");
        return { SExprLocRef::invalid() };
    }
    auto new_ctx = ctx;
    new_ctx.is_core = macro.is_core;
    new_ctx.output_excluded_scope = macro.output_excluded_scope;
    auto r =  expand(add_scope(result, intro, ctx.arena), new_ctx);
    return r;
}

[[nodiscard]] static std::vector<SExprLocRef> expand(
    SExprLocRef root, ExpCtx ctx) {
    if (!root.is_valid())
        return { SExprLocRef::invalid() };

    if (ctx.current_depth > ctx.max_depth) {
        report_error(root, ctx,
            "expand: maximum macro expansion depth exceeded (possible infinite "
            "recursion)");
        return { SExprLocRef::invalid() };
    }

    const auto& sexpr = ctx.arena.at(root);

    if (sexpr.isa<LispIdent>()) {
        const auto& ident = sexpr.get_unchecked<LispIdent>();
        auto binding = ctx.env.find_binding(ident, ctx.output_excluded_scope);
        if (!binding) {
            auto clean = ident;
            clean.scopes.clear();
            return { ctx.arena.emplace(root.loc_ref(), std::move(clean)) };
        }
        if (binding->get().isa<VarBinding>())
            return { ctx.arena.emplace(root.loc_ref(),
                binding->get().get_unchecked<VarBinding>().id) };
        return { root };
    }

    if (sexpr.isa<SExprList>()) {
        auto list = sexpr.get_unchecked<SExprList>();
        if (list.elem.empty())
            return { root };

        const auto& head_expr = ctx.arena.at(list.elem[0]);
        if (head_expr.isa<LispIdent>()) {
            auto head_id = head_expr.get_unchecked<LispIdent>();
            auto binding
                = ctx.env.find_binding(head_id, ctx.output_excluded_scope);

            if (binding && binding->get().isa<CoreBinding>()) {
                const auto& name = head_id.name;

                ExpStackRef frame = ctx.stack.push(root, true, ctx.parent);
                auto core_ctx = ctx;
                core_ctx.parent = frame;

                if (name == "lambda")
                    return expand_lambda(list, root, core_ctx);
                if (name == "quote")
                    return expand_quote(list, root, core_ctx);
                if (name == "if")
                    return expand_if(list, root, core_ctx);
                if (name == "begin")
                    return expand_begin(list, root, core_ctx);
                if (name == "set!")
                    return expand_set(list, root, core_ctx);
                if (name == "define")
                    return expand_define(list, root, core_ctx);
                if (name == "define-syntax")
                    return expand_define_syntax(list, root, core_ctx);
                if (name == "let-syntax")
                    return expand_let_letrec_syntax(
                        list, root, core_ctx, false);
                if (name == "letrec-syntax")
                    return expand_let_letrec_syntax(list, root, core_ctx, true);
                if (name == "syntax-error") {
                    std::string msg;
                    if (list.elem.size() >= 3) {
                        auto msg_expr = ctx.arena.at(list.elem[1]);
                        if (msg_expr.isa<LispString>()) {
                            msg = msg_expr.get_unchecked<LispString>();
                        }
                    }
                    report_error(
                        root, core_ctx, std::format("syntax-error: {}", msg));
                    return { SExprLocRef::invalid() };
                }
            }

            if (binding && binding->get().isa<MacroBinding>()) {
                bool is_core
                    = binding->get().get_unchecked<MacroBinding>().is_core;
                ExpStackRef frame = ctx.stack.push(root, is_core, ctx.parent);
                auto macro_ctx = ctx;
                macro_ctx.parent = frame;
                macro_ctx.current_depth++;
                return expand_macro(
                    root, binding->get().get<MacroBinding>()->get(), macro_ctx);
            }
        }

        std::vector<SExprLocRef> out;
        out.reserve(list.elem.size());
        for (const auto& el : list.elem) {
            auto sub_ctx = ctx;
            auto r = expand(el, sub_ctx);
            if (std::ranges::any_of(
                    r, [](const auto& r) { return !r.is_valid(); })) {
                // return { SExprLocRef::invalid() };
                ctx.had_error = true;
                continue;
            }
            out.insert(out.end(), r.begin(), r.end());
        }
        return { ctx.arena.emplace(root.loc_ref(), SExprList(std::move(out))) };
    }

    return { root };
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
                .max_depth = _max_expansion_depth,
                .current_depth = 0,
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
        .max_depth = _max_expansion_depth,
        .current_depth = 0,
        .output_excluded_scope = std::nullopt,
    };

    if (arena.at(root).isa<SExprList>()) {
        const auto& list = arena.at(root).get_unchecked<SExprList>();
        std::vector<SExprLocRef> out;
        out.reserve(list.elem.size());
        for (const auto& el : list.elem) {
            auto r = expand(el, ctx);
            if (std::ranges::any_of(
                    r, [](const auto& r) { return !r.is_valid(); })) {
                _had_error = true;
                // We try to report all errors here
                // return { SExprLocRef::invalid() };
                continue;
            }
            out.insert(out.end(), r.begin(), r.end());
        }
        auto expanded
            = arena.emplace(root.loc_ref(), SExprList(std::move(out)));
        if (_had_error)
            return SExprLocRef::invalid();
        return expanded;
    }
    __builtin_unreachable();
}

ExpandPass::ExpandPass(
    bool show_core_expansion, std::uint32_t max_expansion_depth) noexcept
    : _show_core_expansion(show_core_expansion)
    , _max_expansion_depth(max_expansion_depth) {
    _env.define_core_syntax("lambda");
    _env.define_core_syntax("quote");
    _env.define_core_syntax("if");
    _env.define_core_syntax("set!");
    _env.define_core_syntax("begin");
    _env.define_core_syntax("define");
    _env.define_core_syntax("define-syntax");
    _env.define_core_syntax("let-syntax");
    _env.define_core_syntax("letrec-syntax");
    _env.define_core_syntax("syntax-error");
}

} // namespace lpc::frontend
