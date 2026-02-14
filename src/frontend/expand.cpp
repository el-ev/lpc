module lpc.frontend.expand;

import std;
import lpc.utils.logging;
import lpc.frontend.lexer;
import lpc.frontend.syntax;

namespace lpc::frontend {

using lpc::utils::Error;

template <typename F>
SExprLocRef transform_sexpr(SExprLocRef expr, SExprArena& arena, F&& f) {
    if (!expr.is_valid())
        return expr;

    const auto& sexpr = arena.at(expr);

    SExprLocRef new_expr = expr;

    if (sexpr.isa<SExprList>()) {
        auto elems = sexpr.get_unchecked<SExprList>().elem;
        std::vector<SExprLocRef> v;
        v.reserve(elems.size());
        for (const auto& el : elems)
            v.push_back(transform_sexpr(el, arena, std::forward<F>(f)));
        new_expr = arena.emplace(expr.loc_ref(), SExprList(std::move(v)));
    } else if (sexpr.isa<SExprVector>()) {
        auto elems = sexpr.get_unchecked<SExprVector>().elem;
        std::vector<SExprLocRef> v;
        v.reserve(elems.size());
        for (const auto& el : elems)
            v.push_back(transform_sexpr(el, arena, std::forward<F>(f)));
        new_expr = arena.emplace(expr.loc_ref(), SExprVector(std::move(v)));
    }

    return std::forward<F>(f)(new_expr, arena);
}

SExprLocRef Expander::add_scope(SExprLocRef expr, ScopeID scope) {
    return transform_sexpr(expr, _arena, [scope](SExprLocRef e, SExprArena& a) {
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

void Expander::report_error(SExprLocRef failed_expr, std::string_view msg) {
    _had_error = true;

    auto failed_str = _arena.dump(failed_expr.expr_ref());

    Error("{}", msg);
    std::println(std::cerr, "  for: {}", failed_str);

    struct Frame {
        int core_omitted;
        std::string dump;
    };
    std::vector<Frame> frames;
    frames.reserve(64);
    int core_omitted = 0;
    auto cur = _parent;
    while (cur != ExpansionStack::INVALID) {
        const auto& f = _stack.at(cur);
        if (f.is_core && !_show_core) {
            core_omitted++;
            cur = f.parent;
            continue;
        }
        frames.push_back({ core_omitted, _arena.dump(f.expr.expr_ref()) });
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

    auto loc = _arena.location(failed_expr.loc_ref());
    std::println(std::cerr, "  at {}", loc.source_location());
}

bool Expander::check_arity(
    SExprLocRef el, std::size_t min_arity, std::size_t max_arity) {
    // max_arity == 0 means no maximum
    const auto& expr = _arena.at(el);
    if (!expr.isa<SExprList>()) {
        return false;
    }
    const auto& list = expr.get_unchecked<SExprList>();
    if (list.elem.size() < min_arity + 1) {
        report_error(el,
            std::format("arity mismatch: expected{} {} arguments, got {}",
                max_arity == 0 ? " at least" : "", min_arity,
                list.elem.size() - 1));
        _had_error = true;
        return false;
    }
    if (max_arity == min_arity && list.elem.size() != min_arity + 1) {
        report_error(el,
            std::format("arity mismatch: expected {} arguments, got {}",
                min_arity, list.elem.size() - 1));
        _had_error = true;
        return false;
    }
    if (max_arity != 0 && list.elem.size() > max_arity + 1) {
        report_error(el,
            std::format("arity mismatch: expected at most {} arguments, got {}",
                max_arity, list.elem.size() - 1));
        _had_error = true;
        return false;
    }
    auto tail = _arena[list.elem.back()];
    if (!tail.isa<LispNil>()) {
        report_error(el, "arity mismatch: improper list");
        _had_error = true;
        return false;
    }
    return true;
}

bool Expander::is_identifier_active(const LispIdent& id) {
    auto id_binding = _env.find_binding(id);
    if (!id_binding)
        return false;

    auto matches = [&](SExprLocRef k) {
        if (!k.is_valid())
            return false;
        const auto& k_expr = _arena.at(k);
        if (k_expr.isa<SExprList>()) {
            const auto& list = k_expr.get_unchecked<SExprList>();
            if (!list.elem.empty()) {
                const auto& head = _arena.at(list.elem[0]);
                if (head.isa<LispIdent>()) {
                    auto head_binding
                        = _env.find_binding(head.get_unchecked<LispIdent>());
                    if (!head_binding)
                        return false;
                    return &head_binding->get() == &id_binding->get();
                }
            }
        }
        return false;
    };

    auto cur = _parent;
    while (cur != ExpansionStack::INVALID) {
        const auto& frame = _stack.at(cur);
        if (matches(frame.expr))
            return true;
        cur = frame.parent;
    }
    return false;
}

std::vector<SExprLocRef> Expander::expand_lambda(
    const SExprList& list, SExprLocRef root) {
    // (lambda formals body...)
    if (!check_arity(root, 2, 0))
        return { SExprLocRef::invalid() };

    ScopeID scope = _env.new_scope();
    auto scoped_params = add_scope(list.elem[1], scope);

    std::vector<SExprLocRef> params;
    bool bind_error = false;
    auto bind = [&](SExprLocRef p) {
        if (_arena.at(p).isa<LispIdent>()) {
            auto id = _arena.at(p).get_unchecked<LispIdent>();
            auto resolved = _env.unique_name(id.name);
            auto resolved_id = LispIdent(resolved);
            _env.add_binding(id, Binding(VarBinding(resolved_id)));
            params.push_back(
                _arena.emplace(p.loc_ref(), std::move(resolved_id)));
        } else if (_arena.at(p).isa<LispNil>()) {
            params.push_back(p);
        } else {
            report_error(p, "lambda: expected identifier in parameter list");
            bind_error = true;
        }
    };

    const auto& p_expr = _arena.at(scoped_params);
    if (p_expr.isa<SExprList>()) {
        // FIXME: check for
        // 1) duplicate parameters
        // 2) All indents, last one is Nil or parameter
        for (const auto& p : p_expr.get_unchecked<SExprList>().elem)
            bind(p);
        if (bind_error)
            return { SExprLocRef::invalid() };
    } else if (p_expr.isa<LispIdent>()) {
        bind(scoped_params);
    } else {
        report_error(scoped_params,
            "lambda: expected one identifier or a list of identifiers");
        return { SExprLocRef::invalid() };
    }
    auto final_params
        = _arena.emplace(scoped_params.loc_ref(), SExprList(std::move(params)));

    std::vector<SExprLocRef> out;
    out.push_back(make_canonical(list.elem[0].loc_ref(), "lambda", _arena));
    out.push_back(final_params);
    for (std::size_t i = 2; i < list.elem.size(); ++i) {
        auto r = as_sub_expression().expand(add_scope(list.elem[i], scope));
        if (r.size() != 1 || !r[0].is_valid())
            return { SExprLocRef::invalid() };
        out.insert(out.end(), r.begin(), r.end());
    }
    return { _arena.emplace(root.loc_ref(), SExprList(std::move(out))) };
}

std::vector<SExprLocRef> Expander::expand_quote(
    const SExprList& list, SExprLocRef root) {
    if (!check_arity(root, 2, 2))
        return { SExprLocRef::invalid() };
    std::vector<SExprLocRef> out;
    out.push_back(make_canonical(list.elem[0].loc_ref(), "quote", _arena));
    out.push_back(list.elem[1]);
    out.push_back(_arena.nil(root.loc_ref()));
    return { _arena.emplace(root.loc_ref(), SExprList(std::move(out))) };
}

std::vector<SExprLocRef> Expander::expand_if(
    const SExprList& list, SExprLocRef root) {
    // (if condition then-clause else-clause?)
    if (!check_arity(root, 3, 4))
        return { SExprLocRef::invalid() };

    std::vector<SExprLocRef> out;
    out.push_back(make_canonical(list.elem[0].loc_ref(), "if", _arena));
    for (std::size_t i = 1; i < list.elem.size(); ++i) {
        auto r = as_sub_expression().expand(list.elem[i]);
        if (r.size() != 1 || !r[0].is_valid())
            return { SExprLocRef::invalid() };
        out.push_back(r[0]);
    }
    return { _arena.emplace(root.loc_ref(), SExprList(std::move(out))) };
}

std::vector<SExprLocRef> Expander::expand_begin(
    const SExprList& list, SExprLocRef root) {
    if (!check_arity(root, 1, 0))
        return { SExprLocRef::invalid() };

    if (!_is_top_level) {
        // call macro __begin
        auto new_list = list;
        new_list.elem[0] = make_canonical(root.loc_ref(), "__begin", _arena);
        auto ref
            = _arena.emplace(root.loc_ref(), SExprList(std::move(new_list)));
        auto r = expand(ref);
        if (r.size() != 1 || !r[0].is_valid())
            return { SExprLocRef::invalid() };
        return { r[0] };
    }

    std::vector<SExprLocRef> out;
    for (std::size_t i = 1; i < list.elem.size(); ++i) {
        auto r = expand(list.elem[i]);
        if (std::ranges::any_of(r, [](const auto& r) { return !r.is_valid(); }))
            return { SExprLocRef::invalid() };
        out.insert(out.end(), r.begin(), r.end());
    }
    return out;
}

std::vector<SExprLocRef> Expander::expand_set(
    const SExprList& list, SExprLocRef root) {
    // (set! variable expression)
    if (!check_arity(root, 3, 3))
        return { SExprLocRef::invalid() };

    auto var_ref = list.elem[1];
    auto var_expanded = as_sub_expression().expand(var_ref);
    if (var_expanded.size() != 1 || !var_expanded[0].is_valid())
        return { SExprLocRef::invalid() };
    if (!_arena.at(var_expanded[0]).isa<LispIdent>()) {
        report_error(var_ref, "set!: expected identifier for variable");
        return { SExprLocRef::invalid() };
    }

    std::vector<SExprLocRef> out;
    out.push_back(make_canonical(list.elem[0].loc_ref(), "set!", _arena));
    out.push_back(var_expanded[0]);
    for (std::size_t i = 2; i < list.elem.size(); ++i) {
        auto r = as_sub_expression().expand(list.elem[i]);
        if (r.size() != 1 || !r[0].is_valid())
            return { SExprLocRef::invalid() };
        out.push_back(r[0]);
    }
    return { _arena.emplace(root.loc_ref(), SExprList(std::move(out))) };
}

std::vector<SExprLocRef> Expander::expand_define(
    const SExprList& list, SExprLocRef root) {
    if (!check_arity(root, 3, 0))
        return { SExprLocRef::invalid() };

    auto var = list.elem[1];

    if (_arena.at(var).isa<SExprList>()) {
        const auto& var_list = _arena.at(var).get_unchecked<SExprList>().elem;
        if (var_list.empty())
            return { SExprLocRef::invalid() };

        auto func_name = var_list[0];

        if (!_arena.at(func_name).isa<LispIdent>()) {
            report_error(
                func_name, "define: expected identifier for function name");
            return { SExprLocRef::invalid() };
        }

        std::vector<SExprLocRef> params;
        std::size_t var_logical = var_list.size();
        if (!var_list.empty() && _arena.at(var_list.back()).isa<LispNil>())
            var_logical--;
        for (std::size_t i = 1; i < var_logical; ++i)
            params.push_back(var_list[i]);
        params.push_back(_arena.nil(var.loc_ref()));
        auto params_node
            = _arena.emplace(var.loc_ref(), SExprList(std::move(params)));

        std::vector<SExprLocRef> lam;
        lam.push_back(make_canonical(list.elem[0].loc_ref(), "lambda", _arena));
        lam.push_back(params_node);
        for (std::size_t i = 2; i < list.elem.size(); ++i)
            lam.push_back(list.elem[i]);
        auto lam_node
            = _arena.emplace(var.loc_ref(), SExprList(std::move(lam)));

        std::vector<SExprLocRef> def;
        def.push_back(make_canonical(list.elem[0].loc_ref(), "define", _arena));
        def.push_back(func_name);
        def.push_back(lam_node);
        def.push_back(_arena.nil(root.loc_ref()));
        auto desugared
            = _arena.emplace(root.loc_ref(), SExprList(std::move(def)));
        return { expand(desugared) };
    }

    // (define var expr)
    if (_arena.at(var).isa<LispIdent>()) {
        auto id = _arena.at(var).get_unchecked<LispIdent>();
        if (is_identifier_active(id)) {
            report_error(root, "define: invalid context for definition");
            return { SExprLocRef::invalid() };
        }

        const auto exact = _env.find_exact_binding(id);
        if (exact && exact->get().isa<VarBinding>()) {
            var = _arena.emplace(
                var.loc_ref(), exact->get().get_unchecked<VarBinding>().id);
        } else {
            auto resolved = _env.unique_name(id.name);
            auto resolved_id = LispIdent(resolved);
            _env.add_binding(id, Binding(VarBinding(resolved_id)));
            var = _arena.emplace(var.loc_ref(), std::move(resolved_id));
        }
    } else {
        report_error(var, "define: expected identifier or list");
        return { SExprLocRef::invalid() };
    }

    std::vector<SExprLocRef> out;
    out.push_back(make_canonical(list.elem[0].loc_ref(), "define", _arena));
    out.push_back(var);
    for (std::size_t i = 2; i < list.elem.size(); ++i) {
        auto r = as_sub_expression().expand(list.elem[i]);
        if (r.size() != 1 || !r[0].is_valid())
            return { SExprLocRef::invalid() };
        out.push_back(r[0]);
    }
    return { _arena.emplace(root.loc_ref(), SExprList(std::move(out))) };
}

std::optional<std::unique_ptr<Transformer>> Expander::parse_syntax_rules(
    SExprLocRef transformer_spec, std::string_view form_prefix) {
    if (!_arena.at(transformer_spec).isa<SExprList>()) {
        report_error(transformer_spec,
            std::format("{}: expected (syntax-rules ...)", form_prefix));
        return std::nullopt;
    }
    const auto& spec_list
        = _arena.at(transformer_spec).get<SExprList>()->get().elem;
    if (spec_list.empty()) {
        report_error(transformer_spec,
            std::format("{}: expected (syntax-rules ...)", form_prefix));
        return std::nullopt;
    }
    if (!_arena.at(spec_list[0]).isa<LispIdent>()) {
        report_error(transformer_spec,
            std::format("{}: expected syntax-rules keyword", form_prefix));
        return std::nullopt;
    }
    if (_arena.at(spec_list[0]).get<LispIdent>()->get().name
        != "syntax-rules") {
        report_error(transformer_spec,
            std::format("{}: expected syntax-rules keyword", form_prefix));
        return std::nullopt;
    }
    std::vector<std::string> literals;
    if (spec_list.size() >= 2) {
        if (_arena.at(spec_list[1]).isa<SExprList>()) {
            const auto& lit_list
                = _arena.at(spec_list[1]).get_unchecked<SExprList>().elem;
            for (const auto& lit : lit_list) {
                if (_arena.at(lit).isa<LispNil>())
                    continue;
                if (_arena.at(lit).isa<LispIdent>()) {
                    literals.push_back(
                        _arena.at(lit).get_unchecked<LispIdent>().name);
                } else {
                    report_error(lit,
                        std::format(
                            "{}: invalid syntax-rule: not an identifier",
                            form_prefix));
                }
            }
        } else {
            report_error(transformer_spec,
                std::format(
                    "{}: invalid syntax-rule: capture list is not a list",
                    form_prefix));
            return std::nullopt;
        }
    }
    std::vector<Transformer::SyntaxRule> rules;
    for (std::size_t i = 2; i < spec_list.size(); ++i) {
        if (!_arena.at(spec_list[i]).isa<SExprList>()) {
            if (i == spec_list.size() - 1
                && _arena.at(spec_list[i]).isa<LispNil>())
                continue;
            report_error(transformer_spec,
                std::format(
                    "{}: invalid syntax-rule: not a list", form_prefix));
            continue;
        }
        const auto& rule_parts
            = _arena.at(spec_list[i]).get_unchecked<SExprList>().elem;
        if (rule_parts.size() == 3 && _arena.at(rule_parts[2]).isa<LispNil>())
            rules.push_back({ rule_parts[0], rule_parts[1] });
        else
            report_error(transformer_spec,
                std::format("{}: invalid syntax-rule: unexpected rule format",
                    form_prefix));
    }
    return std::make_unique<Transformer>(
        std::move(rules), std::move(literals), _arena);
}

std::vector<SExprLocRef> Expander::expand_define_syntax(
    const SExprList& list, SExprLocRef root) {
    // (define-syntax name (syntax-rules (literals…) (pattern template) …))
    if (!_is_top_level) {
        report_error(root, "define-syntax: define-syntax allowed only at top level");
        return { SExprLocRef::invalid() };
    }
    if (list.elem.size() < 3) {
        report_error(root, "define-syntax: missing name or transformer path");
        return { SExprLocRef::invalid() };
    }

    auto name_ex = list.elem[1];
    auto transformer_spec = list.elem[2];

    if (!_arena.at(name_ex).isa<LispIdent>()) {
        report_error(
            name_ex, "define-syntax: expected identifier for macro name");
        return { SExprLocRef::invalid() };
    }
    auto macro_name = _arena.at(name_ex).get<LispIdent>()->get();

    if (is_identifier_active(macro_name)) {
        report_error(root, "define-syntax: invalid context for definition");
        return { SExprLocRef::invalid() };
    }

    auto transformer = parse_syntax_rules(transformer_spec);
    if (!transformer)
        return { SExprLocRef::invalid() };
    _env.add_binding(macro_name,
        Binding(MacroBinding { .transformer = std::move(*transformer),
            .is_core = _is_core,
            .output_excluded_scope = std::nullopt }));
    return {};
}

std::vector<SExprLocRef> Expander::expand_let_letrec_syntax(
    const SExprList& list, SExprLocRef root, bool is_letrec) {
    std::string let_syntax_name = is_letrec ? "letrec-syntax" : "let-syntax";
    // (let-syntax ((name transformer) ...) body ...)
    if (list.elem.size() < 3) {
        report_error(
            root, std::format("{}: missing bindings or body", let_syntax_name));
        return { SExprLocRef::invalid() };
    }

    const auto& bindings_expr = _arena.at(list.elem[1]);
    if (!bindings_expr.isa<SExprList>()) {
        report_error(list.elem[1],
            std::format("{}: expected list of bindings", let_syntax_name));
        return { SExprLocRef::invalid() };
    }

    ScopeID scope = _env.new_scope();
    auto bindings = bindings_expr.get_unchecked<SExprList>().elem;

    for (const auto& binding : bindings) {
        const auto& binding_expr = _arena.at(binding);
        if (binding_expr.isa<LispNil>())
            continue;
        if (!binding_expr.isa<SExprList>()) {
            report_error(binding,
                std::format("{}: expected (name transformer) for each binding",
                    let_syntax_name));
            return { SExprLocRef::invalid() };
        }
        auto pair = binding_expr.get_unchecked<SExprList>().elem;
        if (pair.size() < 2) {
            report_error(binding,
                std::format("{}: expected (name transformer) for each binding",
                    let_syntax_name));
            return { SExprLocRef::invalid() };
        }

        auto name_ex = pair[0];
        if (!_arena.at(name_ex).isa<LispIdent>()) {
            report_error(
                name_ex, "let-syntax: expected identifier for macro name");
            return { SExprLocRef::invalid() };
        }
        auto scoped_name = add_scope(name_ex, scope);
        auto macro_ident = _arena.at(scoped_name).get_unchecked<LispIdent>();

        auto transformer_spec = pair[1];
        if (is_letrec) {
            transformer_spec = add_scope(transformer_spec, scope);
        }

        auto transformer = parse_syntax_rules(
            transformer_spec, is_letrec ? "letrec-syntax" : "let-syntax");
        if (!transformer)
            return { SExprLocRef::invalid() };

        _env.add_binding(macro_ident,
            Binding(MacroBinding { .transformer = std::move(*transformer),
                .is_core = _is_core,
                .output_excluded_scope
                = is_letrec ? std::nullopt : std::optional(scope) }));
    }

    std::vector<SExprLocRef> out;
    out.push_back(make_canonical(list.elem[0].loc_ref(), "begin", _arena));
    for (std::size_t i = 2; i < list.elem.size(); ++i) {
        out.push_back(list.elem[i]);
    }
    auto new_out
        = _arena.emplace(list.elem[0].loc_ref(), SExprList(std::move(out)));
    return expand(add_scope(new_out, scope));
}

std::vector<SExprLocRef> Expander::expand_macro(
    SExprLocRef root, const MacroBinding& macro) {
    ScopeID intro = _env.new_scope();
    auto scoped_in = add_scope(root, intro);
    auto result = macro.transformer->transcribe(scoped_in, _arena);
    if (!result.is_valid()) {
        report_error(
            root, "macro expansion failed: no syntax-rules pattern matched");
        return { SExprLocRef::invalid() };
    }

    return with_core(macro.is_core)
        .with_excluded_scope(macro.output_excluded_scope)
        .expand(add_scope(result, intro));
}

std::vector<SExprLocRef> Expander::expand(SExprLocRef root) {
    if (!root.is_valid())
        return { SExprLocRef::invalid() };

    if (_current_depth > _max_depth) {
        report_error(root,
            "expand: maximum macro expansion depth exceeded (possible infinite "
            "recursion)");
        return { SExprLocRef::invalid() };
    }

    const auto& sexpr = _arena.at(root);

    if (sexpr.isa<LispIdent>()) {
        const auto& ident = sexpr.get_unchecked<LispIdent>();
        auto binding = _env.find_binding(ident, _output_excluded_scope);
        if (!binding) {
            auto clean = ident;
            clean.scopes.clear();
            return { _arena.emplace(root.loc_ref(), std::move(clean)) };
        }
        if (binding->get().isa<VarBinding>())
            return { _arena.emplace(root.loc_ref(),
                binding->get().get_unchecked<VarBinding>().id) };
        return { root };
    }

    if (sexpr.isa<SExprList>()) {
        auto list = sexpr.get_unchecked<SExprList>();
        if (list.elem.empty())
            return { root };

        const auto& head_expr = _arena.at(list.elem[0]);
        if (head_expr.isa<LispIdent>()) {
            auto head_id = head_expr.get_unchecked<LispIdent>();
            auto binding = _env.find_binding(head_id, _output_excluded_scope);

            if (binding && binding->get().isa<CoreBinding>()) {
                const auto& name = head_id.name;

                ExpStackRef frame = _stack.push(root, true, _parent);
                auto core_expander = with_parent(frame, true);

                if (name == "lambda")
                    return core_expander.expand_lambda(list, root);
                if (name == "quote")
                    return core_expander.expand_quote(list, root);
                if (name == "if")
                    return core_expander.expand_if(list, root);
                if (name == "begin")
                    return core_expander.expand_begin(list, root);
                if (name == "set!")
                    return core_expander.expand_set(list, root);
                if (name == "define")
                    return core_expander.expand_define(list, root);
                if (name == "define-syntax")
                    return core_expander.expand_define_syntax(list, root);
                if (name == "let-syntax")
                    return core_expander.expand_let_letrec_syntax(
                        list, root, false);
                if (name == "letrec-syntax")
                    return core_expander.expand_let_letrec_syntax(
                        list, root, true);
                if (name == "syntax-error") {
                    std::string msg;
                    if (list.elem.size() >= 3) {
                        auto msg_expr = _arena.at(list.elem[1]);
                        if (msg_expr.isa<LispString>()) {
                            msg = msg_expr.get_unchecked<LispString>();
                        }
                    }
                    report_error(root, std::format("syntax-error: {}", msg));
                    return { SExprLocRef::invalid() };
                }
            }

            if (binding && binding->get().isa<MacroBinding>()) {
                bool is_core
                    = binding->get().get_unchecked<MacroBinding>().is_core;
                ExpStackRef frame = _stack.push(root, is_core, _parent);
                return with_parent(frame, is_core)
                    .with_depth(_current_depth + 1)
                    .expand_macro(
                        root, binding->get().get<MacroBinding>()->get());
            }
        }

        std::vector<SExprLocRef> out;
        out.reserve(list.elem.size());
        for (const auto& el : list.elem) {
            auto r = expand(el);
            if (std::ranges::any_of(
                    r, [](const auto& r) { return !r.is_valid(); })) {
                _had_error = true;
                continue;
            }
            out.insert(out.end(), r.begin(), r.end());
        }
        return { _arena.emplace(root.loc_ref(), SExprList(std::move(out))) };
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
            Expander expander(_env, *_core_arena, _exp_stack, dummy_error,
                _show_core_expansion, _max_expansion_depth);
            (void)expander.with_core(true).expand(form);
        }
    }
    _core_loaded = true;
}

[[nodiscard]] SExprLocRef ExpandPass::run(
    SExprLocRef root, SExprArena& arena) noexcept {
    if (!_core_loaded)
        load_core(arena);

    _had_error = false;
    Expander expander(_env, arena, _exp_stack, _had_error, _show_core_expansion,
        _max_expansion_depth);

    if (arena.at(root).isa<SExprList>()) {
        const auto& list = arena.at(root).get_unchecked<SExprList>();
        std::vector<SExprLocRef> out;
        out.reserve(list.elem.size());
        for (const auto& el : list.elem) {
            auto r = expander.expand(el);
            if (std::ranges::any_of(
                    r, [](const auto& r) { return !r.is_valid(); })) {
                _had_error = true;
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
