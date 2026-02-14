module lpc.frontend.expand;

import std;

import lpc.frontend.lexer;
import lpc.frontend.syntax;
import lpc.utils.logging;

namespace lpc::frontend {

using lpc::utils::Error;

[[nodiscard]] SpanRef Expander::add_scope(SpanRef expr, ScopeID scope) {
    if (!expr.is_valid())
        return expr;

    auto scopes = _arena.scopes(expr);
    if (scopes.contains(scope))
        scopes.erase(scope);
    else
        scopes.insert(scope);
    auto new_scopes_ref = _arena.scope_arena().intern(std::move(scopes));
    const auto& sexpr = _arena.expr(expr);
    if (const auto* list = sexpr.get<SExprList>()) {
        std::vector<SpanRef> v;
        v.reserve(list->elem.size());
        for (const auto& el : list->elem)
            v.push_back(add_scope(el, scope));
        return _arena.expand(_arena.loc_ref(expr), _parent, new_scopes_ref,
            SExprList(std::move(v)));
    }
    if (const auto* vec = sexpr.get<SExprVector>()) {
        std::vector<SpanRef> v;
        v.reserve(vec->elem.size());
        for (const auto& el : vec->elem)
            v.push_back(add_scope(el, scope));
        return _arena.expand(_arena.loc_ref(expr), _parent, new_scopes_ref,
            SExprVector(std::move(v)));
    }
    return _arena.expand(
        _arena.loc_ref(expr), _parent, new_scopes_ref, _arena.expr_ref(expr));
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

void Expander::report_error(SpanRef failed_expr, std::string_view msg) {
    _had_error = true;

    auto failed_str = _arena.dump(failed_expr);

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
    while (cur.is_valid()) {
        if (_arena.is_core_binding(cur) && !_show_core) {
            core_omitted++;
            cur = _arena.at(cur).parent();
            continue;
        }
        frames.push_back({ core_omitted, _arena.dump(cur) });
        core_omitted = 0;
        cur = _arena.at(cur).parent();
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

    auto loc = _arena.loc(failed_expr);
    std::println(std::cerr, "  at {}", loc.source_location());
}

bool Expander::check_arity(
    SpanRef el, std::size_t min_arity, std::size_t max_arity) {
    // max_arity == 0 means no maximum
    const auto& list = *_arena.get<SExprList>(el);
    auto size = list.elem.size();
    if (size < min_arity + 1) {
        report_error(el,
            std::format("arity mismatch: expected{} {} arguments, got {}",
                max_arity == 0 ? " at least" : "", min_arity, size - 1));
        _had_error = true;
        return false;
    }
    if (max_arity == min_arity && size != min_arity + 1) {
        report_error(el,
            std::format("arity mismatch: expected {} arguments, got {}",
                min_arity, size - 1));
        _had_error = true;
        return false;
    }
    if (max_arity != 0 && size > max_arity + 1) {
        report_error(el,
            std::format("arity mismatch: expected at most {} arguments, got {}",
                max_arity, size - 1));
        _had_error = true;
        return false;
    }
    if (!_arena.is_nil(list.elem.back())) {
        report_error(el, "arity mismatch: improper list");
        _had_error = true;
        return false;
    }
    return true;
}

bool Expander::is_identifier_active(SpanRef id_ref) {
    const auto* id = _arena.get<LispIdent>(id_ref);
    if (!id)
        return false;
    const auto* id_binding = _env.find_binding(id->name, _arena.scopes(id_ref));
    if (id_binding == nullptr)
        return false;

    auto matches = [&](SpanRef k) {
        if (!k.is_valid())
            return false;
        if (const auto* list = _arena.get<SExprList>(k)) {
            if (!list->elem.empty()) {
                if (const auto* kid = _arena.get<LispIdent>(list->elem[0])) {
                    const auto* binding = _env.find_binding(
                        kid->name, _arena.scopes(list->elem[0]));
                    if (!binding)
                        return false;
                    return binding == id_binding;
                }
            }
        }
        return false;
    };

    auto cur = _parent;
    while (cur.is_valid()) {
        if (matches(cur))
            return true;
        cur = _arena.at(cur).parent();
    }
    return false;
}

std::vector<SpanRef> Expander::expand_lambda(
    const SExprList& list, SpanRef root) {
    // (lambda formals body...)
    if (!check_arity(root, 2, 0))
        return { SpanRef::invalid() };

    ScopeID scope = _env.new_scope();
    auto scoped_params = add_scope(list.elem[1], scope);

    std::vector<SpanRef> params;
    std::set<std::string> seen_names;
    bool bind_error = false;

    auto bind = [&](SpanRef p, bool is_rest, bool is_last) {
        if (const auto* id = _arena.get<LispIdent>(p)) {
            if (seen_names.contains(id->name)) {
                report_error(p,
                    std::format(
                        "lambda: duplicate parameter name: {}", id->name));
                bind_error = true;
                return;
            }
            seen_names.insert(id->name);
            auto resolved = _env.unique_name(id->name);
            _env.add_binding(id->name, _arena.scopes(p),
                Binding(VarBinding(LispIdent(resolved))));
            params.push_back(
                _arena.get_ident(_arena.loc_ref(p), resolved, _parent));
        } else if (_arena.is_nil(p)) {
            if (is_rest) {
                report_error(
                    p, "lambda: expected identifier for rest parameter");
                bind_error = true;
                return;
            }
            if (!is_last) {
                report_error(
                    p, "lambda: invalid use of empty list in parameter list");
                bind_error = true;
                return;
            }
            params.push_back(p);
        } else {
            report_error(p, "lambda: expected identifier in parameter list");
            bind_error = true;
        }
    };

    SpanRef final_params;
    if (const auto* p_list = _arena.get<SExprList>(scoped_params)) {
        for (std::size_t i = 0; i < p_list->elem.size(); ++i) {
            bool is_last = (i == p_list->elem.size() - 1);
            bool is_rest = is_last && !_arena.is_nil(p_list->elem[i]);
            bind(p_list->elem[i], is_rest, is_last);
        }
        if (bind_error)
            return { SpanRef::invalid() };
        final_params = _arena.expand(_arena.loc_ref(scoped_params), _parent,
            ScopeSetRef::invalid(), SExprList(std::move(params)));
    } else if (_arena.is_ident(scoped_params)) {
        bind(scoped_params, true, true);
        if (bind_error)
            return { SpanRef::invalid() };
        final_params = params[0];
    } else if (_arena.is_nil(scoped_params)) {
        final_params = scoped_params;
    } else {
        report_error(scoped_params,
            "lambda: expected one identifier or a list of identifiers");
        return { SpanRef::invalid() };
    }

    std::vector<SpanRef> out;
    out.push_back(
        _arena.get_ident(_arena.loc_ref(list.elem[0]), "lambda", _parent));
    out.push_back(final_params);
    for (std::size_t i = 2; i < list.elem.size(); ++i) {
        auto r = as_sub_expression().expand(add_scope(list.elem[i], scope));
        if (r.size() != 1 || !r[0].is_valid())
            return { SpanRef::invalid() };
        out.insert(out.end(), r.begin(), r.end());
    }
    return { _arena.expand(_arena.loc_ref(root), _parent,
        ScopeSetRef::invalid(), SExprList(std::move(out))) };
}

std::vector<SpanRef> Expander::expand_quote(
    const SExprList& list, SpanRef root) {
    if (!check_arity(root, 2, 2))
        return { SpanRef::invalid() };
    std::vector<SpanRef> out;
    out.push_back(
        _arena.get_ident(_arena.loc_ref(list.elem[0]), "quote", _parent));
    out.push_back(list.elem[1]);
    out.push_back(_arena.nil(_arena.loc_ref(root), _parent));
    return { _arena.expand(_arena.loc_ref(root), _parent,
        ScopeSetRef::invalid(), SExprList(std::move(out))) };
}

std::vector<SpanRef> Expander::expand_if(const SExprList& list, SpanRef root) {
    // (if condition then-clause else-clause?)
    if (!check_arity(root, 3, 4))
        return { SpanRef::invalid() };

    std::vector<SpanRef> out;
    out.push_back(
        _arena.get_ident(_arena.loc_ref(list.elem[0]), "if", _parent));
    for (std::size_t i = 1; i < list.elem.size(); ++i) {
        auto r = as_sub_expression().expand(list.elem[i]);
        if (r.size() != 1 || !r[0].is_valid())
            return { SpanRef::invalid() };
        out.push_back(r[0]);
    }
    return { _arena.expand(_arena.loc_ref(root), _parent,
        ScopeSetRef::invalid(), SExprList(std::move(out))) };
}

std::vector<SpanRef> Expander::expand_begin(
    const SExprList& list, SpanRef root) {
    if (!check_arity(root, 1, 0))
        return { SpanRef::invalid() };

    if (!_is_top_level) {
        // call macro __begin
        auto new_list = list;
        new_list.elem[0] = _arena.get_ident(
            _arena.loc_ref(list.elem[0]), "__begin", _parent);
        auto ref = _arena.expand(_arena.loc_ref(root), _parent,
            ScopeSetRef::invalid(), SExprList(std::move(new_list)));
        auto r = expand(ref);
        if (r.size() != 1 || !r[0].is_valid())
            return { SpanRef::invalid() };
        return { r[0] };
    }

    std::vector<SpanRef> out;
    for (std::size_t i = 1; i < list.elem.size(); ++i) {
        auto r = expand(list.elem[i]);
        if (std::ranges::any_of(r, [](const auto& r) { return !r.is_valid(); }))
            return { SpanRef::invalid() };
        out.insert(out.end(), r.begin(), r.end());
    }
    return out;
}

std::vector<SpanRef> Expander::expand_set(const SExprList& list, SpanRef root) {
    // (set! variable expression)
    if (!check_arity(root, 3, 3))
        return { SpanRef::invalid() };

    auto var_ref = list.elem[1];
    auto var_expanded = as_sub_expression().expand(var_ref);
    if (var_expanded.size() != 1 || !var_expanded[0].is_valid())
        return { SpanRef::invalid() };
    if (!_arena.is_ident(var_expanded[0])) {
        report_error(var_ref, "set!: expected identifier for variable");
        return { SpanRef::invalid() };
    }

    std::vector<SpanRef> out;
    out.push_back(
        _arena.get_ident(_arena.loc_ref(list.elem[0]), "set!", _parent));
    out.push_back(var_expanded[0]);
    for (std::size_t i = 2; i < list.elem.size(); ++i) {
        auto r = as_sub_expression().expand(list.elem[i]);
        if (r.size() != 1 || !r[0].is_valid())
            return { SpanRef::invalid() };
        out.push_back(r[0]);
    }
    return { _arena.expand(_arena.loc_ref(root), _parent,
        ScopeSetRef::invalid(), SExprList(std::move(out))) };
}

std::vector<SpanRef> Expander::expand_define(
    const SExprList& list, SpanRef root) {
    if (!check_arity(root, 3, 0))
        return { SpanRef::invalid() };

    auto var = list.elem[1];

    if (const auto* var_list = _arena.get<SExprList>(var)) {
        auto elems = var_list->elem;
        if (elems.empty())
            return { SpanRef::invalid() };

        auto func_name = elems[0];

        if (!_arena.is_ident(func_name)) {
            report_error(
                func_name, "define: expected identifier for function name");
            return { SpanRef::invalid() };
        }

        std::vector<SpanRef> params;
        std::size_t var_logical = elems.size();
        if (_arena.is_nil(elems.back()))
            var_logical--;
        for (std::size_t i = 1; i < var_logical; ++i)
            params.push_back(elems[i]);
        params.push_back(_arena.nil(_arena.loc_ref(var), _parent));
        auto params_node = _arena.expand(_arena.loc_ref(var), _parent,
            ScopeSetRef::invalid(), SExprList(std::move(params)));

        std::vector<SpanRef> lam;
        lam.push_back(
            _arena.get_ident(_arena.loc_ref(list.elem[0]), "lambda", _parent));
        lam.push_back(params_node);
        for (std::size_t i = 2; i < list.elem.size(); ++i)
            lam.push_back(list.elem[i]);
        auto lam_node = _arena.expand(_arena.loc_ref(var), _parent,
            ScopeSetRef::invalid(), SExprList(std::move(lam)));

        std::vector<SpanRef> def;
        def.push_back(
            _arena.get_ident(_arena.loc_ref(list.elem[0]), "define", _parent));
        def.push_back(func_name);
        def.push_back(lam_node);
        def.push_back(_arena.nil(_arena.loc_ref(root), _parent));
        auto desugared = _arena.expand(_arena.loc_ref(root), _parent,
            ScopeSetRef::invalid(), SExprList(std::move(def)));
        return { expand(desugared) };
    }

    // (define var expr)
    if (const auto* id = _arena.get<LispIdent>(var)) {
        if (is_identifier_active(var)) {
            report_error(root, "define: invalid context for definition");
            return { SpanRef::invalid() };
        }

        const auto* exact
            = _env.find_exact_binding(id->name, _arena.scopes(var));
        if (exact != nullptr && exact->isa<VarBinding>()) {
            var = _arena.get_ident(_arena.loc_ref(var),
                exact->get_unchecked<VarBinding>()->id.name, _parent);
        } else {
            auto resolved = _env.unique_name(id->name);
            _env.add_binding(id->name, _arena.scopes(var),
                Binding(VarBinding(LispIdent(resolved))));
            var = _arena.get_ident(_arena.loc_ref(var), resolved, _parent);
        }
    } else {
        report_error(var, "define: expected identifier or list");
        return { SpanRef::invalid() };
    }

    std::vector<SpanRef> out;
    out.push_back(
        _arena.get_ident(_arena.loc_ref(list.elem[0]), "define", _parent));
    out.push_back(var);
    for (std::size_t i = 2; i < list.elem.size(); ++i) {
        auto r = as_sub_expression().expand(list.elem[i]);
        if (r.size() != 1 || !r[0].is_valid())
            return { SpanRef::invalid() };
        out.push_back(r[0]);
    }
    return { _arena.expand(_arena.loc_ref(root), _parent,
        ScopeSetRef::invalid(), SExprList(std::move(out))) };
}

std::optional<std::unique_ptr<Transformer>> Expander::parse_syntax_rules(
    SpanRef transformer_spec, std::string_view form_prefix) {
    if (!_arena.is_list(transformer_spec)) {
        report_error(transformer_spec,
            std::format("{}: expected (syntax-rules ...)", form_prefix));
        return std::nullopt;
    }
    const auto spec_list = _arena.get<SExprList>(transformer_spec)->elem;
    if (spec_list.empty()) {
        report_error(transformer_spec,
            std::format("{}: expected (syntax-rules ...)", form_prefix));
        return std::nullopt;
    }
    if (!_arena.is_ident(spec_list[0])) {
        report_error(transformer_spec,
            std::format("{}: expected syntax-rules keyword", form_prefix));
        return std::nullopt;
    }
    if (_arena.get<LispIdent>(spec_list[0])->name != "syntax-rules") {
        report_error(transformer_spec,
            std::format("{}: expected syntax-rules keyword", form_prefix));
        return std::nullopt;
    }
    std::vector<std::string> literals;
    if (spec_list.size() >= 2) {
        if (_arena.is_list(spec_list[1])) {
            const auto lit_list = _arena.get<SExprList>(spec_list[1])->elem;
            for (const auto& lit : lit_list) {
                if (_arena.is_nil(lit))
                    continue;
                if (_arena.is_ident(lit)) {
                    literals.push_back(_arena.get<LispIdent>(lit)->name);
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
        if (!_arena.is_list(spec_list[i])) {
            if (i == spec_list.size() - 1 && _arena.is_nil(spec_list[i]))
                continue;
            report_error(transformer_spec,
                std::format(
                    "{}: invalid syntax-rule: not a list", form_prefix));
            continue;
        }
        const auto rule_parts = _arena.get<SExprList>(spec_list[i])->elem;
        if (rule_parts.size() == 3 && _arena.is_nil(rule_parts[2])) {
            // Validate pattern: must be a list with >= 2 elements or nil
            if (const auto* pattern_list_ptr
                = _arena.get<SExprList>(rule_parts[0])) {
                if (pattern_list_ptr->elem.size() < 2) {
                    report_error(rule_parts[0],
                        std::format("{}: invalid syntax-rule: pattern must "
                                    "be a non-empty list",
                            form_prefix));
                    continue;
                }
            } else {
                report_error(rule_parts[0],
                    std::format("{}: invalid syntax-rule: pattern must be a "
                                "list, got {}",
                        form_prefix, _arena.dump(rule_parts[0])));
                continue;
            }
            // Strip the head of the pattern
            const auto pattern_list
                = _arena.get<SExprList>(rule_parts[0])->elem;
            std::vector<SpanRef> pattern_tail_list;
            pattern_tail_list.reserve(pattern_list.size() - 1);
            for (std::size_t i = 1; i < pattern_list.size(); ++i)
                pattern_tail_list.push_back(pattern_list[i]);
            auto pattern_tail = _arena.expand(_arena.loc_ref(rule_parts[0]),
                _parent, ScopeSetRef::invalid(),
                SExprList(std::move(pattern_tail_list)));
            rules.push_back({ pattern_tail, rule_parts[1] });
        } else
            report_error(transformer_spec,
                std::format("{}: invalid syntax-rule: unexpected rule format",
                    form_prefix));
    }
    return std::make_unique<Transformer>(
        std::move(rules), std::move(literals), _arena);
}

std::vector<SpanRef> Expander::expand_define_syntax(
    const SExprList& list, SpanRef root) {
    // (define-syntax name (syntax-rules (literals…) (pattern template) …))
    if (!_is_top_level) {
        report_error(
            root, "define-syntax: define-syntax allowed only at top level");
        return { SpanRef::invalid() };
    }
    if (list.elem.size() < 3) {
        report_error(root, "define-syntax: missing name or transformer path");
        return { SpanRef::invalid() };
    }

    auto name_ex = list.elem[1];
    auto transformer_spec = list.elem[2];

    if (!_arena.is_ident(name_ex)) {
        report_error(
            name_ex, "define-syntax: expected identifier for macro name");
        return { SpanRef::invalid() };
    }
    auto macro_name = *_arena.get<LispIdent>(name_ex);

    if (is_identifier_active(name_ex)) {
        report_error(root, "define-syntax: invalid context for definition");
        return { SpanRef::invalid() };
    }

    auto transformer = parse_syntax_rules(transformer_spec);
    if (!transformer)
        return { SpanRef::invalid() };
    _env.add_binding(macro_name.name, _arena.scopes(name_ex),
        Binding(MacroBinding { .transformer = std::move(*transformer),
            .output_excluded_scope = std::nullopt }));
    return {};
}

std::vector<SpanRef> Expander::expand_let_letrec_syntax(
    const SExprList& list, SpanRef root, bool is_letrec) {
    std::string let_syntax_name = is_letrec ? "letrec-syntax" : "let-syntax";
    // (let-syntax ((name transformer) ...) body ...)
    if (list.elem.size() < 3) {
        report_error(
            root, std::format("{}: missing bindings or body", let_syntax_name));
        return { SpanRef::invalid() };
    }

    if (!_arena.is_list(list.elem[1])) {
        report_error(list.elem[1],
            std::format("{}: expected list of bindings", let_syntax_name));
        return { SpanRef::invalid() };
    }

    ScopeID scope = _env.new_scope();
    auto bindings = _arena.get<SExprList>(list.elem[1])->elem;

    for (const auto& binding : bindings) {
        if (_arena.is_nil(binding))
            continue;
        if (!_arena.is_list(binding)) {
            report_error(binding,
                std::format("{}: expected (name transformer) for each binding",
                    let_syntax_name));
            return { SpanRef::invalid() };
        }
        auto pair = _arena.get<SExprList>(binding)->elem;
        if (pair.size() < 2) {
            report_error(binding,
                std::format("{}: expected (name transformer) for each binding",
                    let_syntax_name));
            return { SpanRef::invalid() };
        }

        auto name_ex = pair[0];
        if (!_arena.is_ident(name_ex)) {
            report_error(
                name_ex, "let-syntax: expected identifier for macro name");
            return { SpanRef::invalid() };
        }
        auto scoped_name = add_scope(name_ex, scope);
        auto macro_ident = *_arena.get<LispIdent>(scoped_name);

        auto transformer_spec = pair[1];
        if (is_letrec) {
            transformer_spec = add_scope(transformer_spec, scope);
        }

        auto transformer = parse_syntax_rules(
            transformer_spec, is_letrec ? "letrec-syntax" : "let-syntax");
        if (!transformer)
            return { SpanRef::invalid() };

        _env.add_binding(macro_ident.name, _arena.scopes(scoped_name),
            Binding(MacroBinding { .transformer = std::move(*transformer),
                .output_excluded_scope
                = is_letrec ? std::nullopt : std::optional(scope) }));
    }

    std::vector<SpanRef> out;
    out.push_back(
        _arena.get_ident(_arena.loc_ref(list.elem[0]), "begin", _parent));
    for (std::size_t i = 2; i < list.elem.size(); ++i) {
        out.push_back(list.elem[i]);
    }
    auto new_out = _arena.expand(_arena.loc_ref(list.elem[0]), _parent,
        ScopeSetRef::invalid(), SExprList(std::move(out)));
    return expand(add_scope(new_out, scope));
}

std::vector<SpanRef> Expander::expand_macro(
    SpanRef root, const MacroBinding& macro) {
    ScopeID intro = _env.new_scope();
    auto scoped_in = add_scope(root, intro);
    auto result = macro.transformer->transcribe(scoped_in, root);
    if (!result.is_valid()) {
        report_error(
            root, "macro expansion failed: no syntax-rules pattern matched");
        return { SpanRef::invalid() };
    }

    return with_parent(root)
        .with_excluded_scope(macro.output_excluded_scope)
        .expand(add_scope(result, intro));
}

std::vector<SpanRef> Expander::expand(SpanRef root) {
    if (!root.is_valid())
        return { SpanRef::invalid() };

    if (_current_depth > _max_depth) {
        report_error(root,
            "expand: maximum macro expansion depth exceeded (possible infinite "
            "recursion)");
        return { SpanRef::invalid() };
    }

    if (const auto* ident = _arena.get<LispIdent>(root)) {
        const auto* binding = _env.find_binding(
            ident->name, _arena.scopes(root), _output_excluded_scope);
        if (binding == nullptr) {
            // clean.scopes.clear(); // NO LONGER NEEDED, ident has no scopes
            return { _arena.get_ident(
                _arena.loc_ref(root), ident->name, _parent) };
        }
        if (binding->isa<VarBinding>())
            return { _arena.get_ident(_arena.loc_ref(root),
                binding->get_unchecked<VarBinding>()->id.name, _parent) };
        return { root };
    }

    if (const auto* list_ptr = _arena.get<SExprList>(root)) {
        const auto list = *list_ptr;
        if (list.elem.empty())
            return { root };

        if (const auto* ident_ptr = _arena.get<LispIdent>(list.elem[0])) {
            const auto ident = *ident_ptr;
            const auto* binding = _env.find_binding(ident.name,
                _arena.scopes(list.elem[0]), _output_excluded_scope);

            if (binding != nullptr && binding->isa<CoreBinding>()) {
                const auto& name = ident.name;

                root = _arena.expand(_arena.loc_ref(root), _parent,
                    ScopeSetRef::invalid(), list);
                auto core_expander = with_parent(root);

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
                    if (!_arena.is_nil(list.elem.back())) {
                        report_error(root,
                            std::format(
                                "syntax-error: syntax-error: syntax-error: "
                                "syntax-error: syntax-error: syntax-error: "));
                        return { SpanRef::invalid() };
                    }
                    if (list.elem.size() >= 3)
                        if (const auto* str
                            = _arena.get<LispString>(list.elem[1]))
                            msg = *str;
                    report_error(root, std::format("syntax-error: {}", msg));
                    return { SpanRef::invalid() };
                }
            }

            if (binding != nullptr && binding->isa<MacroBinding>()) {
                root = _arena.expand(_arena.loc_ref(root), _parent,
                    ScopeSetRef::invalid(), list);
                return with_depth(_current_depth + 1)
                    .expand_macro(root, *binding->get<MacroBinding>());
            }
        }

        std::vector<SpanRef> out;
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
        return { _arena.expand(_arena.loc_ref(root), _parent,
            ScopeSetRef::invalid(), SExprList(std::move(out))) };
    }

    return { root };
}

#include "../core.scm"

void ExpandPass::load_core(SpanArena& user_arena) {
    // Add a leading space so it can't be a valid file name
    Lexer lexer(user_arena.location_arena(), " <core> ", CORE_SOURCE);
    if (lexer.is_failed())
        return;

    Parser parser(lexer.tokens(), user_arena);
    if (parser.is_failed())
        return;

    auto core_root = parser.root();

    if (const auto* list_ptr = user_arena.get<SExprList>(core_root)) {
        const auto list = *list_ptr;
        bool dummy_error = false;
        for (const auto& form : list.elem) {
            Expander expander(_env, user_arena, dummy_error,
                _show_core_expansion, _max_expansion_depth);
            (void)expander.expand(form);
        }
    }
    _core_loaded = true;
}

[[nodiscard]] SpanRef ExpandPass::run(SpanRef root, SpanArena& arena) noexcept {
    if (!_core_loaded)
        load_core(arena);

    _had_error = false;
    Expander expander(
        _env, arena, _had_error, _show_core_expansion, _max_expansion_depth);

    if (const auto* list_ptr = arena.get<SExprList>(root)) {
        const auto list = *list_ptr;
        std::vector<SpanRef> out;
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
        auto expanded = arena.expand(arena.loc_ref(root), SpanRef::invalid(),
            ScopeSetRef::invalid(), SExprList(std::move(out)));
        if (_had_error)
            return SpanRef::invalid();
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
