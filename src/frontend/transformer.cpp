module lpc.frontend.transformer;

namespace lpc::frontend {

static bool is_ellipsis(SExprLocRef ref, SExprArena& arena) {
    if (!ref.is_valid())
        return false;
    const auto& expr = arena.at(ref);
    if (!expr.holds_alternative<LispIdent>())
        return false;
    return expr.get<LispIdent>()->get().name == "...";
}

static std::size_t logical_size(const SExprList& list, SExprArena& arena) {
    if (list.elem.empty())
        return 0;
    if (arena.at(list.elem.back()).holds_alternative<LispNil>())
        return list.elem.size() - 1;
    return list.elem.size();
}

static void collect_pattern_vars(
    SExprLocRef pattern, SExprArena& arena, std::set<std::string>& vars) {
    if (!pattern.is_valid())
        return;
    const auto& expr = arena.at(pattern);
    if (expr.holds_alternative<LispIdent>()) {
        const auto& name = expr.get<LispIdent>()->get().name;
        if (name != "_" && name != "...")
            vars.insert(name);
    } else if (expr.holds_alternative<SExprList>()) {
        const auto& list = expr.get<SExprList>()->get().elem;
        for (const auto& el : list) {
            collect_pattern_vars(el, arena, vars);
        }
    }
}

static bool match(SExprLocRef pattern, SExprArena& pattern_arena,
    SExprLocRef input, SExprArena& input_arena, Bindings& bindings) {
    if (!pattern.is_valid())
        return !input.is_valid();

    const auto& p_expr = pattern_arena.at(pattern);

    if (p_expr.holds_alternative<LispIdent>()) {
        const auto& name = p_expr.get<LispIdent>()->get().name;
        if (name == "_")
            return true;

        bindings[name] = BindingValue::single(input);
        return true;
    }

    if (p_expr.holds_alternative<SExprList>()) {
        if (!input.is_valid())
            return false;
        const auto& i_expr = input_arena.at(input);
        if (!i_expr.holds_alternative<SExprList>())
            return false;

        const auto& p_list = p_expr.get<SExprList>()->get();
        const auto& i_list = i_expr.get<SExprList>()->get();

        std::size_t p_logical = logical_size(p_list, pattern_arena);
        std::size_t i_logical = logical_size(i_list, input_arena);

        int ellipsis_pos = -1;
        for (std::size_t i = 0; i < p_logical; ++i) {
            if (is_ellipsis(p_list.elem[i], pattern_arena)) {
                ellipsis_pos = static_cast<int>(i);
                break;
            }
        }

        if (ellipsis_pos < 0) {
            if (p_logical != i_logical)
                return false;
            for (std::size_t i = 0; i < p_logical; ++i) {
                if (!match(p_list.elem[i], pattern_arena, i_list.elem[i],
                        input_arena, bindings))
                    return false;
            }
            return true;
        }

        if (ellipsis_pos == 0)
            return false;

        auto fixed_before = static_cast<std::size_t>(ellipsis_pos - 1);
        auto fixed_after = p_logical - ellipsis_pos - 1;

        if (i_logical < fixed_before + fixed_after)
            return false;

        for (std::size_t i = 0; i < fixed_before; ++i) {
            if (!match(p_list.elem[i], pattern_arena, i_list.elem[i],
                    input_arena, bindings))
                return false;
        }

        std::size_t repeat_count = i_logical - fixed_before - fixed_after;
        auto repeat_pattern = p_list.elem[ellipsis_pos - 1];

        std::set<std::string> ellipsis_vars;
        collect_pattern_vars(repeat_pattern, pattern_arena, ellipsis_vars);

        for (const auto& var : ellipsis_vars)
            bindings[var] = BindingValue::list({});

        for (std::size_t i = 0; i < repeat_count; ++i) {
            Bindings temp;
            if (!match(repeat_pattern, pattern_arena,
                    i_list.elem[fixed_before + i], input_arena, temp))
                return false;
            for (const auto& [name, val] : temp)
                if (ellipsis_vars.contains(name))
                    for (const auto& v : val.values)
                        bindings[name].values.push_back(v);
        }

        for (std::size_t i = 0; i < fixed_after; ++i) {
            if (!match(p_list.elem[ellipsis_pos + 1 + i], pattern_arena,
                    i_list.elem[i_logical - fixed_after + i], input_arena,
                    bindings))
                return false;
        }

        return true;
    }

    if (!input.is_valid())
        return false;
    const auto& i_expr = input_arena.at(input);
    if (p_expr.index() != i_expr.index())
        return false;

    if (p_expr.holds_alternative<LispNil>())
        return true;
    if (p_expr.holds_alternative<LispString>())
        return p_expr.get<LispString>()->get()
            == i_expr.get<LispString>()->get();
    if (p_expr.holds_alternative<LispNumber>())
        return p_expr.get<LispNumber>()->get()
            == i_expr.get<LispNumber>()->get();
    if (p_expr.holds_alternative<LispChar>())
        return p_expr.get<LispChar>()->get() == i_expr.get<LispChar>()->get();
    if (p_expr.holds_alternative<LispBool>())
        return p_expr.get<LispBool>()->get() == i_expr.get<LispBool>()->get();

    return false;
}

static SExprLocRef instantiate(SExprLocRef element, SExprArena& tmpl_arena,
    SExprArena& output_arena, const Bindings& bindings) {
    if (!element.is_valid())
        return element;

    auto loc = element.loc_ref();
    const auto& expr = tmpl_arena.at(element);

    if (expr.holds_alternative<LispIdent>()) {
        auto ident = expr.get<LispIdent>()->get();
        auto it = bindings.find(ident.name);
        if (it != bindings.end()) {
            if (!it->second.is_list && !it->second.values.empty())
                return it->second.values[0];
            if (!it->second.values.empty())
                return it->second.values[0];
        }
        return output_arena.emplace(loc, std::move(ident));
    }

    if (expr.holds_alternative<SExprList>()) {
        auto elems = expr.get<SExprList>()->get().elem;
        std::size_t tmpl_logical = elems.size();
        if (!elems.empty()
            && tmpl_arena.at(elems.back()).holds_alternative<LispNil>())
            tmpl_logical--;

        int ellipsis_pos = -1;
        for (std::size_t i = 0; i < tmpl_logical; ++i) {
            if (is_ellipsis(elems[i], tmpl_arena)) {
                ellipsis_pos = static_cast<int>(i);
                break;
            }
        }

        std::vector<SExprLocRef> out;

        if (ellipsis_pos < 0) {
            for (const auto& el : elems)
                out.push_back(
                    instantiate(el, tmpl_arena, output_arena, bindings));
        } else if (ellipsis_pos == 0) {
            for (const auto& el : elems)
                out.push_back(
                    instantiate(el, tmpl_arena, output_arena, bindings));
        } else {
            for (int i = 0; i < ellipsis_pos - 1; ++i)
                out.push_back(
                    instantiate(elems[i], tmpl_arena, output_arena, bindings));

            auto repeat_tmpl = elems[ellipsis_pos - 1];

            std::set<std::string> tmpl_vars;
            collect_pattern_vars(repeat_tmpl, tmpl_arena, tmpl_vars);

            std::size_t repeat_count = 0;
            bool found_list = false;
            for (const auto& var : tmpl_vars) {
                auto it = bindings.find(var);
                if (it != bindings.end() && it->second.is_list) {
                    repeat_count = it->second.values.size();
                    found_list = true;
                    break;
                }
            }

            if (found_list) {
                for (std::size_t i = 0; i < repeat_count; ++i) {
                    Bindings temp = bindings;
                    for (auto& [name, val] : temp)
                        if (val.is_list && i < val.values.size())
                            val = BindingValue::single(val.values[i]);
                    out.push_back(instantiate(
                        repeat_tmpl, tmpl_arena, output_arena, temp));
                }
            } else {
                out.push_back(instantiate(
                    repeat_tmpl, tmpl_arena, output_arena, bindings));
            }

            for (std::size_t i = ellipsis_pos + 1; i < elems.size(); ++i)
                out.push_back(
                    instantiate(elems[i], tmpl_arena, output_arena, bindings));
        }

        return output_arena.emplace(loc, SExprList(std::move(out)));
    }

    if (expr.holds_alternative<LispNil>())
        return output_arena.emplace(loc, LispNil());
    if (expr.holds_alternative<LispNumber>()) {
        auto v = expr.get<LispNumber>()->get();
        return output_arena.emplace(loc, v);
    }
    if (expr.holds_alternative<LispBool>()) {
        auto v = expr.get<LispBool>()->get();
        return output_arena.emplace(loc, v);
    }
    if (expr.holds_alternative<LispChar>()) {
        auto v = expr.get<LispChar>()->get();
        return output_arena.emplace(loc, v);
    }
    if (expr.holds_alternative<LispString>()) {
        auto v = expr.get<LispString>()->get();
        return output_arena.emplace(loc, std::move(v));
    }

    return element;
}

SExprLocRef Transformer::transcribe(
    SExprLocRef input, SExprArena& input_arena) const {

    for (const auto& rule : _rules) {
        Bindings bindings;
        if (match(rule.pattern, _def_arena, input, input_arena, bindings)) {
            return instantiate(
                rule.template_, _def_arena, input_arena, bindings);
        }
    }
    return SExprLocRef::invalid();
}
} // namespace lpc::frontend
