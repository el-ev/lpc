module lpc.frontend.transformer;

import std;

import lpc.frontend.ast;

namespace lpc::frontend {

static bool is_ellipsis(SpanRef ref, SpanArena& arena) {
    if (!ref.is_valid())
        return false;
    if (const auto* expr = arena.get<LispIdent>(ref))
        return expr->name == "...";
    return false;
}

static std::size_t logical_size(const SExprList& list, SpanArena& arena) {
    if (arena.is_nil(list.elem.back()))
        return list.elem.size() - 1;
    return list.elem.size();
}

static void collect_pattern_vars(
    SpanRef pattern, SpanArena& arena, std::set<std::string>& vars) {
    if (!pattern.is_valid())
        return;
    if (const auto* ident = arena.get<LispIdent>(pattern)) {
        if (ident->name != "_" && ident->name != "...")
            vars.insert(ident->name);
    } else if (const auto* list = arena.get<SExprList>(pattern)) {
        for (const auto& el : list->elem)
            collect_pattern_vars(el, arena, vars);
    }
}

static SpanRef get_tail(
    const SExprList& list, std::size_t start, SpanArena& arena, SpanRef parent) {
    bool is_improper = !list.elem.empty() && !arena.is_nil(list.elem.back());
    if (start >= list.elem.size()) {
        if (is_improper)
            return list.elem.back();
        return arena.nil(arena.loc_ref(list.elem.back()), parent);
    }

    if (is_improper && start == list.elem.size() - 1)
        return list.elem.back();

    std::vector<SpanRef> subset;
    subset.reserve(list.elem.size() - start);
    for (std::size_t i = start; i < list.elem.size(); ++i)
        subset.push_back(list.elem[i]);

    return arena.expand(arena[list.elem.back()].loc(),
        SExpr(SExprList(std::move(subset))), parent);
}

bool Transformer::match(
    SpanRef pattern, SpanRef input, Bindings& bindings) const {
    if (!pattern.is_valid() || !input.is_valid())
        return false;

    const auto& p_expr = _arena.expr(pattern);
    const auto& i_expr = _arena.expr(input);

    if (const auto* p_id = p_expr.get<LispIdent>()) {
        if (p_id->name == "_")
            return true;

        if (_literals.contains(p_id->name)) {
            if (const auto* i_id = i_expr.get<LispIdent>())
                if (i_id->name == p_id->name)
                    return true;
            return false;
        }

        bindings[p_id->name] = BindingValue::single(input);
        return true;
    }

    if (const auto* p_list = p_expr.get<SExprList>()) {
        const auto* i_list = i_expr.get<SExprList>();
        if (i_list == nullptr)
            return false;

        std::size_t p_logical = logical_size(*p_list, _arena);
        std::size_t i_logical = logical_size(*i_list, _arena);

        int ellipsis_pos = -1;
        for (std::size_t i = 0; i < p_logical; ++i) {
            if (is_ellipsis(p_list->elem[i], _arena)) {
                ellipsis_pos = static_cast<int>(i);
                break;
            }
        }

        bool p_improper
            = !p_list->elem.empty() && !_arena.is_nil(p_list->elem.back());
        bool i_improper
            = !i_list->elem.empty() && !_arena.is_nil(i_list->elem.back());

        if (!p_improper) {
            if (i_improper)
                return false;

            if (ellipsis_pos < 0) {
                if (p_logical != i_logical)
                    return false;
                for (std::size_t i = 0; i < p_logical; ++i)
                    if (!match(p_list->elem[i], i_list->elem[i], bindings))
                        return false;
                return true;
            }
            if (ellipsis_pos == 0)
                return false;

            auto fixed_before = static_cast<std::size_t>(ellipsis_pos - 1);
            auto fixed_after = p_logical - ellipsis_pos - 1;

            if (i_logical < fixed_before + fixed_after)
                return false;

            for (std::size_t i = 0; i < fixed_before; ++i)
                if (!match(p_list->elem[i], i_list->elem[i], bindings))
                    return false;

            std::size_t repeat_count = i_logical - fixed_before - fixed_after;
            auto repeat_pattern = p_list->elem[ellipsis_pos - 1];

            std::set<std::string> ellipsis_vars;
            collect_pattern_vars(repeat_pattern, _arena, ellipsis_vars);

            for (const auto& var : ellipsis_vars)
                bindings[var] = BindingValue::list({});

            for (std::size_t i = 0; i < repeat_count; ++i) {
                Bindings temp;
                if (!match(
                        repeat_pattern, i_list->elem[fixed_before + i], temp))
                    return false;
                for (const auto& [name, val] : temp)
                    if (ellipsis_vars.contains(name))
                        for (const auto& v : val.values)
                            bindings[name].values.push_back(v);
            }

            for (std::size_t i = 0; i < fixed_after; ++i) {
                if (!match(p_list->elem[ellipsis_pos + 1 + i],
                        i_logical - fixed_after + i < i_list->elem.size()
                            ? i_list->elem[i_logical - fixed_after + i]
                            : SpanRef::invalid(),
                        bindings))
                    return false;
            }

            return true;
        }
        // Improper pattern
        auto head_count = p_logical - 1;
        auto tail_pattern = p_list->elem.back();
        std::size_t i_element_count = i_improper ? i_logical - 1 : i_logical;

        if (ellipsis_pos < 0) {
            if (i_element_count < head_count)
                return false;

            for (std::size_t i = 0; i < head_count; ++i)
                if (!match(p_list->elem[i], i_list->elem[i], bindings))
                    return false;
            return match(
                tail_pattern, get_tail(*i_list, head_count, _arena, SpanRef::invalid()), bindings);
        }
        if (ellipsis_pos == 0)
            return false;

        auto fixed_before = static_cast<std::size_t>(ellipsis_pos - 1);
        auto fixed_after = head_count - ellipsis_pos - 1;

        if (i_element_count < fixed_before + fixed_after)
            return false;

        for (std::size_t i = 0; i < fixed_before; ++i)
            if (!match(p_list->elem[i], i_list->elem[i], bindings))
                return false;

        std::size_t repeat_count = i_element_count - fixed_before - fixed_after;
        auto repeat_pattern = p_list->elem[ellipsis_pos - 1];

        std::set<std::string> ellipsis_vars;
        collect_pattern_vars(repeat_pattern, _arena, ellipsis_vars);
        for (const auto& var : ellipsis_vars)
            bindings[var] = BindingValue::list({});

        for (std::size_t i = 0; i < repeat_count; ++i) {
            Bindings temp;
            if (!match(repeat_pattern, i_list->elem[fixed_before + i], temp))
                return false;
            for (const auto& [name, val] : temp)
                if (ellipsis_vars.contains(name))
                    for (const auto& v : val.values)
                        bindings[name].values.push_back(v);
        }

        for (std::size_t i = 0; i < fixed_after; ++i)
            if (!match(p_list->elem[ellipsis_pos + 1 + i],
                    i_list->elem[i_element_count - fixed_after + i], bindings))
                return false;

        return match(
            tail_pattern, get_tail(*i_list, i_element_count, _arena, SpanRef::invalid()), bindings);
    }

    return p_expr == i_expr;
}

SpanRef Transformer::instantiate(
    SpanRef element, const Bindings& bindings, LocRef call_site_loc, SpanRef parent) const {
    if (!element.is_valid())
        return element;

    const auto expr = _arena.expr(element);

    if (const auto* ident = expr.get<LispIdent>()) {
        auto it = bindings.find(ident->name);
        if (it != bindings.end() && !it->second.values.empty())
            return it->second.values[0];
        return element;
    }

    if (const auto* list = expr.get<SExprList>()) {
        std::size_t tmpl_logical = list->elem.size();
        if (!list->elem.empty() && _arena.is_nil(list->elem.back()))
            tmpl_logical--;

        std::vector<SpanRef> out;

        auto elems = list->elem;
        for (std::size_t i = 0; i < elems.size(); ++i) {
            if (i + 1 < tmpl_logical && is_ellipsis(elems[i + 1], _arena)) {
                auto repeat_tmpl = elems[i];
                std::set<std::string> tmpl_vars;
                collect_pattern_vars(repeat_tmpl, _arena, tmpl_vars);

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
                    for (std::size_t j = 0; j < repeat_count; ++j) {
                        Bindings temp = bindings;
                        for (auto& [name, val] : temp)
                            if (val.is_list && j < val.values.size())
                                val = BindingValue::single(val.values[j]);
                        out.push_back(
                            instantiate(repeat_tmpl, temp, call_site_loc, parent));
                    }
                } else {
                    out.push_back(
                        instantiate(repeat_tmpl, bindings, call_site_loc, parent));
                }
                ++i;
            } else if (is_ellipsis(elems[i], _arena)) {
            } else {
                out.push_back(instantiate(elems[i], bindings, call_site_loc, parent));
            }
        }

        return _arena.expand(call_site_loc, SExpr(SExprList(std::move(out))), parent);
    }

    return element;
}

SpanRef Transformer::transcribe(SpanRef input, SpanRef parent) const {
    const auto* i_list = _arena.get<SExprList>(input);
    if (i_list == nullptr || i_list->elem.empty())
        return SpanRef::invalid();
    auto i_tail = get_tail(*i_list, 1, _arena, parent);
    for (const auto& rule : _rules) {
        Bindings bindings;
        if (match(rule.pattern_tail, i_tail, bindings)) {
            return instantiate(rule.template_, bindings, _arena.loc_ref(input), parent);
        }
    }
    return SpanRef::invalid();
}
} // namespace lpc::frontend
