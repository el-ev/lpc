module lpc.frontend.transformer;

import lpc.frontend.ast;
import std;

namespace lpc::frontend {

static bool is_ellipsis(SExprLocRef ref, SExprArena& arena) {
    if (!ref.is_valid())
        return false;
    const auto& expr = arena.at(ref);
    if (!expr.isa<LispIdent>())
        return false;
    return expr.get_unchecked<LispIdent>().name == "...";
}

static std::size_t logical_size(const SExprList& list, SExprArena& arena) {
    if (list.elem.empty())
        return 0; // Should never happen
    if (arena.at(list.elem.back()).isa<LispNil>())
        return list.elem.size() - 1;
    return list.elem.size();
}

static void collect_pattern_vars(
    SExprLocRef pattern, SExprArena& arena, std::set<std::string>& vars) {
    if (!pattern.is_valid())
        return;
    const auto& expr = arena.at(pattern);
    if (expr.isa<LispIdent>()) {
        const auto& name = expr.get_unchecked<LispIdent>().name;
        if (name != "_" && name != "...")
            vars.insert(name);
    } else if (expr.isa<SExprList>()) {
        const auto& list = expr.get_unchecked<SExprList>().elem;
        for (const auto& el : list) {
            collect_pattern_vars(el, arena, vars);
        }
    }
}

static SExprLocRef get_tail(
    const SExprList& list, std::size_t start, SExprArena& arena) {
    if (start >= list.elem.size()) {
        if (!list.elem.empty() && !arena.at(list.elem.back()).isa<LispNil>()) {
            return list.elem.back();
        }
        return arena.nil(SExprLocRef::invalid().loc_ref());
    }

    bool is_improper
        = !list.elem.empty() && !arena.at(list.elem.back()).isa<LispNil>();

    if (is_improper && start == list.elem.size() - 1) {
        return list.elem.back();
    }

    std::vector<SExprLocRef> subset;
    subset.reserve(list.elem.size() - start);
    for (std::size_t i = start; i < list.elem.size(); ++i) {
        subset.push_back(list.elem[i]);
    }
    return arena.emplace(
        SExprLocRef::invalid().loc_ref(), SExprList(std::move(subset)));
}

bool Transformer::match(
    SExprLocRef pattern, SExprLocRef input, Bindings& bindings) const {
    if (!pattern.is_valid() || !input.is_valid())
        return false;

    const auto& p_expr = _arena.at(pattern);

    if (p_expr.isa<LispIdent>()) {
        const auto& name = p_expr.get_unchecked<LispIdent>().name;
        if (name == "_")
            return true;

        if (_literals.contains(name)) {
            const auto& i_expr = _arena.at(input);
            if (!i_expr.isa<LispIdent>())
                return false;
            return i_expr.get_unchecked<LispIdent>().name == name;
        }

        bindings[name] = BindingValue::single(input);
        return true;
    }

    if (p_expr.isa<SExprList>()) {
        if (!input.is_valid())
            return false;
        const auto& i_expr = _arena.at(input);
        if (!i_expr.isa<SExprList>())
            return false;

        const auto& p_list = p_expr.get_unchecked<SExprList>();
        const auto& i_list = i_expr.get_unchecked<SExprList>();

        std::size_t p_logical = logical_size(p_list, _arena);
        std::size_t i_logical = logical_size(i_list, _arena);

        int ellipsis_pos = -1;
        for (std::size_t i = 0; i < p_logical; ++i) {
            if (is_ellipsis(p_list.elem[i], _arena)) {
                ellipsis_pos = static_cast<int>(i);
                break;
            }
        }

        bool p_improper = !p_list.elem.empty()
            && !_arena.at(p_list.elem.back()).isa<LispNil>();
        bool i_improper = !i_list.elem.empty()
            && !_arena.at(i_list.elem.back()).isa<LispNil>();

        if (!p_improper) {
            if (i_improper)
                return false;

            if (ellipsis_pos < 0) {
                if (p_logical != i_logical)
                    return false;
                for (std::size_t i = 0; i < p_logical; ++i) {
                    if (!match(p_list.elem[i], i_list.elem[i], bindings))
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
                if (!match(p_list.elem[i], i_list.elem[i], bindings))
                    return false;
            }

            std::size_t repeat_count = i_logical - fixed_before - fixed_after;
            auto repeat_pattern = p_list.elem[ellipsis_pos - 1];

            std::set<std::string> ellipsis_vars;
            collect_pattern_vars(repeat_pattern, _arena, ellipsis_vars);

            for (const auto& var : ellipsis_vars)
                bindings[var] = BindingValue::list({});

            for (std::size_t i = 0; i < repeat_count; ++i) {
                Bindings temp;
                if (!match(repeat_pattern, i_list.elem[fixed_before + i], temp))
                    return false;
                for (const auto& [name, val] : temp)
                    if (ellipsis_vars.contains(name))
                        for (const auto& v : val.values)
                            bindings[name].values.push_back(v);
            }

            for (std::size_t i = 0; i < fixed_after; ++i) {
                if (!match(p_list.elem[ellipsis_pos + 1 + i],
                        i_logical - fixed_after + i < i_list.elem.size()
                            ? i_list.elem[i_logical - fixed_after + i]
                            : SExprLocRef::invalid(),
                        bindings))
                    return false;
            }

            return true;
        } 
        // Improper pattern
        auto head_count = p_logical - 1;
        auto tail_pattern = p_list.elem.back();
        std::size_t i_element_count = i_improper ? i_logical - 1 : i_logical;

        if (ellipsis_pos < 0) {
            if (i_element_count < head_count)
                return false;

            for (std::size_t i = 0; i < head_count; ++i) {
                if (!match(p_list.elem[i], i_list.elem[i], bindings))
                    return false;
            }
            return match(
                tail_pattern, get_tail(i_list, head_count, _arena), bindings);
        }
        if (ellipsis_pos == 0)
            return false;

        auto fixed_before = static_cast<std::size_t>(ellipsis_pos - 1);
        auto fixed_after = head_count - ellipsis_pos - 1;

        if (i_element_count < fixed_before + fixed_after)
            return false;

        for (std::size_t i = 0; i < fixed_before; ++i) {
            if (!match(p_list.elem[i], i_list.elem[i], bindings))
                return false;
        }

        std::size_t repeat_count = i_element_count - fixed_before - fixed_after;
        auto repeat_pattern = p_list.elem[ellipsis_pos - 1];

        std::set<std::string> ellipsis_vars;
        collect_pattern_vars(repeat_pattern, _arena, ellipsis_vars);
        for (const auto& var : ellipsis_vars)
            bindings[var] = BindingValue::list({});

        for (std::size_t i = 0; i < repeat_count; ++i) {
            Bindings temp;
            if (!match(repeat_pattern, i_list.elem[fixed_before + i], temp))
                return false;
            for (const auto& [name, val] : temp)
                if (ellipsis_vars.contains(name))
                    for (const auto& v : val.values)
                        bindings[name].values.push_back(v);
        }

        for (std::size_t i = 0; i < fixed_after; ++i) {
            if (!match(p_list.elem[ellipsis_pos + 1 + i],
                    i_list.elem[i_element_count - fixed_after + i], bindings))
                return false;
        }

        return match(
            tail_pattern, get_tail(i_list, i_element_count, _arena), bindings);
    }

    if (!input.is_valid())
        return false;
    const auto& i_expr = _arena.at(input);

    return p_expr == i_expr;
}

SExprLocRef Transformer::instantiate(
    SExprLocRef element, const Bindings& bindings, LocRef call_site_loc) const {
    if (!element.is_valid())
        return element;

    const auto& expr = _arena.at(element);

    if (expr.isa<LispIdent>()) {
        auto ident = expr.get_unchecked<LispIdent>();
        auto it = bindings.find(ident.name);
        if (it != bindings.end() && !it->second.values.empty())
            return it->second.values[0];
        return element;
    }

    if (expr.isa<SExprList>()) {
        auto elems = expr.get_unchecked<SExprList>().elem;
        std::size_t tmpl_logical = elems.size();
        if (!elems.empty() && _arena.at(elems.back()).isa<LispNil>())
            tmpl_logical--;

        std::vector<SExprLocRef> out;

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
                            instantiate(repeat_tmpl, temp, call_site_loc));
                    }
                } else {
                    out.push_back(
                        instantiate(repeat_tmpl, bindings, call_site_loc));
                }
                ++i;
            } else if (is_ellipsis(elems[i], _arena)) {
            } else {
                out.push_back(instantiate(elems[i], bindings, call_site_loc));
            }
        }

        return _arena.emplace(call_site_loc, SExprList(std::move(out)));
    }

    return element;
}

SExprLocRef Transformer::transcribe(SExprLocRef input) const {
    const auto& i_list = _arena.at(input).get_unchecked<SExprList>();
    if (i_list.elem.empty())
        return SExprLocRef::invalid();
    auto i_tail = get_tail(i_list, 1, _arena);
    for (const auto& rule : _rules) {
        Bindings bindings;
        if (match(rule.pattern_tail, i_tail, bindings)) {
            return instantiate(rule.template_, bindings, input.loc_ref());
        }
    }
    return SExprLocRef::invalid();
}
} // namespace lpc::frontend
