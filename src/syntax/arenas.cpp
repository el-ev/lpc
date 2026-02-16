module lpc.syntax.arenas;

import std;

import lpc.syntax.ast;
import lpc.syntax.span;
import lpc.utils.arena;
import lpc.utils.logging;

namespace lpc::syntax {

using lpc::utils::Error;

namespace {
    template <typename... Ts>
    struct SExprVisitor : Ts... {
        using Ts::operator()...;
    };

    // TODO: better?
    [[nodiscard]] std::pair<std::size_t, std::size_t> find_cycle_suffix(
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
                const auto subrange = seq.subspan(start, L);
                if (!std::ranges::equal(subrange, pattern))
                    break;
                k = j + 1;
            }
            if (k >= 2)
                return { L, k };
        }
        return { 0, 0 };
    }

    void flush_expansion_segment(std::vector<std::string>& segment) {
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
                std::println(
                    std::cerr, "  ({} identical frames omitted)", run - 1);
            i += run;
        }
        if (cycle_len > 0 && num_reps >= 2) {
            for (std::size_t i = 0; i < cycle_len; ++i)
                std::println(std::cerr, "  in expansion of: {}",
                    segment[cycle_start + i]);
            const auto to_omit = (num_reps - 1) * cycle_len;
            if (cycle_len == 1)
                std::println(
                    std::cerr, "  ({} identical frames omitted)", to_omit);
            else
                std::println(
                    std::cerr, "  ({} similar frames omitted)", to_omit);
        }
        segment.clear();
    }
} // namespace

[[nodiscard]] const SExpr& SExprArena::at(SExprRef ref) const& {
    return Arena::at(ref);
}

SExprRef SExprArena::nil() noexcept {
    if (!_nil_node.is_valid())
        _nil_node = Arena::emplace(LispNil());
    return _nil_node;
}

SExprRef SExprArena::get_bool(bool value) noexcept {
    if (value) {
        if (!_bool_nodes.first.is_valid())
            _bool_nodes.first = Arena::emplace(LispBool(true));
        return _bool_nodes.first;
    }
    if (!_bool_nodes.second.is_valid())
        _bool_nodes.second = Arena::emplace(LispBool(false));
    return _bool_nodes.second;
}

SExprRef SExprArena::get_ident(const std::string& name) noexcept {
    if (auto it = _ident_nodes.find(name); it != _ident_nodes.end())
        return it->second;
    auto ref = Arena::emplace(LispIdent(name));
    _ident_nodes[name] = ref;
    return ref;
}

const Span& SpanArena::at(SpanRef ref) const& {
    return Arena::at(ref);
}

void SpanArena::walk(SpanRef ref, const std::function<void(SpanRef)>& f) {
    f(ref);
    if (at(ref).parent().is_valid())
        walk(at(ref).parent(), f);
}

bool SpanArena::report_error(
    SpanRef failed_expr, std::string_view msg, bool show_core) const {
    auto failed_str = dump(failed_expr);

    Error("{}", msg);
    std::println(std::cerr, "  for: {}", failed_str);

    dump_backtrace(at(failed_expr).parent(), show_core);

    auto location = loc(failed_expr);
    std::println(std::cerr, "  at {}", location.source_location());

    return false;
}

void SpanArena::dump_backtrace(SpanRef parent, bool show_core) const {
    struct Frame {
        int core_omitted;
        std::string dump;
    };
    std::vector<Frame> frames;
    frames.reserve(64);
    int core_omitted = 0;
    auto cur = parent;
    while (cur.is_valid()) {
        if (is_core_binding(cur) && !show_core) {
            core_omitted++;
            cur = at(cur).parent();
            continue;
        }
        frames.push_back({ core_omitted, dump(cur) });
        core_omitted = 0;
        cur = at(cur).parent();
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
}

Location SpanArena::loc(SpanRef ref) const noexcept {
    return _loc_arena.at(at(ref).loc());
}

Location SpanArena::loc(LocRef ref) const noexcept {
    return _loc_arena.at(ref);
}

LocRef SpanArena::loc_ref(SpanRef ref) const noexcept {
    return at(ref).loc();
}

const SExpr& SpanArena::expr(SpanRef ref) const noexcept {
    return _expr_arena.at(at(ref).expr());
}

const SExpr& SpanArena::expr(SExprRef ref) const noexcept {
    return _expr_arena.at(ref);
}

SExprRef SpanArena::expr_ref(SpanRef ref) const noexcept {
    return at(ref).expr();
}

const std::set<ScopeID>& SpanArena::scopes(SpanRef ref) const noexcept {
    return _scope_arena.at(at(ref).scopes());
}

ScopeSetRef SpanArena::scope_ref(SpanRef ref) const noexcept {
    return at(ref).scopes();
}

SpanRef SpanArena::nil(
    LocRef loc, SpanRef parent, ScopeSetRef scopes) noexcept {
    if (!scopes.is_valid())
        scopes = parent.is_valid() ? at(parent).scopes()
                                   : _scope_arena.empty_set();
    return emplace(loc, _expr_arena.nil(), parent, scopes);
}

SpanRef SpanArena::get_bool(
    LocRef loc, bool value, SpanRef parent, ScopeSetRef scopes) noexcept {
    if (!scopes.is_valid())
        scopes = parent.is_valid() ? at(parent).scopes()
                                   : _scope_arena.empty_set();
    return emplace(loc, _expr_arena.get_bool(value), parent, scopes);
}

SpanRef SpanArena::get_ident(LocRef loc, const std::string& name,
    SpanRef parent, ScopeSetRef scopes) noexcept {
    if (!scopes.is_valid())
        scopes = parent.is_valid() ? at(parent).scopes()
                                   : _scope_arena.empty_set();
    return emplace(loc, _expr_arena.get_ident(name), parent, scopes);
}

bool SpanArena::is_core_binding(SpanRef ref) const noexcept {
    return loc(ref).file() == " <core> ";
}

std::string SpanArena::dump_root(SpanRef root) const {
    if (!root.is_valid())
        return "";
    const auto* children = expr(root).get<SExprList>();
    std::string result;
    for (const auto& child : children->elem) {
        if (is_nil(child))
            continue;
        result += dump(child) + "\n";
    }
    return result;
}

std::string SpanArena::dump(SpanRef ref) const {
    return expr(ref).visit(SExprVisitor {
        [](const LispIdent& id) { return id.name; },
        [](const LispString& str) { return "\"" + str + "\""; },
        [](const LispNumber& num) { return std::to_string(num); },
        [](const LispChar& c) -> std::string {
            switch (c) {
            case '\n':
                return "#\\newline";
            case ' ':
                return "#\\space";
            default:
                return std::string("#\\") + c;
            }
        },
        [](const LispBool& b) { return b ? "#t" : "#f"; },
        [](const LispNil&) { return "()"; },
        [this](const SExprList& list) {
            if (list.elem.size() == 3 && is_nil(list.elem.back())
                && is_ident(list.elem[0])) {
                const auto& id = expr(list.elem[0]).get_unchecked<LispIdent>();
                if (id->name == "quote")
                    return "'" + dump(list.elem[1]);
                if (id->name == "quasiquote")
                    return "`" + dump(list.elem[1]);
                if (id->name == "unquote")
                    return "," + dump(list.elem[1]);
                if (id->name == "unquote-splicing")
                    return ",@" + dump(list.elem[1]);
            }
            std::string result = "(";
            if (!list.elem.empty()) {
                for (std::size_t i = 0; i + 1 < list.elem.size(); ++i) {
                    if (i > 0)
                        result += " ";
                    result += dump(list.elem[i]);
                }
                if (!is_nil(list.elem.back())) {
                    if (list.elem.size() > 1)
                        result += " . ";
                    result += dump(list.elem.back());
                }
            }
            result += ")";
            return result;
        },
        [this](const SExprVector& vec) {
            std::string result = "#(";
            for (std::size_t i = 0; i < vec.elem.size(); ++i) {
                if (i > 0)
                    result += " ";
                result += dump(vec.elem[i]);
            }
            result += ")";
            return result;
        },
        [](const auto&) { return ""; } });
};

} // namespace lpc::syntax
