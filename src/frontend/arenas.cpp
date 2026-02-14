module lpc.frontend.arenas;

import std;

import lpc.frontend.ast;
import lpc.frontend.span;
import lpc.utils.arena;

namespace lpc::frontend {

namespace {
    template <typename... Ts>
    struct SExprVisitor : Ts... {
        using Ts::operator()...;
    };
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

SpanRef SpanArena::from_loc(LocRef loc, SExpr&& expr) {
    auto expr_ref = _expr_arena.emplace(std::move(expr));
    return emplace(loc, expr_ref, SpanRef::invalid());
}

SpanRef SpanArena::expand(LocRef loc, SExpr&& expr, SpanRef parent) {
    auto expr_ref = _expr_arena.emplace(std::move(expr));
    return emplace(loc, expr_ref, parent);
}

const Span& SpanArena::at(SpanRef ref) const& {
    return Arena::at(ref);
}

void SpanArena::walk(SpanRef ref, const std::function<void(SpanRef)>& f) {
    f(ref);
    if (at(ref).parent().is_valid())
        walk(at(ref).parent(), f);
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

SpanRef SpanArena::nil(LocRef loc, SpanRef parent) noexcept {
    return emplace(loc, _expr_arena.nil(), parent);
}

SpanRef SpanArena::get_bool(LocRef loc, bool value, SpanRef parent) noexcept {
    return emplace(loc, _expr_arena.get_bool(value), parent);
}

// FIXME
bool SpanArena::is_core_binding(SpanRef ref) const noexcept {
    return location_arena().file_idx(at(ref).loc()) == 1;
}

std::string SpanArena::dump_root(SpanRef root) const {
    if (!root.is_valid())
        return "";
    const auto* children = expr(root).get<SExprList>();
    std::string result;
    for (const auto& child : children->elem) {
        if (isa<LispNil>(child))
            continue;
        result += dump(child) + "\n";
    }
    return result;
}

std::string SpanArena::dump(SpanRef ref) const {
    return expr(ref).visit(SExprVisitor { [](const LispIdent& id) {
                                             std::string res = id.name;
                                             //    The indentifer is resolved in
                                             //    ExpandPass. If we don't dump
                                             //    extensively there, omitting
                                             //    scopes is fine.

                                             return res;
                                         },
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
        [this](const SExprList& list) {
            if (list.elem.size() == 3 && isa<LispNil>(list.elem.back())
                && expr(list.elem[0]).isa<LispIdent>()) {
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
                if (!isa<LispNil>(list.elem.back())) {
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

} // namespace lpc::frontend
