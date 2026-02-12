module lpc.frontend.ast;
import std;

namespace lpc::frontend {

template <typename... Ts>
struct SExprVisitor : Ts... {
    using Ts::operator()...;
};

std::string SExprArena::dump_root(SExprRef root) const {
    if (!root.is_valid())
        return "";
    const auto& children = at(root).get_unchecked<SExprList>();
    std::string result;
    for (const auto& child : children.elem) {
        if (at(child).isa<LispNil>())
            continue;
        result += dump(child.expr_ref()) + "\n";
    }
    return result;
}

std::string SExprArena::dump(SExprRef ref) const {
    return at(ref).visit(SExprVisitor { [](const LispIdent& id) {
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
            std::string result = "(";
            if (!list.elem.empty()) {
                for (std::size_t i = 0; i + 1 < list.elem.size(); ++i) {
                    if (i > 0)
                        result += " ";
                    result += dump(list.elem[i].expr_ref());
                }
                if (!at(list.elem.back()).isa<LispNil>()) {
                    if (list.elem.size() > 1)
                        result += " . ";
                    result += dump(list.elem.back().expr_ref());
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
                result += dump(vec.elem[i].expr_ref());
            }
            result += ")";
            return result;
        },
        [](const auto&) { return ""; } });
};

const SExpr& SExprArena::at(SExprRef ref) const& {
    return Arena::at(ref);
}

const SExpr& SExprArena::at(SExprLocRef ref) const& {
    return Arena::at(ref.expr_ref());
}

SExprLocRef SExprArena::nil(LocRef loc) noexcept {
    if (!_nil_node.is_valid())
        _nil_node = Arena::emplace(LispNil());
    return SExprLocRef(_nil_node, loc);
}

SExprLocRef SExprArena::get_boolean(LocRef loc, bool value) noexcept {
    if (value) {
        if (!_boolean_nodes.first.is_valid())
            _boolean_nodes.first = Arena::emplace(LispBool(true));
        return SExprLocRef(_boolean_nodes.first, loc);
    }
    if (!_boolean_nodes.second.is_valid())
        _boolean_nodes.second = Arena::emplace(LispBool(false));
    return SExprLocRef(_boolean_nodes.second, loc);
}

SExprLocRef SExprArena::get_variable(LocRef loc, std::string&& name) noexcept {
    auto [it, inserted] = _variables.try_emplace(name, SExprRef::invalid());
    if (inserted)
        it->second = Arena::emplace(LispIdent(std::move(name)));
    return SExprLocRef(it->second, loc);
}

SExprLocRef Cursor::get_ident() const noexcept {
    if (type() != TokenType::IDENT)
        return SExprLocRef::invalid();
    std::string name = value().get_unchecked<std::string>();
    return arena().get_variable(loc(), std::move(name));
}

SExprLocRef Cursor::get_constant() const noexcept {
    SExprLocRef ref;
    switch (type()) {
    case TokenType::NUMBER: {
        LispNumber v = value().get_unchecked<LispNumber>();
        ref = arena().emplace(loc(), v);
        break;
    }
    case TokenType::BOOLEAN: {
        bool v = value().get_unchecked<LispBool>();
        ref = arena().get_boolean(loc(), v);
        break;
    }
    case TokenType::CHARACTER: {
        char v = value().get_unchecked<LispChar>();
        ref = arena().emplace(loc(), v);
        break;
    }
    case TokenType::STRING: {
        auto v = value().get_unchecked<LispString>();
        ref = arena().emplace(loc(), std::move(v));
        break;
    }
    default:
        break;
    }
    return ref;
}

} // namespace lpc::frontend
