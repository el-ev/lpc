module lpc.frontend.ast;

namespace lpc::frontend {

std::string SExprArena::dump_root(SExprRef root) const {
    if (!root.is_valid())
        return "";
    const auto& children = at(root).get_unchecked<SExprList>();
    std::string result;
    for (const auto& child : children.elem) {
        result += dump(child.expr_ref()) + "\n";
    }
    return result;
}

std::string SExprArena::dump(SExprRef ref) const {
    const SExpr& s = at(ref);
    if (s.holds_alternative<LispNil>()) {
        // normally won't happen
    } else if (s.holds_alternative<LispIdent>()) {
        return s.get_unchecked<LispIdent>().name;
    } else if (s.holds_alternative<LispString>()) {
        return "\"" + s.get_unchecked<LispString>() + "\"";
    } else if (s.holds_alternative<LispNumber>()) {
        return std::to_string(s.get_unchecked<LispNumber>());
    } else if (s.holds_alternative<LispChar>()) {
        char c = s.get_unchecked<LispChar>();
        switch (c) {
        case '\n':
            return "#\\newline";
        case ' ':
            return "#\\space";
        default:
            return std::string("#\\") + c;
        }
    } else if (s.holds_alternative<LispBool>()) {
        return s.get_unchecked<LispBool>() ? "#t" : "#f";
    } else if (s.holds_alternative<SExprList>()) {
        const auto& children = s.get_unchecked<SExprList>();
        std::string result = "(";
        for (std::size_t i = 0; i < children.elem.size() - 1; ++i) {
            if (i > 0)
                result += " ";
            result += dump(children.elem[i].expr_ref());
        }
        if (!at(children.elem.back()).holds_alternative<LispNil>()) {
            if (children.elem.size() > 1)
                result += " . ";
            result += dump(children.elem.back().expr_ref());
        }
        result += ")";
        return result;
    }

    return "";
};
const SExpr& SExprArena::at(SExprRef ref) const& {
    return Arena::at(ref);
}

const SExpr& SExprArena::at(SExprLocRef ref) const& {
    return Arena::at(ref.expr_ref());
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
