module lpc.frontend.ast;

namespace lpc::frontend {

using NodeList = std::vector<NodeLocRef>;

std::string NodeArena::dump_json(NodeLocRef ref, std::size_t indent) const {
    std::string result;
    std::string prefix(indent, ' ');

    const ASTNode& node = at(ref);
    const auto& value = node.value();

    result += prefix + "{\n";
    result += prefix + R"(  "type": ")" + node_type_to_string(node.type())
        + "\",\n";
    result += prefix + R"(  "location": ")" + location(ref).source_location()
        + "\"";

    switch (node.type()) {
    case NodeType::Variable:
    case NodeType::String:
        result += ",\n" + prefix + R"(  "value": ")";
        for (char c : value.get_unchecked<std::string>()) {
            switch (c) {
            case '"':
                result += "\\\"";
                break;
            case '\\':
                result += "\\\\";
                break;
            default:
                result += c;
                break;
            }
        }
        result += "\"";
        break;
    case NodeType::Character: {
        char c = value.get_unchecked<char>();
        result += ",\n" + prefix + R"(  "value": "#\\)";
        switch (c) {
        case '\n':
            result += "newline";
            break;
        case ' ':
            result += "space";
            break;
        default:
            result += c;
            break;
        }
        result += "\"";
        break;
    }
    case NodeType::Number:
        result += ",\n" + prefix + "  \"value\": "
            + std::to_string(value.get_unchecked<std::int64_t>());
        break;
    case NodeType::Boolean:
        result += ",\n" + prefix + "  \"value\": "
            + (value.get_unchecked<bool>() ? "\"#t\"" : "\"#f\"");
        break;
    case NodeType::Keyword:
        result += ",\n" + prefix + R"(  "value": ")"
            + std::string(lex_defs::KEYWORDS[static_cast<std::size_t>(
                value.get_unchecked<Keyword>())])
            + "\"";
        break;
    case NodeType::Nil:
        break;
    default:
        result += ",\n" + prefix + "  \"children\": [\n";
        const auto& children = value.get_unchecked<NodeList>();
        for (std::size_t i = 0; i < children.size(); ++i) {
            result += dump_json(children[i], indent + 4);
            if (i < children.size() - 1) {
                result += ",";
            }
            result += "\n";
        }
        result += prefix + "  ]";
        break;
    }

    result += "\n" + prefix + "}";
    return result;
}

std::string NodeArena::dump(NodeLocRef ref) const {
    const ASTNode& node = at(ref);
    const auto& value = node.value();

    switch (node.type()) {
    case NodeType::Variable:
        return value.get_unchecked<std::string>();
    case NodeType::String:
        return "\"" + value.get_unchecked<std::string>() + "\"";
    case NodeType::Character: {
        char c = value.get_unchecked<char>();
        switch (c) {
        case '\n':
            return "#\\newline";
        case ' ':
            return "#\\space";
        default:
            return std::string("#\\") + c;
        }
    }
    case NodeType::Number:
        return std::to_string(value.get_unchecked<std::int64_t>());
    case NodeType::Boolean:
        return value.get_unchecked<bool>() ? "#t" : "#f";
    case NodeType::Keyword:
        return std::string(lex_defs::KEYWORDS[static_cast<std::size_t>(
            value.get_unchecked<Keyword>())]);
    case NodeType::ProcedureCall:
    case NodeType::List: {
        const auto& children = value.get_unchecked<NodeList>();
        std::string result = "(";
        for (std::size_t i = 0; i < children.size() - 1; ++i) {
            if (i > 0)
                result += " ";
            result += dump(children[i]);
        }
        if (!at(children.back()).is<NodeType::Nil>())
            result += " . " + dump(children.back());
        result += ")";
        return result;
    }
    case NodeType::Vector: {
        const auto& children = value.get_unchecked<NodeList>();
        std::string result = "#(";
        for (std::size_t i = 0; i < children.size(); ++i) {
            if (i > 0)
                result += " ";
            result += dump(children[i]);
        }
        result += ")";
        return result;
    }
    case NodeType::Program: {
        const auto& children = value.get_unchecked<NodeList>();
        std::string result;
        for (auto i : children)
            result += dump(i) + "\n";
        return result;
    }
    case NodeType::Definition: {
        const auto& children = value.get_unchecked<NodeList>();
        std::string def_str
            = "(define " + dump(children[0]) + " " + dump(children[1]) + ")";
        return def_str;
    }
    case NodeType::Lambda: {
        const auto& children = value.get_unchecked<NodeList>();
        std::string lambda_str = "(lambda";
        for (auto i : children)
            lambda_str += " " + dump(i);
        lambda_str += ")";
        return lambda_str;
    }
    case NodeType::Quotation:
        return "'" + dump(value.get_unchecked<NodeList>()[0]);
    case NodeType::Nil:
        return "()";
    default:
        return "";
    }
}

const ASTNode& NodeArena::at(NodeLocRef ref) const& {
    return Arena::at(ref.node_ref());
}

const ASTNode* NodeArena::get(NodeLocRef ref) const noexcept {
    return Arena::get(ref.node_ref());
}

NodeLocRef NodeArena::get_keyword(LocRef loc, Keyword keyword) noexcept {
    if (!_keywords[static_cast<std::size_t>(keyword)].is_valid())
        _keywords[static_cast<std::size_t>(keyword)]
            = Arena::emplace(NodeType::Keyword, keyword);
    return NodeLocRef(_keywords[static_cast<std::size_t>(keyword)], loc);
}

NodeLocRef NodeArena::get_variable(LocRef loc, std::string&& name) noexcept {
    auto [it, inserted]
        = _variables.try_emplace(name, NodeArena::ASTNodeRef::invalid());
    if (inserted)
        it->second = Arena::emplace(NodeType::Variable, std::move(name));
    return NodeLocRef(it->second, loc);
}

NodeLocRef NodeArena::get_boolean(LocRef loc, bool value) noexcept {
    if (value) {
        if (!_boolean_nodes.first.is_valid())
            _boolean_nodes.first = Arena::emplace(NodeType::Boolean, true);
        return NodeLocRef(_boolean_nodes.first, loc);
    }
    if (!_boolean_nodes.second.is_valid()) {
        _boolean_nodes.second = Arena::emplace(NodeType::Boolean, false);
    }
    return NodeLocRef(_boolean_nodes.second, loc);
}

NodeLocRef Cursor::get_keyword() const noexcept {
    if (type() != TokenType::KEYWORD)
        return NodeLocRef::invalid();
    Keyword keyword = value().get_unchecked<Keyword>();
    return arena().get_keyword(loc(), keyword);
}

NodeLocRef Cursor::get_ident() const noexcept {
    if (type() != TokenType::IDENT)
        return NodeLocRef::invalid();
    std::string name = value().get_unchecked<std::string>();
    return arena().get_variable(loc(), std::move(name));
}

NodeLocRef Cursor::get_constant() const noexcept {
    NodeLocRef ref = NodeLocRef::invalid();
    switch (type()) {
    case TokenType::NUMBER: {
        std::int64_t v = value().get_unchecked<std::int64_t>();
        ref = arena().emplace(loc(), NodeType::Number, v);
        break;
    }
    case TokenType::BOOLEAN: {
        bool v = value().get_unchecked<bool>();
        ref = arena().get_boolean(loc(), v);
        break;
    }
    case TokenType::CHARACTER: {
        char v = value().get_unchecked<char>();
        ref = arena().emplace(loc(), NodeType::Character, v);
        break;
    }
    case TokenType::STRING: {
        auto v = value().get_unchecked<std::string>();
        ref = arena().emplace(loc(), NodeType::String, std::move(v));
        break;
    }
    default:
        break;
    }
    return ref;
}
} // namespace lpc::frontend
