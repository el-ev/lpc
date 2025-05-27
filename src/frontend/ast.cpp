module lpc.frontend.ast;

namespace lpc::frontend {

std::string ASTNode::dump_json(const ASTNodeArena& arena,
    const LocationArena& loc_arena, std::size_t indent) const {
    std::string result;
    std::string prefix(indent, ' ');

    result += prefix + "{\n";
    result += prefix + R"(  "type": ")" + node_type_to_string(_type) + "\",\n";
    result += prefix + R"(  "location": ")" + loc_arena[_location].to_string()
        + "\"";

    switch (_type) {
    case NodeType::Variable:
    case NodeType::String:
        result += ",\n" + prefix + R"(  "value": ")";
        for (char c : _value.get_unchecked<std::string>()) {
            switch (c) {
            case '"' : result += "\\\""; break;
            case '\\': result += "\\\\"; break;
            default  : result += c; break;
            }
        }
        result += "\"";
        break;
    case NodeType::Character: {
        char c = _value.get_unchecked<char>();
        result += ",\n" + prefix + R"(  "value": "#\\)";
        switch (c) {
        case '\n': result += "newline"; break;
        case ' ' : result += "space"; break;
        default  : result += c; break;
        }
        result += "\"";
        break;
    }
    case NodeType::Number:
        result += ",\n" + prefix + "  \"value\": "
            + std::to_string(_value.get_unchecked<std::int64_t>());
        break;
    case NodeType::Boolean:
        result += ",\n" + prefix + "  \"value\": "
            + (_value.get_unchecked<bool>() ? "\"#t\"" : "\"#f\"");
        break;
    case NodeType::Keyword:
        result += ",\n" + prefix + R"(  "value": ")"
            + std::string(lex_defs::KEYWORDS[static_cast<std::size_t>(
                _value.get_unchecked<Keyword>())])
            + "\"";
        break;
    default:
        result += ",\n" + prefix + "  \"children\": [\n";
        const auto& children = _value.get_unchecked<NodeList>();
        for (std::size_t i = 0; i < children.size(); ++i) {
            result
                += arena[children[i]].dump_json(arena, loc_arena, indent + 4);
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

[[nodiscard]] ASTNodeArena::NodeRef ASTNodeArena::emplace(ASTNode&& node) {
    return Arena::emplace(std::move(node));
}

[[nodiscard]] ASTNodeArena::NodeRef ASTNodeArena::back_ref() const noexcept {
    return Arena::back_ref();
}

[[nodiscard]] const ASTNode& ASTNodeArena::at(NodeRef ref) const& {
    return Arena::at(ref);
}

[[nodiscard]] const ASTNode* ASTNodeArena::get(NodeRef ref) const noexcept {
    return Arena::get(ref);
}
} // namespace lpc::frontend
