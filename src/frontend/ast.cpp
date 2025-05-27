module lpc.frontend.ast;

namespace lpc::frontend {

std::string ASTNode::dump_json(std::size_t indent) const {
    std::string result;
    std::string prefix(indent, ' ');

    result += prefix + "{\n";
    result += prefix + R"(  "type": ")" + node_type_to_string(_type) + "\",\n";
    result += prefix + R"(  "location": ")" + _location.to_string() + "\"";

    switch (_type) {
    case NodeType::Variable:
    case NodeType::String:
        result += ",\n" + prefix + R"(  "value": ")";
        for (char c : std::get<std::string>(_value)) {
            switch (c) {
            case '"' : result += "\\\""; break;
            case '\\': result += "\\\\"; break;
            default  : result += c; break;
            }
        }
        result += "\"";
        break;
    case NodeType::Character: {
        char c = std::get<char>(_value);
        result += ",\n" + prefix + R"(  "value": "#\)";
        switch (c) {
        case '\n': result += "newline"; break;
        case ' ' : result += "space"; break;
        default  : result += c; break;
        }
        result += "\"";
    } break;
    case NodeType::Number:
        result += ",\n" + prefix
            + "  \"value\": " + std::to_string(std::get<std::int64_t>(_value));
        break;
    case NodeType::Boolean:
        result += ",\n" + prefix
            + "  \"value\": " + (std::get<bool>(_value) ? "\"#t\"" : "\"#f\"");
        break;
    case NodeType::Keyword:
        result += ",\n" + prefix + R"(  "value": ")"
            + std::string(lex_defs::KEYWORDS[static_cast<std::size_t>(
                std::get<Keyword>(_value))])
            + "\"";
        break;
    default:
        result += ",\n" + prefix + "  \"children\": [\n";
        for (std::size_t i = 0; i < _children.size(); ++i) {
            result += _children[i]->dump_json(indent + 4);
            if (i < _children.size() - 1) {
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
} // namespace lpc::frontend
