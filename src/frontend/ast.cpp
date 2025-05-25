module lpc.frontend.ast;

import std;
import lpc.frontend.token;

namespace lpc::frontend {

[[nodiscard]] std::string ASTNode::dump(std::size_t indent) const {
    std::string result;
    std::string prefix(indent, ' ');

    result += prefix + node_type_to_string(_type);

    if (is_terminal()) {
        const auto* terminal = dynamic_cast<const TerminalASTNode*>(this);
        result += ": ";
        std::visit(
            [&result](const auto& value) {
                using T = std::decay_t<decltype(value)>;
                if constexpr (std::is_same_v<T, std::string>) {
                    result += "\"" + value + "\"";
                } else if constexpr (std::is_same_v<T, char>) {
                    if (value == '\n')
                        result += "'\\n'";
                    else if (value == ' ')
                        result += "'\\s'";
                    else
                        result += "'" + std::string(1, value) + "'";
                } else if constexpr (std::is_same_v<T, bool>) {
                    result += value ? "#t" : "#f";
                } else if constexpr (std::is_same_v<T, Keyword>) {
                    result += std::string(
                        lex_defs::KEYWORDS[static_cast<std::size_t>(value)]);
                } else {
                    result += std::to_string(value);
                }
            },
            terminal->token().value());
    }

    result += " @" + _location.to_string() + "\n";

    for (const auto& child : _children)
        result += child->dump(indent + 2);

    return result;
}

std::string ASTNode::dump_json(std::size_t indent) const {
    std::string result;
    std::string prefix(indent, ' ');

    result += prefix + "{\n";
    result += prefix + R"(  "type": ")" + node_type_to_string(_type) + "\",\n";
    result += prefix + R"(  "location": ")" + _location.to_string() + "\"";

    if (is_terminal()) {
        const auto* terminal = dynamic_cast<const TerminalASTNode*>(this);
        result += ",\n" + prefix + "  \"value\": ";
        std::visit(
            [&result](const auto& value) {
                using T = std::decay_t<decltype(value)>;
                if constexpr (std::is_same_v<T, std::string>) {
                    // Escape JSON string
                    result += "\"";
                    for (char c : value) {
                        switch (c) {
                        case '"' : result += "\\\""; break;
                        case '\\': result += "\\\\"; break;
                        case '\n': result += "\\n"; break;
                        case '\r': result += "\\r"; break;
                        case '\t': result += "\\t"; break;
                        default  : result += c; break;
                        }
                    }
                    result += "\"";
                } else if constexpr (std::is_same_v<T, char>) {
                    result += "\"";
                    switch (value) {
                    case '"' : result += "\\\""; break;
                    case '\\': result += "\\\\"; break;
                    case '\n': result += "\\n"; break;
                    case '\r': result += "\\r"; break;
                    case '\t': result += "\\t"; break;
                    default  : result += value; break;
                    }
                    result += "\"";
                } else if constexpr (std::is_same_v<T, bool>) {
                    result += value ? "true" : "false";
                } else if constexpr (std::is_same_v<T, Keyword>) {
                    result += "\""
                        + std::string(
                            lex_defs::KEYWORDS[static_cast<std::size_t>(value)])
                        + "\"";
                } else {
                    result += std::to_string(value);
                }
            },
            terminal->token().value());
    }

    if (!_children.empty()) {
        result += ",\n" + prefix + "  \"children\": [\n";
        for (std::size_t i = 0; i < _children.size(); ++i) {
            result += _children[i]->dump_json(indent + 4);
            if (i < _children.size() - 1) {
                result += ",";
            }
            result += "\n";
        }
        result += prefix + "  ]";
    }

    result += "\n" + prefix + "}";
    return result;
}
} // namespace lpc::frontend
