module lpc.frontend.expand;

import lpc.utils.logging;

namespace lpc::frontend {

[[nodiscard]] bool ExpandPass::run(NodeLocRef root, NodeArena& arena) noexcept {
    std::vector<NodeLocRef> global_macros;
    std::vector<NodeLocRef> macros;
    std::vector<std::pair<NodeLocRef, std::size_t>> walk_stack;
    std::vector<NodeLocRef> transform_stack;
    walk_stack.reserve(32);

    walk_stack.emplace_back(root, 0);
    while (!walk_stack.empty()) {
        auto [current, macro_count] = walk_stack.back();
        walk_stack.pop_back();
        macros.resize(macro_count);
        switch (arena[current].type()) {
        case NodeType::Program:
            for (const auto& child :
                arena[current].value().get_unchecked<NodeList>()) {
                NodeType child_type = arena[child].type();
                if (child_type == NodeType::List
                    || child_type == NodeType::Quotation) {
                    walk_stack.emplace_back(child, macro_count);
                }
            }
            continue;
        case NodeType::List: {

            continue;
        }
        case NodeType::Vector:
        case NodeType::Keyword:
        case NodeType::Variable:
        case NodeType::Number:
        case NodeType::Character:
        case NodeType::Boolean:
        case NodeType::String:
        default                 : continue;
        }
    }

    return true;
}

} // namespace lpc::frontend
