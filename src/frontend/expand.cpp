module lpc.frontend.expand;

import lpc.utils.logging;

namespace lpc::frontend {

template <NodeType T>
[[nodiscard]] NodeLocRef walk(NodeLocRef node, NodeArena& arena) noexcept;

template <>
NodeLocRef walk<NodeType::Program>(NodeLocRef node, NodeArena& arena) noexcept {
    if (!node.is_valid() || arena[node].type() != NodeType::Program)
        return NodeLocRef::invalid();
    std::vector<NodeLocRef> new_children;
    const auto& children = arena[node].value().get_unchecked<NodeList>();

    if (std::ranges::equal(children, new_children))
        return node;
    return arena.emplace(
        node.loc_ref(), NodeType::Program, std::move(new_children));
}

template <NodeType T>
NodeLocRef walk(NodeLocRef node, NodeArena& /* arena */) noexcept {
    return node;
}

NodeLocRef ExpandPass::run(NodeLocRef root, NodeArena& arena) noexcept {
    return walk<NodeType::Program>(root, arena);
    // std::vector<NodeLocRef> global_macros;
    // std::vector<NodeLocRef> macros;
    // std::vector<std::pair<NodeLocRef, std::size_t>> walk_stack;
    // std::vector<NodeLocRef> new_children;
    // std::vector<NodeLocRef> transform_stack;
    // walk_stack.reserve(32);

    // walk_stack.emplace_back(root, 0);
    // while (!walk_stack.empty()) {
    //     auto [current, macro_count] = walk_stack.back();
    //     walk_stack.pop_back();
    //     macros.resize(macro_count);
    //     switch (arena[current].type()) {
    //     case NodeType::Program:
    //         new_children.clear();
    //         for (const auto& child :
    //             arena[current].value().get_unchecked<NodeList>()) {
    //             NodeType child_type = arena[child].type();
    //             if (child_type == NodeType::List
    //                 || child_type == NodeType::Quotation) {
    //                 walk_stack.emplace_back(child, macro_count);
    //             }
    //         }
    //         arena.emplace(
    //             current.loc_ref(), NodeType::Program,
    //             std::move(new_children));
    //         continue;
    //     case NodeType::List: {
    //         const auto& children
    //             = arena[current].value().get_unchecked<NodeList>();
    //         if (children.empty())
    //             continue;

    //         continue;
    //     }
    //     case NodeType::Vector:
    //     case NodeType::Keyword:
    //     case NodeType::Variable:
    //     case NodeType::Number:
    //     case NodeType::Character:
    //     case NodeType::Boolean:
    //     case NodeType::String:
    //     default                 : continue;
    //     }
    // }

    // return NodeLocRef::invalid();
}

} // namespace lpc::frontend
