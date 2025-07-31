module lpc.frontend.expand;

import lpc.utils.logging;

namespace lpc::frontend {

using lpc::utils::Error;

// TODO

template <>
NodeLocRef ExpandPass::walk<NodeType::Program>(
    NodeLocRef node, NodeArena& arena) noexcept {
    if (!node.is_valid() || arena[node].type() != NodeType::Program)
        return NodeLocRef::invalid();
    std::vector<NodeLocRef> new_children;
    const auto& children = arena[node].value().get_unchecked<NodeList>();
    new_children.reserve(children.size());
    for (const auto& child : children) {
        const ASTNode& child_node = arena[child];
        NodeLocRef new_child;
        switch (child_node.type()) {
        case NodeType::List: {
            const auto& list_children
                = child_node.value().get_unchecked<NodeList>();
            if (list_children.empty())
                new_child = child;
            else {
                int result = try_add_syntax_def(child, arena);
                if (result == -1)
                    return NodeLocRef::invalid();
                if (result == 1)
                    continue;
                return walk<NodeType::List>(child, arena);
            }
            break;
        }
        case NodeType::Identifier:
        case NodeType::Number:
        case NodeType::Character:
        case NodeType::Boolean:
        case NodeType::String:
            new_child = child;
            break;
        case NodeType::Vector:
            Error("Invalid syntax: Unexpected {}({}) in program body at {}",
                node_type_to_string(child_node.type()), arena.dump(child),
                arena.location(child).source_location());
            return NodeLocRef::invalid();
        default:
            __builtin_unreachable();
        }
        new_children.push_back(new_child);
    }
    if (std::ranges::equal(children, new_children))
        return node;
    return arena.emplace(
        node.loc_ref(), NodeType::Program, std::move(new_children));
}

template <NodeType T>
NodeLocRef ExpandPass::walk(
    NodeLocRef /* node */, NodeArena& /* arena */) noexcept {
    // terminal nodes should be handled by their parents
    __builtin_unreachable();
}

NodeLocRef ExpandPass::run(NodeLocRef root, NodeArena& arena) noexcept {
    return walk<NodeType::Program>(root, arena);
}

int ExpandPass::try_add_syntax_def(
    NodeLocRef list_node_ref, NodeArena& arena) noexcept {
    // (define-syntax id transformer-spec)
    const ASTNode& node = arena[list_node_ref];
    // it is verified that it is a list and not empty
    const auto& children = node.value().get_unchecked<NodeList>();
    const ASTNode& first_child = arena[children[0]];
    if (!first_child.is<NodeType::Identifier>())
        return 0;
    const auto& ident = first_child.value().get_unchecked<std::string>();
    if (ident != "define-syntax")
        return 0;
    // we cannot return 0 after this
    if (children.size() != 4) {
        Error("Invalid syntax: define-syntax at {} must have 3 children, "
              "found {}",
            arena.location(list_node_ref).source_location(),
            children.size() - 1);
        return -1;
    }
    const ASTNode& second_child = arena[children[1]];
    if (!second_child.is<NodeType::Identifier>()) {
        Error("Invalid syntax: expected an identifier at {}, found {}({})",
            arena.location(children[1]).source_location(),
            node_type_to_string(second_child.type()), arena.dump(children[1]));
        return -1;
    }
    if (!verify_transformer_spec(children[2], arena))
        return -1;
    _global_macros.push_back(list_node_ref);
    return 1;
};

bool verify_transformer_spec(NodeLocRef spec_node, NodeArena& arena) noexcept {
    const ASTNode& node = arena[spec_node];
    if (!node.is<NodeType::List>()) {
        Error("Invalid syntax: expected a Transformer Spec at {}, found a {}",
            arena.location(spec_node).source_location(),
            node_type_to_string(node.type()));
        return false;
    }
    const auto& children = node.value().get_unchecked<NodeList>();
    if (children.size() < 2) {
        Error("Invalid syntax: Transformer Spec must have at least 2 children "
              "at {}, "
              "found {}",
            arena.location(spec_node).source_location(), children.size());
        return false;
    }
    const ASTNode& first_child = arena[children[0]];
    if (!first_child.is<NodeType::Identifier>()
        || first_child.value().get_unchecked<std::string>() != "syntax-rules") {
        Error("Invalid syntax: expected \"syntax-rules\" at {}, found {}",
            arena.location(children[0]).source_location(),
            arena.location(children[0]).lexeme());
        return false;
    }
    const ASTNode& second_child = arena[children[1]];
    if (!second_child.is<NodeType::List>())
        return false;
    // TODO
    // std::set<ASTNodeRef> bindings;
    // bindings.insert(name);
    for (auto ident : second_child.value().get_unchecked<NodeList>()) {
        const ASTNode& ident_node = arena[ident];
        if (!ident_node.is<NodeType::Identifier, NodeType::Nil>()) {
            Error("Invalid syntax: expected an identifier at {}, found {}({})",
                arena.location(ident).source_location(),
                node_type_to_string(ident_node.type()), arena.dump(ident));
            return false;
        }
        // bindings.insert(ident.node_ref());
    }
    for (std::size_t i = 2; i < children.size(); ++i)
        if (!verify_syntax_rule(children[i], arena))
            return false;
    return true;
}

bool verify_syntax_rule(NodeLocRef rule_node, NodeArena& arena) noexcept {
    const ASTNode& node = arena[rule_node];
    if (!node.is<NodeType::List>()) {
        Error("Invalid syntax: expected a Syntax Rule at {}, found a {}",
            arena.location(rule_node).source_location(),
            node_type_to_string(node.type()));
        return false;
    }
    const auto& children = node.value().get_unchecked<NodeList>();
    if (children.size() != 3) {
        Error("Invalid syntax: Syntax Rule at {} must have 2 children, "
              "found {}",
            arena.location(rule_node).source_location(), children.size() - 1);
        return false;
    }

    std::vector<std::pair<NodeLocRef, std::size_t>> variadic_bindings;
    // Verify pattern
    if (!verify_pattern(children[0], arena, variadic_bindings, 0))
        return false;

    // Verify template
    if (!verify_template(children[1], arena, variadic_bindings, 0))
        return false;

    return true;
}

bool verify_pattern(NodeLocRef pattern_node, NodeArena& arena,
    std::vector<std::pair<NodeLocRef, std::size_t>>& variadic_bindings,
    std::size_t dimension) noexcept {
    const ASTNode& node = arena[pattern_node];
    switch (node.type()) { }
    return false;
}

bool verify_template(NodeLocRef template_node, NodeArena& arena,
    const std::vector<std::pair<NodeLocRef, std::size_t>>& variadic_bindings,
    std::size_t dimension) noexcept {
    return false;
}

} // namespace lpc::frontend
