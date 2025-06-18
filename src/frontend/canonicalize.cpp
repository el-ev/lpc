module lpc.frontend.canonicalize;

import lpc.utils.logging;

namespace lpc::frontend {

using lpc::utils::Error;

[[nodiscard]] NodeLocRef quote(NodeLocRef node, NodeArena& arena) noexcept {
    return arena.emplace(
        node.loc_ref(), NodeType::Quotation, NodeList { node });
}

[[nodiscard]] NodeLocRef CanonicalizePass::visit(
    NodeLocRef node, NodeArena& arena, bool top_level) noexcept {
    const ASTNode& astnode = arena[node];
    switch (astnode.type()) {
    case NodeType::Definition: {
        const auto& children = astnode.value().get_unchecked<NodeList>();
        NodeLocRef lhs;
        if (top_level) {
            _symbol_mapping.add_mapping(
                children[0].node_ref(), children[0].node_ref());
            lhs = children[0];
        } else {
            lhs = arena.emplace(children[0].loc_ref(), NodeType::Variable,
                arena[children[0]].value().get_unchecked<std::string>()
                    + std::to_string(_counter++));
            _symbol_mapping.add_mapping(children[0].node_ref(), lhs.node_ref());
        }
        _symbol_mapping.push_scope();
        NodeLocRef rhs = visit(children[1], arena, false);
        _symbol_mapping.pop_scope();
        return arena.emplace(
            node.loc_ref(), NodeType::Definition, NodeList { lhs, rhs });
    }
    case NodeType::Variable: {
        auto mapping = _symbol_mapping.get_mapping(node.node_ref());
        if (!mapping.has_value()) {
            Error("Variable '{}' is not bound at {}",
                astnode.value().get_unchecked<std::string>(),
                arena.location(node).source_location());
            return NodeLocRef::invalid();
        }
        return NodeLocRef(mapping.value(), node.loc_ref());
    }
    case NodeType::Assignment: {
        const auto& children = astnode.value().get_unchecked<NodeList>();
        // should not have any difference when top_level is true or false
        // because set! do not create new bindings
        NodeLocRef lhs = visit(children[0], arena, top_level);
        _symbol_mapping.push_scope();
        NodeLocRef rhs = visit(children[1], arena, false);
        _symbol_mapping.pop_scope();
        if (!lhs.is_valid() || !rhs.is_valid())
            return NodeLocRef::invalid();
        return arena.emplace(
            node.loc_ref(), NodeType::Assignment, NodeList { lhs, rhs });
    }
    case NodeType::ProcedureCall: {
        const auto& children = astnode.value().get_unchecked<NodeList>();
        NodeList new_children;
        new_children.reserve(children.size());
        std::ranges::transform(
            std::ranges::subrange(children.begin() + 1, children.end()),
            std::back_inserter(new_children),
            [&](NodeLocRef child) { return visit(child, arena, false); });
        if (arena[children[0]].is<NodeType::Variable>()) {
            auto mapping = _symbol_mapping.get_mapping(children[0].node_ref());
            if (!mapping.has_value()) {
                // check if the first child is a builtin function
                const auto* builtin = _builtins.get(
                    arena[children[0]].value().get_unchecked<std::string>());
                if (builtin != nullptr) {
                    return builtin->get_call(arena, node.loc_ref(),
                        NodeList(children.begin() + 1, children.end()));
                }
                Error("Procedure call '{}' is not bound at {}",
                    arena[children[0]].value().get_unchecked<std::string>(),
                    arena.location(node).source_location());
                return NodeLocRef::invalid();
            }
            new_children.insert(new_children.begin(),
                NodeLocRef(mapping.value(), children[0].loc_ref()));
        } else {
            new_children.insert(
                new_children.begin(), visit(children[0], arena, false));
        }
        if (std::ranges::any_of(new_children,
                [](NodeLocRef child) { return !child.is_valid(); }))
            return NodeLocRef::invalid();
        return arena.emplace(
            node.loc_ref(), NodeType::ProcedureCall, std::move(new_children));
    }
    case NodeType::If: {
        const auto& children = astnode.value().get_unchecked<NodeList>();
        NodeList new_children;
        new_children.reserve(children.size());
        std::ranges::transform(children, std::back_inserter(new_children),
            [&](NodeLocRef child) { return visit(child, arena, false); });
        if (std::ranges::any_of(new_children,
                [](NodeLocRef child) { return !child.is_valid(); }))
            return NodeLocRef::invalid();
        return arena.emplace(
            node.loc_ref(), NodeType::ProcedureCall, std::move(new_children));
    }
    case NodeType::Lambda: {
        const auto& children = astnode.value().get_unchecked<NodeList>();
        const auto& params
            = arena[children[0]].value().get_unchecked<NodeList>();
        _symbol_mapping.push_scope();
        NodeList new_params;
        new_params.reserve(params.size());
        std::ranges::transform(
            params, std::back_inserter(new_params), [&](NodeLocRef param) {
                if (!arena[param].is<NodeType::Variable>())
                    return param;
                NodeLocRef new_param
                    = arena.emplace(param.loc_ref(), NodeType::Variable,
                        arena[param].value().get_unchecked<std::string>()
                            + std::to_string(_counter++));
                _symbol_mapping.add_mapping(
                    param.node_ref(), new_param.node_ref());
                return new_param;
            });
        NodeList new_children;
        new_children.push_back(arena.emplace(
            children[0].loc_ref(), NodeType::List, std::move(new_params)));
        std::ranges::transform(
            std::ranges::subrange(children.begin() + 1, children.end()),
            std::back_inserter(new_children), [&](NodeLocRef body_node) {
                return visit(body_node, arena, false);
            });
        _symbol_mapping.pop_scope();
        if (std::ranges::any_of(new_children,
                [](NodeLocRef child) { return !child.is_valid(); }))
            return NodeLocRef::invalid();
        return arena.emplace(
            node.loc_ref(), NodeType::Lambda, std::move(new_children));
    }
    case NodeType::Number:
    case NodeType::Boolean:
    case NodeType::Character:
    case NodeType::String:
        return quote(node, arena);
    case NodeType::Quotation:
    default:
        return node;
    }
}

[[nodiscard]] NodeLocRef CanonicalizePass::run(
    NodeLocRef root, NodeArena& arena) noexcept {
    _symbol_mapping.push_scope();
    NodeList new_children;
    const auto& children = arena[root].value().get_unchecked<NodeList>();
    new_children.reserve(children.size());
    std::ranges::transform(children, std::back_inserter(new_children),
        [&](NodeLocRef child) { return visit(child, arena, true); });
    if (std::ranges::any_of(
            new_children, [](NodeLocRef child) { return !child.is_valid(); }))
        return NodeLocRef::invalid();
    _symbol_mapping.pop_scope();
    return arena.emplace(
        root.loc_ref(), NodeType::Program, std::move(new_children));
}

} // namespace lpc::frontend
