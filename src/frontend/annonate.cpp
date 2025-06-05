module lpc.frontend.annonate;

import lpc.utils.logging;

namespace lpc::frontend {

using lpc::utils::Error;
using lpc::utils::Warn;

using ConstRefIter = std::vector<NodeLocRef>::const_iterator;

[[nodiscard]] NodeLocRef visit_expression(
    NodeLocRef node, NodeArena& arena) noexcept;

[[nodiscard]] NodeLocRef visit_definition(
    NodeLocRef node, NodeArena& arena) noexcept;

[[nodiscard]] NodeLocRef visit_proc_call(
    NodeLocRef node, NodeArena& arena) noexcept;

[[nodiscard]] NodeLocRef visit_lambda(LocRef loc, ConstRefIter formals_begin,
    ConstRefIter formals_end, ConstRefIter body_begin, ConstRefIter body_end,
    NodeArena& arena) noexcept;

[[nodiscard]] NodeLocRef visit_quote(
    NodeLocRef node, NodeArena& arena) noexcept;

[[nodiscard]] NodeLocRef AnnonatePass::run(
    NodeLocRef root, NodeArena& arena) noexcept {
    std::vector<NodeLocRef> new_children;
    const auto& children = arena[root].value().get_unchecked<NodeList>();
    new_children.reserve(children.size());

    for (const auto& child : children) {
        auto node = visit_quote(child, arena);
        if (!node.is_valid())
            return NodeLocRef::invalid();
        node = visit_expression(node, arena);
        if (!node.is_valid())
            return NodeLocRef::invalid();
        new_children.push_back(node);
    }

    return arena.emplace(
        root.loc_ref(), NodeType::Program, std::move(new_children));
}

NodeLocRef visit_expression(NodeLocRef node, NodeArena& arena) noexcept {
    const ASTNode& astnode = arena[node];
    switch (astnode.type()) {
    case NodeType::List: {
        const auto& children_list = astnode.value().get_unchecked<NodeList>();
        if (children_list.size() < 2) {
            Error("Invalid syntax: Unexpected empty list at {}",
                arena.location(node).source_location());
            return NodeLocRef::invalid();
        }
        if (!arena[children_list.back()].is<NodeType::Nil>()) {
            Error("Invalid syntax: Improper list at {}",
                arena.location(node).source_location());
            return NodeLocRef::invalid();
        }
        const ASTNode& first_child = arena[children_list[0]];
        if (first_child.is<NodeType::Keyword>()) {
            Keyword keyword = first_child.value().get_unchecked<Keyword>();
            switch (keyword) {
            case Keyword::DEFINE:
                return visit_definition(node, arena);
            case Keyword::LAMBDA: {
                if (children_list.size() < 4) {
                    Error(
                        "Invalid syntax: Expected at least 3 children, got {} "
                        "at {}",
                        children_list.size() - 1,
                        arena.location(node).source_location());
                    return NodeLocRef::invalid();
                }
                const ASTNode& second_child = arena[children_list[1]];
                if (!second_child.is<NodeType::List>()) {
                    Error("Invalid syntax: Expected list of formals, got {} "
                          "{} at {}",
                        node_type_to_string(second_child.type()),
                        arena.dump(children_list[1]),
                        arena.location(children_list[1]).source_location());
                    return NodeLocRef::invalid();
                }
                const auto& formals_list
                    = second_child.value().get_unchecked<NodeList>();
                return visit_lambda(node.loc_ref(), formals_list.begin(),
                    formals_list.end(), children_list.begin() + 2,
                    children_list.end(), arena);
            }
            case Keyword::IF: {
                if (children_list.size() != 4 && children_list.size() != 5) {
                    Error("Invalid syntax: Expected 3 or 4 children, got {} "
                          "at {}",
                        children_list.size() - 1,
                        arena.location(node).source_location());
                    return NodeLocRef::invalid();
                }
                NodeLocRef test_node
                    = visit_expression(children_list[1], arena);
                NodeLocRef then_node
                    = visit_expression(children_list[2], arena);
                if (!test_node.is_valid() || !then_node.is_valid())
                    return NodeLocRef::invalid();
                if (children_list.size() == 4)
                    return arena.emplace(node.loc_ref(), NodeType::If,
                        std::vector<NodeLocRef> { test_node, then_node });
                NodeLocRef else_node
                    = visit_expression(children_list[3], arena);
                if (!else_node.is_valid())
                    return NodeLocRef::invalid();
                return arena.emplace(node.loc_ref(), NodeType::If,
                    std::vector<NodeLocRef> {
                        test_node, then_node, else_node });
            }
            case Keyword::SET: {
                if (children_list.size() != 4) {
                    Error("Invalid syntax: Expected 3 children, got {} at {}",
                        children_list.size() - 1,
                        arena.location(node).source_location());
                    return NodeLocRef::invalid();
                }
                const ASTNode& second_child = arena[children_list[1]];
                if (!second_child.is<NodeType::Variable>()) {
                    Error("Invalid syntax: Expected variable, got {} {} at {}",
                        node_type_to_string(second_child.type()),
                        arena.dump(children_list[1]),
                        arena.location(children_list[1]).source_location());
                    return NodeLocRef::invalid();
                }
                NodeLocRef exp_node = visit_expression(children_list[2], arena);
                if (!exp_node.is_valid())
                    return NodeLocRef::invalid();
                return arena.emplace(node.loc_ref(), NodeType::Assignment,
                    std::vector<NodeLocRef> { children_list[1], exp_node });
            }
            default:
                Error("Should be handled earlier: {} at {}", arena.dump(node),
                    arena.location(node).source_location());
                return NodeLocRef::invalid();
            }
        }
        return visit_proc_call(node, arena);
    }
    case NodeType::Variable:
    case NodeType::Number:
    case NodeType::Character:
    case NodeType::Boolean:
    case NodeType::String:
    case NodeType::Quotation:
        return node;
    default:
        Error("Invalid syntax: Expected expression, got {} {} at {}",
            node_type_to_string(astnode.type()), arena.dump(node),
            arena.location(node).source_location());
        return NodeLocRef::invalid();
    }
    return node;
}

// (define <var> <body>)
// (define (<var> <formals>) <body>)
// -> (define <var> (lambda (<formals>) <body>))
NodeLocRef visit_definition(NodeLocRef node, NodeArena& arena) noexcept {
    const ASTNode& astnode = arena[node];
    // Validated by caller
    const auto& children_list = astnode.value().get_unchecked<NodeList>();
    if (children_list.size() < 4) {
        Error("Invalid syntax: Expected at least 3 children, got {} at {}",
            children_list.size() - 1, arena.location(node).source_location());
        return NodeLocRef::invalid();
    }
    // first child is 'define'
    const ASTNode& second_child = arena[children_list[1]];
    if (second_child.is<NodeType::Variable>()) {
        if (children_list.size() != 4) {
            Error("Invalid syntax: Expected 3 children, got {} at {}",
                children_list.size() - 1,
                arena.location(children_list[1]).source_location());
            return NodeLocRef::invalid();
        }
        NodeLocRef exp_node = visit_expression(children_list[2], arena);
        if (!exp_node.is_valid())
            return NodeLocRef::invalid();
        return arena.emplace(node.loc_ref(), NodeType::Definition,
            std::vector<NodeLocRef> { children_list[1], exp_node });
    }
    if (second_child.is<NodeType::List>()) {
        const auto& list = second_child.value().get_unchecked<NodeList>();
        if (list.size() < 2) {
            Error("Invalid syntax: Expected variable name in definition at "
                  "{}",
                arena.location(children_list[1]).source_location());
            return NodeLocRef::invalid();
        }
        if (!arena[list[0]].is<NodeType::Variable>()) {
            Error("Invalid syntax: Expected variable name in definition, "
                  "got {} "
                  "at {}",
                arena.dump(list[0]),
                arena.location(children_list[1]).source_location());
            return NodeLocRef::invalid();
        }
        NodeLocRef name_node = list[0];
        NodeLocRef lambda_node = visit_lambda(children_list[1].loc_ref(),
            list.begin() + 1, list.end(), children_list.begin() + 2,
            children_list.end(), arena);
        if (!lambda_node.is_valid())
            return NodeLocRef::invalid();
        return arena.emplace(node.loc_ref(), NodeType::Definition,
            std::vector<NodeLocRef> { name_node, lambda_node });
    }
    Error("Invalid syntax: Expected variable or list in definition, got {} "
          "{} "
          "at {}",
        node_type_to_string(second_child.type()), arena.dump(children_list[1]),
        arena.location(children_list[1]).source_location());
    return NodeLocRef::invalid();
}

NodeLocRef visit_proc_call(NodeLocRef node, NodeArena& arena) noexcept {
    const ASTNode& astnode = arena[node];
    const auto& children_list = astnode.value().get_unchecked<NodeList>();
    const ASTNode& first_child = arena[children_list[0]];
    if (!first_child.is<NodeType::List, NodeType::Variable>())
        Warn("{} {} doesn't seem to be callable at {}",
            node_type_to_string(first_child.type()),
            arena.dump(children_list[0]),
            arena.location(node).source_location());
    // TODO Remove this
    if (first_child.is<NodeType::Variable>()
        && (first_child.value().get_unchecked<std::string>() == "define-syntax"
            || first_child.value().get_unchecked<std::string>() == "let-syntax"
            || first_child.value().get_unchecked<std::string>()
                == "letrec-syntax")) {
        Error("Should be handled in expand pass: {} at {}", arena.dump(node),
            arena.location(node).source_location());
        return NodeLocRef::invalid();
    }
    std::vector<NodeLocRef> new_children;
    new_children.reserve(children_list.size() - 1);
    for (auto it = children_list.begin(); it != children_list.end() - 1; ++it) {
        NodeLocRef exp_node = visit_expression(*it, arena);
        if (!exp_node.is_valid())
            return NodeLocRef::invalid();
        new_children.push_back(exp_node);
    }
    return arena.emplace(
        node.loc_ref(), NodeType::ProcedureCall, std::move(new_children));
}

NodeLocRef visit_lambda(LocRef loc, ConstRefIter formals_begin,
    ConstRefIter formals_end, ConstRefIter body_begin, ConstRefIter body_end,
    NodeArena& arena) noexcept {
    if (!std::ranges::all_of(formals_begin, formals_end - 1,
            [&arena](const NodeLocRef& formal) {
                return arena[formal].template is<NodeType::Variable>();
            })
        || !arena[*(formals_end - 1)].is<NodeType::Nil, NodeType::Variable>()) {
        Error("Invalid syntax: Some formals are not variables at {}",
            arena.location(*formals_begin).source_location());
        return NodeLocRef::invalid();
    }
    std::vector<NodeLocRef> formal_children;
    std::ranges::copy(
        formals_begin, formals_end, std::back_inserter(formal_children));
    NodeLocRef formal_node
        = arena.emplace(loc, NodeType::List, std::move(formal_children));
    std::vector<NodeLocRef> lambda_children = { formal_node };
    for (const auto& body_node :
        std::ranges::subrange(body_begin, body_end - 1)) {
        NodeLocRef exp_node = visit_expression(body_node, arena);
        if (!exp_node.is_valid())
            return NodeLocRef::invalid();
        lambda_children.push_back(exp_node);
    }

    return arena.emplace(loc, NodeType::Lambda, std::move(lambda_children));
}

[[nodiscard]] NodeLocRef visit_quote(
    NodeLocRef node, NodeArena& arena) noexcept {
    const ASTNode& astnode = arena[node];
    if (!astnode.is<NodeType::List>())
        return node;
    const auto& children_list = astnode.value().get_unchecked<NodeList>();
    std::vector<NodeLocRef> quoted_children;
    for (const auto& child : children_list) {
        auto quoted_node = visit_quote(child, arena);
        if (!quoted_node.is_valid())
            return NodeLocRef::invalid();
        quoted_children.push_back(quoted_node);
    }
    if (quoted_children.size() < 2
        || !arena[quoted_children[0]].is<NodeType::Keyword>()
        || arena[quoted_children[0]].value().get_unchecked<Keyword>()
            != Keyword::QUOTE)
        return arena.emplace(
            node.loc_ref(), NodeType::List, std::move(quoted_children));
    if (quoted_children.size() == 2) {
        Error("Invalid syntax: Expected quoted expression, got {} at {}",
            arena.dump(node), arena.location(node).source_location());
        return NodeLocRef::invalid();
    }
    if (quoted_children.size() > 3) {
        Error("Invalid syntax: Expected quoted expression, got {} at {}",
            quoted_children.size() - 1, arena.location(node).source_location());
        return NodeLocRef::invalid();
    }
    if (!arena[quoted_children.back()].is<NodeType::Nil>()) {
        Error("Invalid syntax: Improper list at {}",
            arena.location(node).source_location());
        return NodeLocRef::invalid();
    }
    return arena.emplace(node.loc_ref(), NodeType::Quotation,
        std::vector<NodeLocRef> { quoted_children[1] });
}

} // namespace lpc::frontend
