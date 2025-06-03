module lpc.frontend.annonate;

import lpc.utils.logging;

namespace lpc::frontend {

using lpc::utils::Error;
using lpc::utils::Warn;

using ConstRefIter = std::vector<NodeLocRef>::const_iterator;

[[nodiscard]] NodeLocRef visit_exp_or_def(
    NodeLocRef node, NodeArena& arena) noexcept;

[[nodiscard]] NodeLocRef visit_expression(
    NodeLocRef node, NodeArena& arena) noexcept;

[[nodiscard]] NodeLocRef visit_definition(
    NodeLocRef node, NodeArena& arena) noexcept;

[[nodiscard]] NodeLocRef visit_proc_call(
    NodeLocRef node, NodeArena& arena) noexcept;

[[nodiscard]] NodeLocRef visit_lambda(LocRef loc, ConstRefIter formals_begin,
    ConstRefIter formals_end, ConstRefIter body_begin, ConstRefIter body_end,
    NodeArena& arena) noexcept;

[[nodiscard]] NodeLocRef AnnonatePass::run(
    NodeLocRef root, NodeArena& arena) noexcept {
    std::vector<NodeLocRef> new_children;
    const auto& children = arena[root].value().get_unchecked<NodeList>();
    new_children.reserve(children.size());

    for (const auto& child : children) {
        auto node = visit_exp_or_def(child, arena);
        if (!node.is_valid())
            return NodeLocRef::invalid();
        new_children.push_back(node);
    }

    return arena.emplace(
        root.loc_ref(), NodeType::Program, std::move(new_children));
}

NodeLocRef visit_exp_or_def(NodeLocRef node, NodeArena& arena) noexcept {
    const ASTNode& astnode = arena[node];
    switch (astnode.type()) {
    case NodeType::List: {
        const auto& children_list = astnode.value().get_unchecked<NodeList>();
        if (!arena[children_list.back()].is<NodeType::Nil>()) {
            Error("Invalid syntax: Improper List {} at {}", arena.dump(node),
                arena.location(node).source_location());
            return NodeLocRef::invalid();
        }
        if (children_list.size() < 2) {
            Error("Invalid syntax: Empty List at top level at {}",
                arena.dump(node), arena.location(node).source_location());
            return NodeLocRef::invalid();
        }
        const ASTNode& first_child = arena[children_list[0]];
        if (first_child.is<NodeType::Keyword>()) {
            Keyword keyword = first_child.value().get_unchecked<Keyword>();
            switch (keyword) {
            case Keyword::DEFINE:
                return visit_definition(node, arena);
            case Keyword::QUOTE:
            case Keyword::LAMBDA:
            case Keyword::IF:
            case Keyword::SET:
                return visit_expression(node, arena);
            case Keyword::COND:
            case Keyword::ELSE:
            case Keyword::ARROW:
            case Keyword::CASE:
            case Keyword::AND:
            case Keyword::OR:
            case Keyword::LET:
            case Keyword::LET_STAR:
            case Keyword::LET_REC:
            case Keyword::BEGIN:
            case Keyword::DO:
            case Keyword::DELAY:
            case Keyword::QUASIQUOTE:
            case Keyword::UNQUOTE:
            case Keyword::UNQUOTE_SPLICING:
                Error("Should be handled in expand pass: {} at {}",
                    arena.dump(node), arena.location(node).source_location());
                return NodeLocRef::invalid();
            default:
                __builtin_unreachable();
            }
        } else if (first_child.is<NodeType::List, NodeType::Variable,
                       NodeType::Number, NodeType::Character, NodeType::Boolean,
                       NodeType::String>()) {
            return visit_proc_call(node, arena);
        } else {
            Error("Invalid syntax: Unexpected {} {} at {}",
                node_type_to_string(first_child.type()),
                arena.dump(children_list[0]),
                arena.location(children_list[0]).source_location());
            return NodeLocRef::invalid();
        }
    }
    case NodeType::Variable:
    case NodeType::Number:
    case NodeType::Character:
    case NodeType::Boolean:
    case NodeType::String:
        // return visit_expression(node, arena);
        return node;
    case NodeType::Vector:
    case NodeType::Keyword:
        Error("Invalid syntax: Unexpected {} {} at top level at {}",
            node_type_to_string(astnode.type()), arena.dump(node),
            arena.location(node).source_location());
        return NodeLocRef::invalid();
    default:
        __builtin_unreachable();
    }
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
            Error("Invalid syntax: Expected variable name in definition at {}",
                arena.location(children_list[1]).source_location());
            return NodeLocRef::invalid();
        }
        if (!arena[list[0]].is<NodeType::Variable>()) {
            Error(
                "Invalid syntax: Expected variable name in definition, got {} "
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
    Error("Invalid syntax: Expected variable or list in definition, got {} {} "
          "at {}",
        node_type_to_string(second_child.type()), arena.dump(children_list[1]),
        arena.location(children_list[1]).source_location());
    return NodeLocRef::invalid();
}

NodeLocRef visit_expression(NodeLocRef node, NodeArena& arena) noexcept {
    const ASTNode& astnode = arena[node];
    switch (astnode.type()) {
    case NodeType::List: {
        const auto& children_list = astnode.value().get_unchecked<NodeList>();
        if (children_list.size() < 2) {
            Error("Invalid syntax: Expected at least 2 children, got {} at {}",
                children_list.size() - 1,
                arena.location(node).source_location());
            return NodeLocRef::invalid();
        }
    }
    case NodeType::Variable:
    case NodeType::Number:
    case NodeType::Character:
    case NodeType::Boolean:
    case NodeType::String:
        return node;
    default:
        Error("Invalid syntax: Expected expression, got {} {} at {}",
            node_type_to_string(astnode.type()), arena.dump(node),
            arena.location(node).source_location());
        return NodeLocRef::invalid();
    }
    return node;
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
    new_children.reserve(children_list.size());
    for (const auto& child : children_list) {
        NodeLocRef exp_node = visit_expression(child, arena);
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
    auto it = body_begin;
    // verify definitions
    while (it != body_end - 1) {
        const ASTNode& astnode = arena[*it];
        if (astnode.is<NodeType::List>()) {
            auto list = astnode.value().get_unchecked<NodeList>();
            if (list.size() >= 2) {
                const ASTNode& first_child = arena[list[0]];
                if (first_child.is<NodeType::Keyword>()
                    && first_child.value().get_unchecked<Keyword>()
                        == Keyword::DEFINE) {
                    NodeLocRef def_node = visit_definition(*it, arena);
                    if (!def_node.is_valid())
                        return NodeLocRef::invalid();
                    lambda_children.push_back(def_node);
                    ++it;
                    continue;
                }
            }
        }
        break;
    }
    // should be a series of expressions
    if (it == body_end - 1) {
        Error("Invalid syntax: There should be at least one expression "
              "in the body of lambda at {}",
            arena.location(*it).source_location());
        return NodeLocRef::invalid();
    }
    while (it != body_end - 1) {
        NodeLocRef exp_node = visit_expression(*it, arena);
        if (!exp_node.is_valid())
            return NodeLocRef::invalid();
        lambda_children.push_back(exp_node);
        ++it;
    }

    return arena.emplace(loc, NodeType::Lambda, std::move(lambda_children));
}
} // namespace lpc::frontend
