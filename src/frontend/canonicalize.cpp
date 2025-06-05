module lpc.frontend.canonicalize;

namespace lpc::frontend {

[[nodiscard]] NodeLocRef CanonicalizePass::run(
    NodeLocRef root, NodeArena& arena) noexcept {
    _symbol_mapping.push_scope();
    std::vector<NodeLocRef> new_children;
    const auto& children = arena[root].value().get_unchecked<NodeList>();
    new_children.reserve(children.size());
    
    _symbol_mapping.pop_scope();
    return arena.emplace(
        root.loc_ref(), NodeType::Program, std::move(new_children));
}

} // namespace lpc::frontend
