export module lpc.frontend.expand;

import std;
import lpc.frontend.ast;
import lpc.passes;

namespace lpc::frontend {

export class ExpandPass final : public Pass {
private:
    std::vector<NodeLocRef> _global_macros;

    template <NodeType T>
    [[nodiscard]] NodeLocRef walk(NodeLocRef node, NodeArena& _arena) noexcept;

public:
    [[nodiscard]] std::string name() const noexcept final {
        return "expand";
    }

    [[nodiscard]] NodeLocRef run(
        NodeLocRef root, NodeArena& arena) noexcept final;

    [[nodiscard]] int try_add_syntax_def(
        NodeLocRef list_node_ref, NodeArena& arena) noexcept;
    ExpandPass() = default;
    ~ExpandPass() final = default;
};

[[nodiscard]] bool verify_transformer_spec(
    NodeLocRef spec_node, NodeArena& arena) noexcept;
[[nodiscard]] bool verify_syntax_rule(
    NodeLocRef rule_node, NodeArena& arena) noexcept;
[[nodiscard]] bool verify_pattern(NodeLocRef pattern_node, NodeArena& arena,
    std::vector<std::pair<NodeLocRef, std::size_t>>& variadic_bindings,
    std::size_t dimension) noexcept;
[[nodiscard]] bool verify_template(NodeLocRef template_node, NodeArena& arena,
    const std::vector<std::pair<NodeLocRef, std::size_t>>& variadic_bindings,
    std::size_t dimension) noexcept;

} // namespace lpc::frontend
