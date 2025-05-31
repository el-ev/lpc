module lpc.frontend.expand;

import lpc.utils.logging;

namespace lpc::frontend {

[[nodiscard]] bool ExpandPass::run(NodeLocRef root, NodeArena& arena) noexcept {
    std::vector<NodeLocRef> macros;
    std::vector<std::pair<NodeLocRef, std::size_t>> stack;
    stack.reserve(32);
    
    // TODO populate builtin macros
    NodeLocRef current = root;
    stack.emplace_back(current, 0 /* builtin */);
    while (true) {
        auto [current, _] = stack.back();
        switch (arena[current].type()) {
            default: return true;
        }
    }

    return true;
}



} // namespace lpc::frontend
