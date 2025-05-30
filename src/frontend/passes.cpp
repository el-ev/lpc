module lpc.frontend.passes;

import lpc.utils.logging;

namespace lpc::frontend {

using lpc::utils::Debug;
using lpc::utils::Error;

bool PassManager::run_all(NodeRef root, ASTNodeArena& arena,
    const LocationArena& loc_arena,
    std::vector<std::string>& print_passes) noexcept {
    for (const auto& pass : _passes) {
        Debug("Running pass: ", pass->name());

        if (!pass->run(root, arena, loc_arena)) {
            Error("Pass failed: ", pass->name());
            return false;
        }

        Debug("Pass ", pass->name(), " completed successfully");

        if (std::ranges::find(print_passes, pass->name())
            != print_passes.end()) {
            std::println("{}", arena[root].dump_json(arena, loc_arena));
        }
    }

    return true;
}

} // namespace lpc::frontend
