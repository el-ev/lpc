module lpc.frontend.passes;

import lpc.utils.logging;

namespace lpc::frontend {

using lpc::utils::Debug;
using lpc::utils::Error;

bool PassManager::run_all(NodeLocRef root, NodeArena& arena,
    std::vector<std::string>& print_passes) noexcept {
    for (const auto& pass : _passes) {
        Debug("Running pass: ", pass->name());

        if (!pass->run(root, arena)) {
            Error("Pass failed: ", pass->name());
            return false;
        }

        Debug("Pass ", pass->name(), " completed successfully");

        if (std::ranges::find(print_passes, pass->name())
            != print_passes.end()) {
            std::println("{}", arena.dump_json(root));
        }
    }

    return true;
}

} // namespace lpc::frontend
