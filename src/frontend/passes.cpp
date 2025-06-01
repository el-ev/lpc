module lpc.frontend.passes;

import lpc.utils.logging;

namespace lpc::frontend {

using lpc::utils::Debug;
using lpc::utils::Error;

NodeLocRef PassManager::run_all(NodeLocRef root, NodeArena& arena,
    std::vector<std::string>& print_passes) noexcept {
    NodeLocRef result = root;
    for (const auto& pass : _passes) {
        Debug("Running pass: ", pass->name());

        result = pass->run(result, arena);
        if (!result.is_valid()) {
            Error("Pass failed: ", pass->name());
            return result;
        }

        Debug("Pass ", pass->name(), " completed successfully");

        if (std::ranges::find(print_passes, pass->name()) != print_passes.end())
            std::println("{}", arena.dump(result));
    }

    return result;
}

} // namespace lpc::frontend
