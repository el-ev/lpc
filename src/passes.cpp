module lpc.passes;

import lpc.utils.logging;

namespace lpc {

using lpc::utils::Debug;
using lpc::utils::Error;

NodeLocRef PassManager::run_all(NodeLocRef root, NodeArena& arena,
    std::vector<std::string>& print_passes, bool print_json) noexcept {
    NodeLocRef result = root;
    for (const auto& pass : _passes) {
        Debug("Running pass: {}", pass->name());

        result = pass->run(result, arena);
        if (!result.is_valid()) {
            Error("Pass failed: {}", pass->name());
            return result;
        }

        Debug("Pass {} completed successfully", pass->name());

        if (std::ranges::find(print_passes, pass->name())
            != print_passes.end()) {
            if (print_json)
                std::print("{}", arena.dump_json(result, 2));
            else
                std::print("{}", arena.dump(result));
        }
    }

    return result;
}

} // namespace lpc::frontend
