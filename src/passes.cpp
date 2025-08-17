module lpc.passes;

import lpc.utils.logging;

namespace lpc {

using lpc::utils::Debug;
using lpc::utils::Error;

SExprLocRef PassManager::run_all(SExprLocRef root, SExprArena& arena,
    std::vector<std::string>& print_passes) noexcept {
    SExprLocRef result = root;
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
            std::print("{}", arena.dump_root(result.expr_ref()));
        }
    }

    return result;
}

} // namespace lpc::frontend
