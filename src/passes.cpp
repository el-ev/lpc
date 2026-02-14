module lpc.passes;

import std;

import lpc.frontend.refs;
import lpc.utils.logging;

namespace lpc {

using lpc::utils::Debug;
using lpc::utils::Error;

SpanRef PassManager::run_all(SpanRef root, SpanArena& arena,
    std::vector<std::string>& print_passes) noexcept {
    SpanRef result = root;
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
            std::print("{}", arena.dump_root(result));
        }
    }

    return result;
}

} // namespace lpc
