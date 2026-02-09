module lpc.frontend.expand;

import lpc.utils.logging;

namespace lpc::frontend {

using lpc::utils::Error;

[[nodiscard]] SExprLocRef ExpandPass::run(
    SExprLocRef root, SExprArena& /* arena */) noexcept {
    // TODO: implement macro expansion
    return root;
}

} // namespace lpc::frontend
