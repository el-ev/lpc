module lpc.cps.lower;

namespace lpc::cps {

[[nodiscard]] SExprLocRef LowerPass::run(
    SExprLocRef root, SExprArena& arena) noexcept {
    return root;
}

} // namespace lpc::cps
