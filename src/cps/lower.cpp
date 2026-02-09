module lpc.cps.lower;

namespace lpc::cps {

[[nodiscard]] SExprLocRef LowerPass::run(
    SExprLocRef root, SExprArena& /* arena */) noexcept {
    // TODO: implement CPS lowering
    return root;
}

} // namespace lpc::cps
