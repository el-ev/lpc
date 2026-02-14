module lpc.cps.lower;

namespace lpc::cps {

[[nodiscard]] SpanRef LowerPass::run(
    SpanRef root, SpanArena& /* arena */) noexcept {
    // TODO: implement CPS lowering
    return root;
}

} // namespace lpc::cps
