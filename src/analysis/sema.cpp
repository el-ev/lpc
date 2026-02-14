module lpc.analysis.sema;

import std;

import lpc.context;
import lpc.syntax.ast;

namespace lpc::analysis {

using namespace lpc::syntax;

[[nodiscard]] SpanRef SemaPass::run(
    SpanRef root, CompilerContext& /* ctx */) noexcept {
    return root;
}

} // namespace lpc::analysis
