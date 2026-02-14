module lpc.frontend.sema;

import std;

import lpc.context;
import lpc.frontend.ast;

namespace lpc::frontend {

class SymbolTable { };

[[nodiscard]] SpanRef SemaPass::run(
    SpanRef root, CompilerContext& /* ctx */) noexcept {
    return root;
}

} // namespace lpc::frontend