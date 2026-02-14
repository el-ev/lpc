module lpc.frontend.sema;

import std;

import lpc.frontend.ast;

namespace lpc::frontend {

class SymbolTable { };

[[nodiscard]] SpanRef SemaPass::run(SpanRef root, SpanArena& arena) noexcept {
    return root;
}

} // namespace lpc::frontend