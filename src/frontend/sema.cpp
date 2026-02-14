module lpc.frontend.sema;

import std;

import lpc.frontend.ast;

namespace lpc::frontend {

class SymbolTable {

};

[[nodiscard]] SExprLocRef SemaPass::run(SExprLocRef root, SExprArena& arena) noexcept {
    return root;
}

} // namespace lpc::frontend