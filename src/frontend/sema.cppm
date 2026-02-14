export module lpc.frontend.sema;

import std;

import lpc.frontend.ast;
import lpc.frontend.arenas;
import lpc.frontend.refs;
import lpc.passes;

namespace lpc::frontend {

export class SemaPass final : public Pass {
private:

public:
    [[nodiscard]] std::string name() const noexcept final {
        return "sema";
    }

    [[nodiscard]] SExprLocRef run(
        SExprLocRef root, SExprArena& arena) noexcept final;

    explicit SemaPass() noexcept = default;
    ~SemaPass() final = default;
};

} // namespace lpc::frontend