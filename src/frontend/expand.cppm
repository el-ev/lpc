export module lpc.frontend.expand;

import std;
import lpc.frontend.ast;
import lpc.frontend.passes;

namespace lpc::frontend {

export class ExpandPass : public Pass {
public:
    [[nodiscard]] std::string name() const noexcept override {
        return "expand";
    }

    [[nodiscard]] bool run(NodeRef root, ASTNodeArena& arena,
        const LocationArena& loc_arena) noexcept override {

        return true;
    }

    ExpandPass() = default;
    ~ExpandPass() override = default;
};

} // namespace lpc::frontend
