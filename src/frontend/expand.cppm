export module lpc.frontend.expand;

import std;
import lpc.frontend.ast;
import lpc.frontend.passes;

namespace lpc::frontend {

export class ExpandPass final : public Pass {
public:
    [[nodiscard]] std::string name() const noexcept final {
        return "expand";
    }

    [[nodiscard]] bool run(NodeLocRef root, NodeArena& arena) noexcept final;

    ExpandPass() = default;
    ~ExpandPass() final = default;
};

} // namespace lpc::frontend
