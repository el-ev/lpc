export module lpc.cps.lower;

import std;
import lpc.passes;
import lpc.frontend.ast;

namespace lpc::cps {
using namespace lpc::frontend;

export class LowerPass final : public Pass {
public:
    [[nodiscard]] std::string name() const noexcept final {
        return "lower";
    }

    [[nodiscard]] NodeLocRef run(
        NodeLocRef root, NodeArena& arena) noexcept final;

    LowerPass() = default;
    ~LowerPass() final = default;
};

} // namespace lpc::cps
