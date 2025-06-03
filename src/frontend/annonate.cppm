export module lpc.frontend.annonate;

import std;
import lpc.frontend.ast;
import lpc.frontend.passes;

namespace lpc::frontend {

export class AnnonatePass final : public Pass {
public:
    [[nodiscard]] std::string name() const noexcept final {
        return "annonate";
    }

    [[nodiscard]] NodeLocRef run(
        NodeLocRef root, NodeArena& arena) noexcept final;

    AnnonatePass() = default;
    ~AnnonatePass() final = default;
};

} // namespace lpc::frontend
