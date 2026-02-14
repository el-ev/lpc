export module lpc.cps.lower;

import std;

import lpc.context;
import lpc.frontend.ast;
import lpc.frontend.arenas;
import lpc.frontend.refs;
import lpc.passes;

namespace lpc::cps {
using namespace lpc::frontend;

export class LowerPass final : public Pass {
public:
    [[nodiscard]] std::string name() const noexcept final {
        return "lower";
    }

    [[nodiscard]] SpanRef run(
        SpanRef root, CompilerContext& ctx) noexcept final;

    LowerPass() = default;
    ~LowerPass() final = default;
};

} // namespace lpc::cps
