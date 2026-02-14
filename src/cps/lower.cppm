export module lpc.cps.lower;

import std;

import lpc.context;
import lpc.syntax.ast;
import lpc.core.arenas;
import lpc.core.refs;
import lpc.passes;

namespace lpc::cps {

using namespace lpc::core;
using namespace lpc::syntax;

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
