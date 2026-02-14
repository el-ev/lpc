export module lpc.analysis.sema;

import std;

import lpc.context;
import lpc.core.arenas;
import lpc.core.refs;
import lpc.passes;
import lpc.syntax.ast;

namespace lpc::analysis {

using namespace lpc::core;
using namespace lpc::syntax;

export class SemaPass final : public Pass {
private:
public:
    [[nodiscard]] std::string name() const noexcept final {
        return "sema";
    }

    [[nodiscard]] SpanRef run(
        SpanRef root, CompilerContext& ctx) noexcept final;

    explicit SemaPass() noexcept = default;
    ~SemaPass() final = default;
};

} // namespace lpc::analysis
