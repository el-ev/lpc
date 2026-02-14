export module lpc.analysis.sema;

import std;

import lpc.context;
import lpc.passes;
import lpc.syntax.arenas;
import lpc.syntax.ast;
import lpc.syntax.refs;

namespace lpc::analysis {

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
