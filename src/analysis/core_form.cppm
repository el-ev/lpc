export module lpc.analysis.core_form;

import std;

import lpc.syntax.refs;
import lpc.utils.arena;
import lpc.utils.tagged_union;

namespace lpc::analysis {

using namespace lpc::syntax;
using lpc::utils::TaggedUnion;

export struct VarId {
    std::uint32_t id;
    std::string debug_name;
};

export struct CoreExprTag { };
export using CoreExprRef
    = lpc::utils::ElementReference<CoreExprTag, std::uint32_t>;
export class CoreExprArena;

export struct CoreLambda {
    std::vector<VarId> params;
    std::optional<VarId> rest_param;
    std::vector<CoreExprRef> body;
};

export struct CoreIf {
    CoreExprRef condition;
    CoreExprRef then_branch;
    CoreExprRef else_branch; // invalid for one-armed if
};

export struct CoreSet {
    VarId target;
    CoreExprRef value;
};
export struct CoreDefine {
    VarId target;
    CoreExprRef value;
};

export struct CoreQuote {
    SpanRef datum;
};

export struct CoreBegin {
    std::vector<CoreExprRef> exprs;
};
export struct CoreApply {
    CoreExprRef func;
    std::vector<CoreExprRef> args;
};

export struct CoreVar {
    VarId var;
};

export struct CoreLiteral {
    SpanRef value;
};

export class CoreExpr
    : public TaggedUnion<CoreLambda, CoreIf, CoreSet, CoreDefine, CoreQuote,
          CoreBegin, CoreApply, CoreVar, CoreLiteral> {
public:
    using TaggedUnion = TaggedUnion<CoreLambda, CoreIf, CoreSet, CoreDefine,
        CoreQuote, CoreBegin, CoreApply, CoreVar, CoreLiteral>;
    SpanRef origin;
    template <typename... Args>
    explicit CoreExpr(SpanRef origin, Args&&... args) noexcept
        : TaggedUnion(std::forward<Args>(args)...)
        , origin(origin) {
    }
};

class CoreExprArena : lpc::utils::Arena<CoreExprTag, CoreExpr, std::uint32_t> {
public:
    template <typename... Args>
    [[nodiscard]] CoreExprRef emplace(SpanRef origin, Args&&... args) {
        return Arena::emplace(origin, std::forward<Args>(args)...);
    }
    [[nodiscard]] const CoreExpr& at(CoreExprRef ref) const { return Arena::at(ref); }
};

} // namespace lpc::analysis
