export module lpc.cps.ir;

import std;

import lpc.analysis.core_form;
import lpc.syntax.span;
import lpc.syntax.refs;
import lpc.utils.arena;
import lpc.utils.tagged_union;

namespace lpc::cps {

using namespace lpc::analysis;
using namespace lpc::syntax;

using lpc::utils::TaggedUnion;

export struct CpsExprTag { };
export using CpsExprRef
    = lpc::utils::ElementReference<CpsExprTag, std::uint32_t>;

export using CpsUnit = std::monostate;

export struct CpsVar {
    VarId var;

    [[nodiscard]] auto operator==(const CpsVar& other) const noexcept {
        return var == other.var;
    }
};

export struct CpsConstant {
    SpanRef value;

    [[nodiscard]] auto operator==(const CpsConstant& other) const noexcept {
        return value == other.value;
    }
};

export class CpsAtom : public TaggedUnion<CpsVar, CpsConstant, CpsUnit> {
public:
    CpsAtom() = default;

    template <typename T>
        requires(!std::is_same_v<std::decay_t<T>, CpsAtom>)
    explicit CpsAtom(T&& value) noexcept
        : TaggedUnion(std::forward<T>(value)) {
    }
};

export struct CpsApp {
    CpsAtom func;
    std::vector<CpsAtom> args;
};

export enum class PrimOp : std::uint8_t {
    Add,
    Sub,
    Mul,
    Div,
    Eq,
    Lt,
    Gt,
    Le,
    Ge,
    Cons,
    Car,
    Cdr,
    Box,
    BoxGet,
    BoxSet,
};

export struct CpsLet {
    VarId target;
    PrimOp op;
    std::vector<CpsAtom> args;
    CpsExprRef body;
};

export struct CpsIf {
    CpsAtom condition;
    CpsExprRef then_branch;
    CpsExprRef else_branch;
};

export struct CpsLambda {
    VarId name;
    std::vector<VarId> params;
    CpsExprRef body;
};

export struct CpsFix {
    std::vector<CpsLambda> functions;
    CpsExprRef body;
};

export struct CpsHalt {
    CpsAtom value;
};

export class CpsExpr
    : public TaggedUnion<CpsApp, CpsLet, CpsIf, CpsFix, CpsHalt, CpsLambda> {
public:
    using TaggedUnion::TaggedUnion;
};

export class CpsArena
    : public lpc::utils::Arena<CpsExprTag, CpsExpr, std::uint32_t> {
public:
    explicit CpsArena() noexcept = default;

    template <typename... Args>
    [[nodiscard]] CpsExprRef emplace(Args&&... args) {
        return emplace(std::forward<Args>(args)...);
    }

    [[nodiscard]] const CpsExpr& get(CpsExprRef ref) const {
        return at(ref);
    }

    [[nodiscard]] CpsExpr& get(CpsExprRef ref) {
        return at(ref);
    }
};

} // namespace lpc::cps
