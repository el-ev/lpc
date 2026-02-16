export module lpc.cps.ir;

import std;

import lpc.sema.core_form;
import lpc.syntax.span;
import lpc.syntax.refs;
import lpc.utils.arena;
import lpc.utils.tagged_union;

namespace lpc::cps {

using namespace lpc::sema;
using namespace lpc::syntax;

using lpc::utils::TaggedUnion;

export struct CpsExprTag { };
export using CpsExprRef
    = lpc::utils::ElementReference<CpsExprTag, std::uint32_t>;

export using CpsUnit = std::monostate;

export struct CpsVar {
    VarId var;

    [[nodiscard]] auto operator<=>(const CpsVar& other) const noexcept {
        return var <=> other.var;
    }

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
#define X(name, str) name,
#include "primops.def"
#undef X
};

export struct CpsLet {
    CpsVar target;
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
    CpsVar name;
    std::vector<CpsVar> params;
    CpsExprRef body;
};

export struct CpsFix {
    std::vector<CpsExprRef> functions;
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
        return Arena::emplace(std::forward<Args>(args)...);
    }

    [[nodiscard]] const CpsExpr& get(CpsExprRef ref) const {
        return at(ref);
    }

    [[nodiscard]] CpsExpr& get(CpsExprRef ref) {
        return at(ref);
    }
};

} // namespace lpc::cps

export template <>
struct std::hash<lpc::cps::CpsVar> {
    std::size_t operator()(const lpc::cps::CpsVar& v) const noexcept {
        return std::hash<lpc::sema::VarId> {}(v.var);
    }
};
