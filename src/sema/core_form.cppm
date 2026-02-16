export module lpc.sema.core_form;

import std;

import lpc.syntax.refs;
import lpc.utils.arena;
import lpc.utils.tagged_union;

namespace lpc::sema {

using namespace lpc::syntax;
using lpc::utils::TaggedUnion;

export struct VarId {
    std::uint32_t id;
    std::string debug_name;

    auto operator<=>(const VarId& other) const noexcept {
        return id <=> other.id;
    }

    bool operator==(const VarId& other) const noexcept {
        return id == other.id;
    }
};

export struct CoreExprTag { };
export using CoreExprRef
    = lpc::utils::ElementReference<CoreExprTag, std::uint32_t>;
export class CoreExprArena;

export struct Arity {
    std::uint32_t min_args;
    std::uint32_t max_args; // 0 for variadic

    [[nodiscard]] static constexpr Arity fixed(std::uint32_t n) {
        return { .min_args = n, .max_args = n };
    }

    [[nodiscard]] static constexpr Arity at_least(std::uint32_t n) {
        return { .min_args = n, .max_args = 0 };
    }

    [[nodiscard]] constexpr bool is_applicable(std::uint32_t n) const noexcept {
        return min_args <= n && (max_args == 0 || n <= max_args);
    }

    [[nodiscard]] constexpr bool is_variadic() const noexcept {
        return max_args == 0;
    }
};

export enum class CoreVarKind : std::uint8_t { Local, Global, Builtin };

export struct CoreVar {
    VarId id;
    CoreVarKind kind = CoreVarKind::Local;

    [[nodiscard]] auto operator<=>(const CoreVar& other) const noexcept {
        return id <=> other.id;
    }

    [[nodiscard]] bool operator==(const CoreVar& other) const noexcept {
        return id == other.id;
    }
};

export struct CoreLambda {
    std::vector<CoreVar> params;
    std::optional<CoreVar> rest_param;
    CoreExprRef body;
};

export struct CoreIf {
    CoreExprRef condition;
    CoreExprRef then_branch;
    CoreExprRef else_branch; // invalid for one-armed if
};

export struct CoreSet {
    CoreVar target;
    CoreExprRef value;
};

export struct CoreDefine {
    CoreVar target;
    CoreExprRef value;
};

export struct CoreSeq {
    std::vector<CoreExprRef> exprs;
};

export struct CoreApply {
    CoreExprRef func;
    std::vector<CoreExprRef> args;
};

export struct CoreConstant {
    SpanRef value;
};

export class CoreExpr
    : public TaggedUnion<CoreLambda, CoreIf, CoreSet, CoreDefine, CoreSeq,
          CoreApply, CoreVar, CoreConstant> {
public:
    using TaggedUnion = TaggedUnion<CoreLambda, CoreIf, CoreSet, CoreDefine,
        CoreSeq, CoreApply, CoreVar, CoreConstant>;
    SpanRef origin;
    template <typename... Args>
    explicit CoreExpr(SpanRef origin, Args&&... args) noexcept
        : TaggedUnion(std::forward<Args>(args)...)
        , origin(origin) {
    }
};

class CoreExprArena : lpc::utils::Arena<CoreExprTag, CoreExpr, std::uint32_t> {
private:
    std::optional<std::set<CoreVar>> _mutated_vars;

public:
    template <typename... Args>
    [[nodiscard]] CoreExprRef emplace(SpanRef origin, Args&&... args) {
        return Arena::emplace(origin, std::forward<Args>(args)...);
    }
    [[nodiscard]] const CoreExpr& at(CoreExprRef ref) const {
        return Arena::at(ref);
    }
    [[nodiscard]] const CoreExpr& operator[](CoreExprRef ref) const {
        return at(ref);
    }

    void set_mutated_vars(std::set<CoreVar> vars) {
        _mutated_vars = std::move(vars);
    }

    [[nodiscard]] bool is_mutated(const CoreVar& var) const noexcept {
        if (!_mutated_vars.has_value()) // no data
            return true;
        return _mutated_vars->contains(var);
    }
};

} // namespace lpc::sema

export template <>
struct std::hash<lpc::sema::VarId> {
    std::size_t operator()(const lpc::sema::VarId& v) const noexcept {
        return std::hash<std::uint32_t> {}(v.id);
    }
};

export template <>
struct std::hash<lpc::sema::CoreVar> {
    std::size_t operator()(const lpc::sema::CoreVar& v) const noexcept {
        return std::hash<lpc::sema::VarId> {}(v.id);
    }
};
