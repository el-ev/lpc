export module lpc.syntax.ast;

import std;

export import lpc.syntax.token;
import lpc.core.refs;
import lpc.utils.tagged_union;

namespace lpc::syntax {

using namespace lpc::core;

using lpc::utils::TaggedUnion;

export class LispNil { };

export inline constexpr bool operator==(
    const LispNil& /* lhs */, const LispNil& /* rhs */) noexcept {
    return true;
}

export class LispIdent {
public:
    std::string name;

    explicit LispIdent(std::string name)
        : name(std::move(name)) {
    }

    [[nodiscard]] inline constexpr bool operator==(
        const LispIdent& other) const noexcept {
        return name == other.name;
    }
};
export using LispString = std::string;
export class SExpr;

export struct SExprList {
    std::vector<SpanRef> elem;

    [[nodiscard]] explicit SExprList() noexcept = default;
    [[nodiscard]] explicit SExprList(std::vector<SpanRef>&& elements) noexcept
        : elem(std::move(elements)) { };
};

export inline constexpr bool operator==(
    const SExprList& lhs, const SExprList& rhs) noexcept {
    return lhs.elem == rhs.elem;
}

export struct SExprVector {
    std::vector<SpanRef> elem;
    [[nodiscard]] explicit SExprVector() noexcept = default;
    [[nodiscard]] explicit SExprVector(std::vector<SpanRef>&& elements) noexcept
        : elem(std::move(elements)) { };
};

export inline constexpr bool operator==(
    const SExprVector& lhs, const SExprVector& rhs) noexcept {
    return lhs.elem == rhs.elem;
}

export class SExpr
    : public TaggedUnion<LispNil, LispIdent, LispString, LispNumber, LispChar,
          LispBool, SExprList, SExprVector> {
public:
    using TaggedUnion = TaggedUnion<LispNil, LispIdent, LispString, LispNumber,
        LispChar, LispBool, SExprList, SExprVector>;

    template <typename... Args>
    [[nodiscard]] explicit SExpr(Args&&... args) noexcept
        : TaggedUnion(std::forward<Args>(args)...) {
    }
};

} // namespace lpc::syntax
