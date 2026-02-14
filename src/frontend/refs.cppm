export module lpc.frontend.refs;

import std;

import lpc.utils.arena;

namespace lpc::frontend {

using lpc::utils::ElementReference;

export struct LocTag {};
export struct SExprTag {};

export using LocRef   = ElementReference<LocTag, std::uint32_t>;
export using SExprRef = ElementReference<SExprTag, std::uint32_t>;

export class SExprLocRef {
private:
    SExprRef _expr_ref;
    LocRef _loc_ref;

public:
    explicit SExprLocRef() noexcept = default;
    explicit SExprLocRef(SExprRef expr_ref, LocRef loc_ref) noexcept
        : _expr_ref(expr_ref)
        , _loc_ref(loc_ref) { };

    static constexpr SExprLocRef invalid() noexcept {
        return SExprLocRef(SExprRef::invalid(), LocRef::invalid());
    }

    [[nodiscard]] inline constexpr bool is_valid() const noexcept {
        return _expr_ref.is_valid();
    }

    [[nodiscard]] inline constexpr SExprRef expr_ref() const noexcept {
        return _expr_ref;
    }

    [[nodiscard]] inline constexpr LocRef loc_ref() const noexcept {
        return _loc_ref;
    }

    [[nodiscard]] inline constexpr bool operator==(
        const SExprLocRef& other) const noexcept {
        return expr_ref() == other.expr_ref() && loc_ref() == other.loc_ref();
    }
};

} // namespace lpc::frontend