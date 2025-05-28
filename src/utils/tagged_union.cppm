export module lpc.utils.tagged_union;

import std;

namespace lpc::utils {

export template <typename... Types>
class TaggedUnion {
private:
    static constexpr std::size_t max_size = std::max({ sizeof(Types)... });
    static constexpr std::size_t max_align = std::max({ alignof(Types)... });

    using storage_t = std::byte[max_size];
    using index_t = std::uint8_t;

    static_assert(sizeof...(Types) < std::numeric_limits<index_t>::max() - 1,
        "This TaggedUnion cannot hold this many types. ");

    template <typename T, typename... Rest>
    [[nodiscard]] static consteval bool are_types_distinct() {
        if constexpr (sizeof...(Rest) == 0) {
            return true;
        } else {
            return (!std::is_same_v<T, Rest> && ...)
                && are_types_distinct<Rest...>();
        }
    }

    static_assert(are_types_distinct<Types...>(),
        "All types in TaggedUnion must be distinct.");

    alignas(max_align) storage_t storage_;
    index_t index_;

    template <typename T>
    [[nodiscard]] static constexpr index_t type_index() {
        constexpr std::array<bool, sizeof...(Types)> matches
            = { std::is_same_v<T, Types>... };
        for (std::size_t i = 0; i < sizeof...(Types); ++i) {
            if (matches[i])
                return static_cast<index_t>(i);
        }
        static_assert(std::disjunction_v<std::is_same<T, Types>...>,
            "This TaggedUnion does not hold the specified type.");
        return static_cast<index_t>(-1);
    }

    template <std::size_t I>
    using type_at = std::tuple_element_t<I, std::tuple<Types...>>;

    template <typename T>
    [[nodiscard]] T* storage_as() noexcept {
        verify_type<T>();
        return reinterpret_cast<T*>(&storage_);
    }

    template <typename T>
    [[nodiscard]] const T* storage_as() const noexcept {
        verify_type<T>();
        return reinterpret_cast<const T*>(&storage_);
    }

    void destroy() {
        if (index_ != -1) {
            destroy_impl(std::make_index_sequence<sizeof...(Types)> {});
            index_ = -1;
        }
    }

    template <std::size_t... Is>
    void destroy_impl(std::index_sequence<Is...> /* indices */) {
        ((index_ == Is ? storage_as<type_at<Is>>()->~type_at<Is>() : void()),
            ...);
    }

    template <std::size_t... Is>
    void copy_construct(
        const TaggedUnion& other, std::index_sequence<Is...> /* indices */) {
        ((other.index_ == Is ? (new (storage_as<type_at<Is>>()) type_at<Is>(
                                    *other.storage_as<type_at<Is>>()),
                                   void())
                             : void()),
            ...);
        index_ = other.index_;
    }

    template <std::size_t... Is>
    void move_construct(
        // NOLINTNEXTLINE(cppcoreguidelines-rvalue-reference-param-not-moved)
        TaggedUnion&& other, std::index_sequence<Is... /* indices */>) {
        ((other.index_ == Is
                 ? (new (storage_as<type_at<Is>>()) type_at<Is>(
                        std::move(*other.storage_as<type_at<Is>>())),
                       void())
                 : void()),
            ...);
        index_ = other.index_;
        other.index_ = static_cast<index_t>(-1);
    }

public:
    static constexpr index_t npos_ = std::numeric_limits<index_t>::max();

    // NOLINTNEXTLINE(cppcoreguidelines-pro-type-member-init, hicpp-member-init)
    constexpr TaggedUnion()
        : index_(npos_) {
    }

    template <typename T>
        requires(!std::is_same_v<std::decay_t<T>, TaggedUnion>)
    // NOLINTNEXTLINE(cppcoreguidelines-pro-type-member-init, hicpp-member-init)
    [[nodiscard]] explicit constexpr TaggedUnion(T&& value) {
        constexpr auto idx = type_index<std::decay_t<T>>();
        new (storage_as<std::decay_t<T>>())
            std::decay_t<T>(std::forward<T>(value));
        index_ = idx;
    }

    // NOLINTNEXTLINE(cppcoreguidelines-pro-type-member-init, hicpp-member-init)
    [[nodiscard]] TaggedUnion(const TaggedUnion& other) {
        copy_construct(other, std::make_index_sequence<sizeof...(Types)> {});
    }

    // NOLINTNEXTLINE(cppcoreguidelines-pro-type-member-init, hicpp-member-init)
    [[nodiscard]] TaggedUnion(TaggedUnion&& other) noexcept {
        move_construct(
            std::move(other), std::make_index_sequence<sizeof...(Types)> {});
    }

    TaggedUnion& operator=(const TaggedUnion& other) {
        if (this != &other) {
            destroy();
            copy_construct(
                other, std::make_index_sequence<sizeof...(Types)> {});
        }
        return *this;
    }

    TaggedUnion& operator=(TaggedUnion&& other) noexcept {
        if (this != &other) {
            destroy();
            move_construct(std::move(other),
                std::make_index_sequence<sizeof...(Types)> {});
        }
        return *this;
    }

    template <typename T>
        requires(!std::is_same_v<std::decay_t<T>, TaggedUnion>)
    TaggedUnion& operator=(T&& value) {
        destroy();
        constexpr auto idx = type_index<std::decay_t<T>>();
        new (storage_as<std::decay_t<T>>())
            std::decay_t<T>(std::forward<T>(value));
        index_ = idx;
        return *this;
    }

    ~TaggedUnion() {
        destroy();
    }

    template <index_t I>
    static consteval void verify_index() {
        static_assert(I < sizeof...(Types),
            "Specified index is out of bounds for this TaggedUnion.");
    }

    template <typename T>
    static consteval void verify_type() {
        static_assert((std::is_same_v<T, Types> || ...),
            "This TaggedUnion does not hold the specified type.");
    }

    [[nodiscard]] constexpr std::size_t index() const noexcept {
        return index_ == npos_ ? static_cast<std::size_t>(-1)
                               : static_cast<index_t>(index_);
    }

    [[nodiscard]] constexpr bool valueless() const noexcept {
        return index_ == npos_;
    }

    template <typename T>
    [[nodiscard]] constexpr bool holds_alternative() const noexcept {
        verify_type<T>();
        return index_ == type_index<T>();
    }

    template <index_t I>
    [[nodiscard]] constexpr std::optional<std::reference_wrapper<type_at<I>>>
    get() & noexcept {
        if (index_ != I)
            return std::nullopt;
        return get_unchecked<I>();
    }

    template <index_t I>
    [[nodiscard]] constexpr std::optional<
        std::reference_wrapper<const type_at<I>>>
    get() const& noexcept {
        if (index_ != I)
            return std::nullopt;
        return get_unchecked<I>();
    }

    template <typename T>
    [[nodiscard]] constexpr std::optional<std::reference_wrapper<T>>
    get() & noexcept {
        constexpr auto idx = type_index<T>();
        if (index_ != idx)
            return std::nullopt;
        return std::ref(get_unchecked<T>());
    }

    template <typename T>
    [[nodiscard]] constexpr std::optional<std::reference_wrapper<const T>>
    get() const& noexcept {
        constexpr auto idx = type_index<T>();
        if (index_ != idx)
            return std::nullopt;
        return std::cref(get_unchecked<T>());
    }

    template <typename T>
    [[nodiscard]] constexpr std::optional<T> get() && noexcept {
        constexpr auto idx = type_index<T>();
        if (index_ != idx)
            return std::nullopt;
        return std::move(get_unchecked<T>());
    }

    template <index_t I>
    [[nodiscard]] constexpr auto& get_unchecked() & noexcept {
        verify_index<I>();
        return *storage_as<type_at<I>>();
    }

    template <index_t I>
    [[nodiscard]] constexpr const auto& get_unchecked() const& noexcept {
        verify_index<I>();
        return *storage_as<type_at<I>>();
    }

    template <index_t I>
    [[nodiscard]] constexpr auto&& get_unchecked() && noexcept {
        verify_index<I>();
        return std::move(*storage_as<type_at<I>>());
    }

    template <typename T>
    [[nodiscard]] constexpr T& get_unchecked() & noexcept {
        verify_type<T>();
        return *storage_as<T>();
    }

    template <typename T>
    [[nodiscard]] constexpr const T& get_unchecked() const& noexcept {
        verify_type<T>();
        return *storage_as<T>();
    }

    template <typename T>
    [[nodiscard]] constexpr T&& get_unchecked() && noexcept {
        verify_type<T>();
        return std::move(*storage_as<T>());
    }

    template <typename T, typename... Args>
    T& emplace(Args&&... args) {
        verify_type<T>();
        destroy();
        constexpr auto idx = type_index<T>();
        // NOLINTNEXTLINE(cppcoreguidelines-owning-memory)
        T* ptr = new (storage_as<T>()) T(std::forward<Args>(args)...);
        index_ = idx;
        return *ptr;
    }

    template <typename Visitor>
    [[nodiscard]] constexpr auto visit(Visitor&& vis) & {
        return visit_impl(std::forward<Visitor>(vis),
            std::make_index_sequence<sizeof...(Types)> {});
    }

    template <typename Visitor>
    [[nodiscard]] constexpr auto visit(Visitor&& vis) const& {
        return visit_impl(std::forward<Visitor>(vis),
            std::make_index_sequence<sizeof...(Types)> {});
    }

    template <typename Visitor>
    [[nodiscard]] constexpr auto visit(Visitor&& vis) && {
        return visit_impl(std::forward<Visitor>(vis),
            std::make_index_sequence<sizeof...(Types)> {});
    }

private:
    template <typename Visitor, std::size_t... Is>
    constexpr auto visit_impl(
        Visitor&& vis, std::index_sequence<Is...> /* indices */) & {
        using return_type
            = std::common_type_t<decltype(vis(*storage_as<type_at<Is>>()))...>;

        constexpr auto jump_table = std::array {
            // NOLINTNEXTLINE(cppcoreguidelines-rvalue-reference-param-not-moved)
            +[](Visitor&& v, storage_t& s) -> return_type {
                return v(*reinterpret_cast<type_at<Is>*>(&s));
            }...
        };

        if (index_ >= sizeof...(Types))
            throw std::bad_variant_access {};
        return jump_table[index_](std::forward<Visitor>(vis), storage_);
    }

    template <typename Visitor, std::size_t... Is>
    constexpr auto visit_impl(
        Visitor&& vis, std::index_sequence<Is...> /* indices */) const& {
        using return_type
            = std::common_type_t<decltype(vis(*storage_as<type_at<Is>>()))...>;

        constexpr auto jump_table = std::array {
            // NOLINTNEXTLINE(cppcoreguidelines-rvalue-reference-param-not-moved)
            +[](Visitor&& v, const storage_t& s) -> return_type {
                return v(*reinterpret_cast<const type_at<Is>*>(&s));
            }...
        };

        if (index_ >= sizeof...(Types))
            throw std::bad_variant_access {};
        return jump_table[index_](std::forward<Visitor>(vis), storage_);
    }

    template <typename Visitor, std::size_t... Is>
    constexpr auto visit_impl(
        Visitor&& vis, std::index_sequence<Is...> /* indices */) && {
        using return_type = std::common_type_t<decltype(vis(
            std::move(*storage_as<type_at<Is>>())))...>;

        constexpr auto jump_table = std::array {
            // NOLINTNEXTLINE(cppcoreguidelines-rvalue-reference-param-not-moved)
            +[](Visitor&& v, storage_t& s) -> return_type {
                return v(std::move(*reinterpret_cast<type_at<Is>*>(&s)));
            }...
        };

        if (index_ >= sizeof...(Types))
            throw std::bad_variant_access {};
        return jump_table[index_](std::forward<Visitor>(vis), storage_);
    }
};

export namespace tagged_union {

    template <typename T, typename... Types>
    [[nodiscard]] constexpr bool holds_alternative(
        const TaggedUnion<Types...>& v) noexcept {
        return v.template holds_alternative<T>();
    }

    template <typename... Types, TaggedUnion<Types...>::index_t I>
    [[nodiscard]] constexpr auto& get(TaggedUnion<Types...>& v) {
        return v.template get<I>();
    }

    template <typename... Types, TaggedUnion<Types...>::index_t I>
    [[nodiscard]] constexpr const auto& get(const TaggedUnion<Types...>& v) {
        return v.template get<I>();
    }

    template <typename... Types, TaggedUnion<Types...>::index_t I>
    [[nodiscard]] constexpr auto&& get(TaggedUnion<Types...>&& v) {
        return std::move(v).template get<I>();
    }

    template <typename T, typename... Types>
    [[nodiscard]] constexpr T& get(TaggedUnion<Types...>& v) {
        return v.template get<T>();
    }

    template <typename T, typename... Types>
    [[nodiscard]] constexpr const T& get(const TaggedUnion<Types...>& v) {
        return v.template get<T>();
    }

    template <typename T, typename... Types>
    [[nodiscard]] constexpr T&& get(TaggedUnion<Types...>&& v) {
        return std::move(v).template get<T>();
    }

    template <typename... Types, TaggedUnion<Types...>::index_t I>
    [[nodiscard]] constexpr auto& get_unchecked(
        TaggedUnion<Types...>& v) noexcept {
        return v.template get_unchecked<I>();
    }

    template <typename... Types, TaggedUnion<Types...>::index_t I>
    [[nodiscard]] constexpr const auto& get_unchecked(
        const TaggedUnion<Types...>& v) noexcept {
        return v.template get_unchecked<I>();
    }

    template <typename... Types, TaggedUnion<Types...>::index_t I>
    [[nodiscard]] constexpr auto&& get_unchecked(
        TaggedUnion<Types...>&& v) noexcept {
        return std::move(v).template get_unchecked<I>();
    }

    template <typename T, typename... Types>
    [[nodiscard]] constexpr T& get_unchecked(
        TaggedUnion<Types...>& v) noexcept {
        return v.template get_unchecked<T>();
    }

    template <typename T, typename... Types>
    [[nodiscard]] constexpr const T& get_unchecked(
        const TaggedUnion<Types...>& v) noexcept {
        return v.template get_unchecked<T>();
    }

    template <typename T, typename... Types>
    [[nodiscard]] constexpr T&& get_unchecked(
        TaggedUnion<Types...>&& v) noexcept {
        return std::move(v).template get_unchecked<T>();
    }

    template <typename Visitor, typename... Types>
    [[nodiscard]] constexpr auto visit(
        Visitor&& vis, TaggedUnion<Types...>& variant) {
        return variant.visit(std::forward<Visitor>(vis));
    }

    template <typename Visitor, typename... Types>
    [[nodiscard]] constexpr auto visit(
        Visitor&& vis, const TaggedUnion<Types...>& variant) {
        return variant.visit(std::forward<Visitor>(vis));
    }

    template <typename Visitor, typename... Types>
    [[nodiscard]] constexpr auto visit(
        Visitor&& vis, TaggedUnion<Types...>&& variant) {
        return std::move(variant).visit(std::forward<Visitor>(vis));
    }

} // namespace tagges_union

} // namespace lpc::utils
