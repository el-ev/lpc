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

    alignas(max_align) storage_t storage_;
    index_t index_;

    template <typename T>
    static constexpr index_t type_index() {
        constexpr std::array<bool, sizeof...(Types)> matches
            = { std::is_same_v<T, Types>... };
        for (std::size_t i = 0; i < sizeof...(Types); ++i) {
            if (matches[i])
                return static_cast<index_t>(i);
        }
        return static_cast<index_t>(-1);
    }

    template <std::size_t I>
    using type_at = std::tuple_element_t<I, std::tuple<Types...>>;

    template <typename T>
    T* storage_as() noexcept {
        return reinterpret_cast<T*>(&storage_);
    }

    template <typename T>
    const T* storage_as() const noexcept {
        return reinterpret_cast<const T*>(&storage_);
    }

    void destroy() {
        if (index_ != static_cast<index_t>(-1)) {
            destroy_impl(std::make_index_sequence<sizeof...(Types)> {});
            index_ = static_cast<index_t>(-1);
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
    static constexpr std::size_t npos = static_cast<std::size_t>(-1);

    // NOLINTNEXTLINE(cppcoreguidelines-pro-type-member-init, hicpp-member-init)
    constexpr TaggedUnion()
        : index_(static_cast<index_t>(-1)) {
    }

    template <typename T>
        requires(std::is_same_v<T, Types> || ...)
    // NOLINTNEXTLINE(cppcoreguidelines-pro-type-member-init, hicpp-member-init)
    explicit constexpr TaggedUnion(T&& value) {
        constexpr auto idx = type_index<std::decay_t<T>>();
        new (storage_as<std::decay_t<T>>())
            std::decay_t<T>(std::forward<T>(value));
        index_ = idx;
    }

    // NOLINTNEXTLINE(cppcoreguidelines-pro-type-member-init, hicpp-member-init)
    TaggedUnion(const TaggedUnion& other) {
        copy_construct(other, std::make_index_sequence<sizeof...(Types)> {});
    }

    // NOLINTNEXTLINE(cppcoreguidelines-pro-type-member-init, hicpp-member-init)
    TaggedUnion(TaggedUnion&& other) noexcept {
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
        requires(std::is_same_v<T, Types> || ...)
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

    [[nodiscard]] constexpr std::size_t index() const noexcept {
        return index_ == static_cast<index_t>(-1)
            ? npos
            : static_cast<std::size_t>(index_);
    }

    [[nodiscard]] constexpr bool valueless_by_exception() const noexcept {
        return index_ == static_cast<index_t>(-1);
    }

    template <typename T>
    [[nodiscard]] constexpr bool holds_alternative() const noexcept {
        return index_ == type_index<T>();
    }

    template <std::size_t I>
    constexpr std::optional<std::reference_wrapper<type_at<I>>>
    get() & noexcept {
        if (index_ != I)
            return std::nullopt;
        return get_unchecked<I>();
    }

    template <std::size_t I>
    constexpr std::optional<std::reference_wrapper<const type_at<I>>>
    get() const& noexcept {
        if (index_ != I)
            return std::nullopt;
        return get_unchecked<I>();
    }

    template <typename T>
    constexpr std::optional<std::reference_wrapper<T>> get() & noexcept {
        constexpr auto idx = type_index<T>();
        if (index_ != idx)
            return std::nullopt;
        return std::ref(get_unchecked<T>());
    }

    template <typename T>
    constexpr std::optional<std::reference_wrapper<const T>>
    get() const& noexcept {
        constexpr auto idx = type_index<T>();
        if (index_ != idx)
            return std::nullopt;
        return std::cref(get_unchecked<T>());
    }

    template <typename T>
    constexpr std::optional<T> get() && noexcept {
        constexpr auto idx = type_index<T>();
        if (index_ != idx)
            return std::nullopt;
        return std::move(get_unchecked<T>());
    }

    template <std::size_t I>
    constexpr auto& get_unchecked() & noexcept {
        return *storage_as<type_at<I>>();
    }

    template <std::size_t I>
    constexpr const auto& get_unchecked() const& noexcept {
        return *storage_as<type_at<I>>();
    }

    template <std::size_t I>
    constexpr auto&& get_unchecked() && noexcept {
        return std::move(*storage_as<type_at<I>>());
    }

    template <typename T>
    constexpr T& get_unchecked() & noexcept {
        return *storage_as<T>();
    }

    template <typename T>
    constexpr const T& get_unchecked() const& noexcept {
        return *storage_as<T>();
    }

    template <typename T>
    constexpr T&& get_unchecked() && noexcept {
        return std::move(*storage_as<T>());
    }

    template <typename T, typename... Args>
    T& emplace(Args&&... args) {
        destroy();
        constexpr auto idx = type_index<T>();
        // NOLINTNEXTLINE(cppcoreguidelines-owning-memory)
        T* ptr = new (storage_as<T>()) T(std::forward<Args>(args)...);
        index_ = idx;
        return *ptr;
    }

    template <typename Visitor>
    constexpr auto visit(Visitor&& vis) & {
        return visit_impl(std::forward<Visitor>(vis),
            std::make_index_sequence<sizeof...(Types)> {});
    }

    template <typename Visitor>
    constexpr auto visit(Visitor&& vis) const& {
        return visit_impl(std::forward<Visitor>(vis),
            std::make_index_sequence<sizeof...(Types)> {});
    }

    template <typename Visitor>
    constexpr auto visit(Visitor&& vis) && {
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

template <typename T, typename... Types>
constexpr bool holds_alternative(const TaggedUnion<Types...>& v) noexcept {
    return v.template holds_alternative<T>();
}

template <std::size_t I, typename... Types>
constexpr auto& get(TaggedUnion<Types...>& v) {
    return v.template get<I>();
}

template <std::size_t I, typename... Types>
constexpr const auto& get(const TaggedUnion<Types...>& v) {
    return v.template get<I>();
}

template <std::size_t I, typename... Types>
constexpr auto&& get(TaggedUnion<Types...>&& v) {
    return std::move(v).template get<I>();
}

template <typename T, typename... Types>
constexpr T& get(TaggedUnion<Types...>& v) {
    return v.template get<T>();
}

template <typename T, typename... Types>
constexpr const T& get(const TaggedUnion<Types...>& v) {
    return v.template get<T>();
}

template <typename T, typename... Types>
constexpr T&& get(TaggedUnion<Types...>&& v) {
    return std::move(v).template get<T>();
}

template <std::size_t I, typename... Types>
constexpr auto& get_unchecked(TaggedUnion<Types...>& v) noexcept {
    return v.template get_unchecked<I>();
}

template <std::size_t I, typename... Types>
constexpr const auto& get_unchecked(const TaggedUnion<Types...>& v) noexcept {
    return v.template get_unchecked<I>();
}

template <std::size_t I, typename... Types>
constexpr auto&& get_unchecked(TaggedUnion<Types...>&& v) noexcept {
    return std::move(v).template get_unchecked<I>();
}

template <typename T, typename... Types>
constexpr T& get_unchecked(TaggedUnion<Types...>& v) noexcept {
    return v.template get_unchecked<T>();
}

template <typename T, typename... Types>
constexpr const T& get_unchecked(const TaggedUnion<Types...>& v) noexcept {
    return v.template get_unchecked<T>();
}

template <typename T, typename... Types>
constexpr T&& get_unchecked(TaggedUnion<Types...>&& v) noexcept {
    return std::move(v).template get_unchecked<T>();
}

template <typename Visitor, typename... Types>
constexpr auto visit(Visitor&& vis, TaggedUnion<Types...>& variant) {
    return variant.visit(std::forward<Visitor>(vis));
}

template <typename Visitor, typename... Types>
constexpr auto visit(Visitor&& vis, const TaggedUnion<Types...>& variant) {
    return variant.visit(std::forward<Visitor>(vis));
}

template <typename Visitor, typename... Types>
constexpr auto visit(Visitor&& vis, TaggedUnion<Types...>&& variant) {
    return std::move(variant).visit(std::forward<Visitor>(vis));
}

} // namespace lpc::utils
