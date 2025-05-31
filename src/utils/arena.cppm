export module lpc.utils.arena;

import std;

namespace lpc::utils {

export template <typename T, typename IndexType = std::size_t>
class Arena {
private:
    std::vector<T> _data;
    IndexType _next_index = 0;

    struct ElementReference {
    public:
        static const IndexType invalid_ref
            = std::numeric_limits<IndexType>::max();

        ElementReference() noexcept : _index(invalid_ref) {}
        ElementReference(const ElementReference&) = default;
        ElementReference& operator=(const ElementReference&) = default;

        [[nodiscard]] inline static ElementReference invalid() noexcept {
            return ElementReference(invalid_ref);
        }

        [[nodiscard]] inline bool operator==(
            const ElementReference& other) const noexcept {
            return _index == other._index;
        }

        [[nodiscard]] inline bool operator!=(
            const ElementReference& other) const noexcept {
            return !(*this == other);
        }

        [[nodiscard]] inline bool is_valid() const noexcept {
            return _index != invalid_ref;
        }

    private:
        IndexType _index;
        explicit ElementReference(IndexType idx) noexcept
            : _index(idx) { };

        friend Arena<T, IndexType>;
    };

protected:
    explicit Arena() noexcept = default;
    explicit Arena(std::size_t initial_capacity) noexcept
        : _data() {
        _data.reserve(initial_capacity);
    }

public:
    using elem_ref = ElementReference;

    Arena(const Arena&) = delete;
    Arena& operator=(const Arena&) = delete;

    Arena(Arena&&) noexcept = default;
    Arena& operator=(Arena&&) noexcept = default;

    [[nodiscard]] inline std::size_t size() const noexcept {
        return _data.size();
    }

    [[nodiscard]] inline std::size_t capacity() const noexcept {
        return _data.capacity();
    }

    [[nodiscard]] inline bool empty() const noexcept {
        return _data.empty();
    }

    inline void reserve(std::size_t size) {
        _data.reserve(size);
    }

    inline void clear() noexcept {
        _data.clear();
        _next_index = 0;
    }

    [[nodiscard]] elem_ref insert(const T& value);
    template <typename... Args>
    [[nodiscard]] elem_ref emplace(Args&&... args);
    [[nodiscard]] elem_ref emplace(T&& value);

    inline void pop_back() noexcept {
        if (!_data.empty()) {
            _data.pop_back();
            --_next_index;
        }
    }

    [[nodiscard]] constexpr T& at(elem_ref ref);
    [[nodiscard]] constexpr const T& at(elem_ref ref) const;
    [[nodiscard]] constexpr T* get(elem_ref ref) noexcept;
    [[nodiscard]] constexpr const T* get(elem_ref ref) const noexcept;

    [[nodiscard]] inline constexpr elem_ref back_ref() const noexcept {
        if (_data.empty())
            return elem_ref::invalid();
        return elem_ref(_next_index - 1);
    }

    using const_iterator = typename std::vector<T>::const_iterator;

    [[nodiscard]] inline constexpr const_iterator begin() const noexcept {
        return _data.begin();
    }

    [[nodiscard]] inline constexpr const_iterator end() const noexcept {
        return _data.end();
    }

    [[nodiscard]] inline constexpr const_iterator cbegin() const noexcept {
        return _data.cbegin();
    }

    [[nodiscard]] inline constexpr const_iterator cend() const noexcept {
        return _data.cend();
    }
};

template <typename T, typename IndexType>
Arena<T, IndexType>::elem_ref Arena<T, IndexType>::insert(const T& value) {
    _data.push_back(value);
    return Arena<T, IndexType>::elem_ref(_next_index++);
}

template <typename T, typename IndexType>
template <typename... Args>
Arena<T, IndexType>::elem_ref Arena<T, IndexType>::emplace(Args&&... args) {
    _data.emplace_back(std::forward<Args>(args)...);
    return Arena<T, IndexType>::elem_ref(_next_index++);
}

template <typename T, typename IndexType>
Arena<T, IndexType>::elem_ref Arena<T, IndexType>::emplace(T&& value) {
    _data.push_back(std::move(value));
    return Arena<T, IndexType>::elem_ref(_next_index++);
}

template <typename T, typename IndexType>
constexpr T& Arena<T, IndexType>::at(Arena<T, IndexType>::elem_ref ref) {
    if (ref._index >= _data.size())
        throw std::out_of_range("Index out of range");
    return _data[ref._index];
}

template <typename T, typename IndexType>
constexpr const T& Arena<T, IndexType>::at(
    Arena<T, IndexType>::elem_ref ref) const {
    if (ref._index >= _data.size())
        throw std::out_of_range("Index out of range");
    return _data[ref._index];
}

template <typename T, typename IndexType>
constexpr T* Arena<T, IndexType>::get(
    Arena<T, IndexType>::elem_ref ref) noexcept {
    if (ref._index >= _data.size())
        return nullptr;
    return &_data[ref._index];
}

template <typename T, typename IndexType>
constexpr const T* Arena<T, IndexType>::get(
    Arena<T, IndexType>::elem_ref ref) const noexcept {
    if (ref._index >= _data.size())
        return nullptr;
    return &_data[ref._index];
}

} // namespace lpc::utils
