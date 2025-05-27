export module lpc.utils.arena;

import std;

namespace lpc::utils {

export template <typename T, typename IndexType = std::size_t>
class Arena {
private:
    std::vector<T> _data;
    IndexType _next_index = 0;

public:
    explicit Arena() noexcept = default;
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

    IndexType insert(const T& value);
    template <typename... Args>
    IndexType emplace(Args&&... args);
    IndexType emplace(T&& value);

    [[nodiscard]] constexpr T& operator[](IndexType index);
    [[nodiscard]] constexpr const T& operator[](IndexType index) const;
    [[nodiscard]] constexpr T& at(IndexType index);
    [[nodiscard]] constexpr const T& at(IndexType index) const;
    [[nodiscard]] constexpr T* get(IndexType index) noexcept;
    [[nodiscard]] constexpr const T* get(IndexType index) const noexcept;

    [[nodiscard]] inline constexpr T& back() {
        if (_data.empty())
            throw std::out_of_range("Arena is empty");
        return _data.back();
    }

    [[nodiscard]] inline constexpr const T& back() const {
        if (_data.empty())
            throw std::out_of_range("Arena is empty");
        return _data.back();
    }

    [[nodiscard]] inline constexpr IndexType next_index() const noexcept {
        return _next_index;
    }

    using iterator = typename std::vector<T>::iterator;
    using const_iterator = typename std::vector<T>::const_iterator;

    [[nodiscard]] inline constexpr iterator begin() noexcept {
        return _data.begin();
    }
    
    [[nodiscard]] inline constexpr iterator end() noexcept {
        return _data.end();
    }

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
IndexType Arena<T, IndexType>::insert(const T& value) {
    _data.push_back(value);
    return _next_index++;
}

template <typename T, typename IndexType>
template <typename... Args>
IndexType Arena<T, IndexType>::emplace(Args&&... args) {
    _data.emplace_back(std::forward<Args>(args)...);
    return _next_index++;
}

template <typename T, typename IndexType>
IndexType Arena<T, IndexType>::emplace(T&& value) {
    _data.push_back(std::move(value));
    return _next_index++;
}

template <typename T, typename IndexType>
constexpr T& Arena<T, IndexType>::operator[](IndexType index) {
    if (index >= _data.size())
        throw std::out_of_range("Index out of range");
    return _data[index];
}

template <typename T, typename IndexType>
constexpr const T& Arena<T, IndexType>::operator[](IndexType index) const {
    if (index >= _data.size())
        throw std::out_of_range("Index out of range");
    return _data[index];
}

template <typename T, typename IndexType>
constexpr T& Arena<T, IndexType>::at(IndexType index) {
    if (index >= _data.size())
        throw std::out_of_range("Index out of range");
    return _data[index];
}

template <typename T, typename IndexType>
constexpr const T& Arena<T, IndexType>::at(IndexType index) const {
    if (index >= _data.size())
        throw std::out_of_range("Index out of range");
    return _data[index];
}

template <typename T, typename IndexType>
constexpr T* Arena<T, IndexType>::get(IndexType index) noexcept {
    if (index >= _data.size())
        return nullptr;
    return &_data[index];
}

template <typename T, typename IndexType>
constexpr const T* Arena<T, IndexType>::get(IndexType index) const noexcept {
    if (index >= _data.size())
        return nullptr;
    return &_data[index];
}

} // namespace lpc::utils
