export module lpc.utils.function_ref;

import std;

namespace lpc::utils {

export template <typename Fn>
class function_ref;

export template <typename Ret, typename... Params>
class function_ref<Ret(Params...)> {
private:
    void* _callable = nullptr;
    Ret (*_callback)(void*, Params...) = nullptr;

    template <typename Callable>
    static Ret callback_fn(void* callable, Params... params) {
        return (*reinterpret_cast<Callable*>(callable))(
            std::forward<Params>(params)...);
    }

public:
    function_ref() = delete;

    template <typename Callable>
        requires(!std::is_same_v<std::remove_cvref_t<Callable>, function_ref>
                    && std::is_invocable_r_v<Ret, Callable, Params...>)
    function_ref(Callable&& callable) noexcept
        : _callable(reinterpret_cast<void*>(
              const_cast<std::remove_cvref_t<Callable>*>(&callable)))
        , _callback(callback_fn<std::remove_cvref_t<Callable>>) {
    }

    function_ref(const function_ref&) noexcept = default;
    function_ref& operator=(const function_ref&) noexcept = default;

    Ret operator()(Params... params) const {
        return _callback(_callable, std::forward<Params>(params)...);
    }
};

} // namespace lpc::utils
