export module lpc.passes;

import std;

import lpc.frontend.ast;
import lpc.frontend.arenas;
import lpc.frontend.refs;

namespace lpc {
using namespace lpc::frontend;

export class Pass {
public:
    virtual ~Pass() = default;

    [[nodiscard]] virtual std::string name() const noexcept = 0;
    [[nodiscard]] virtual SpanRef run(
        SpanRef root, SpanArena& arena) noexcept
        = 0;

    Pass(const Pass&) = delete;
    Pass& operator=(const Pass&) = delete;
    Pass(Pass&&) = default;
    Pass& operator=(Pass&&) = default;

protected:
    explicit Pass() = default;
};

export class PassManager {
private:
    std::vector<std::unique_ptr<Pass>> _passes;

public:
    explicit PassManager() = default;

    PassManager(const PassManager&) = delete;
    PassManager& operator=(const PassManager&) = delete;

    PassManager(PassManager&&) = default;
    PassManager& operator=(PassManager&&) = default;

    template <typename T, typename... Args>
        requires std::is_base_of_v<Pass, T>
    void add_pass(Args&&... args) {
        _passes.emplace_back(std::make_unique<T>(std::forward<Args>(args)...));
    }

    template <typename P, typename... Ps>
        requires std::is_base_of_v<Pass, P>
        && (std::is_base_of_v<Pass, Ps> && ...)
    void add_passes() {
        _passes.emplace_back(std::make_unique<P>());
        if constexpr (sizeof...(Ps) > 0) {
            add_passes<Ps...>();
        }
    }

    [[nodiscard]] SpanRef run_all(SpanRef root, SpanArena& arena,
        std::vector<std::string>& print_passes) noexcept;

    void clear() noexcept {
        _passes.clear();
    }

    [[nodiscard]] std::size_t size() const noexcept {
        return _passes.size();
    }
};

} // namespace lpc
