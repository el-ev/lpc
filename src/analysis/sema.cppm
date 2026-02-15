export module lpc.analysis.sema;

import std;

import lpc.analysis.core_form;
import lpc.context;
import lpc.passes;
import lpc.syntax.arenas;
import lpc.syntax.ast;
import lpc.syntax.refs;

namespace lpc::analysis {

using namespace lpc::syntax;

class SymbolTable {
    struct Scope {
        std::unordered_map<std::string, VarId> bindings;
        std::unique_ptr<Scope> parent;
    };

    std::unique_ptr<Scope> _current;
    std::uint32_t _next_id = 0;

public:
    VarId define(const std::string& name) {
        VarId id { .id = _next_id++, .debug_name = name };
        _current->bindings[name] = id;
        return id;
    }

    [[nodiscard]] std::optional<VarId> resolve(const std::string& name) const {
        for (auto* s = _current.get(); s != nullptr; s = s->parent.get())
            if (auto it = s->bindings.find(name); it != s->bindings.end())
                return it->second;
        return std::nullopt;
    }

    void push_scope() {
        _current = std::make_unique<Scope>(
            Scope { .bindings = {}, .parent = std::move(_current) });
    }
    void pop_scope() {
        _current = std::move(_current->parent);
    }
};

export class SemaPass final : public Pass<SpanRef, SpanRef> {
private:
public:
    [[nodiscard]] std::string name() const noexcept final {
        return "sema";
    }

    [[nodiscard]] SpanRef run(
        SpanRef root, CompilerContext& ctx) noexcept final;

    [[nodiscard]] std::string dump(
        const SpanRef& /* result */, CompilerContext& /* ctx */) const noexcept final {
        return "";
    }

    [[nodiscard]] bool is_failed() const noexcept final {
        return false;
    }

    explicit SemaPass() noexcept = default;
};

} // namespace lpc::analysis
