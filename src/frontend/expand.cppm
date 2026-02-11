export module lpc.frontend.expand;

import std;
import lpc.frontend.ast;
import lpc.passes;
import lpc.frontend.transformer;
import lpc.utils.tagged_union;

namespace lpc::frontend {

using lpc::utils::TaggedUnion;

using ScopeID = lpc::frontend::ScopeID;

struct VarBinding {
    LispIdent id;
};

struct CoreBinding { };
struct MacroBinding {
    std::shared_ptr<Transformer> transformer;
    bool is_stdlib = false;
};

using Binding = TaggedUnion<VarBinding, CoreBinding, MacroBinding>;

export struct ExpansionFrame {
    SExprLocRef expr;
    bool is_stdlib;
    std::uint32_t parent;
};

export class ExpansionStack {
private:
    std::vector<ExpansionFrame> _frames;

public:
    static constexpr std::uint32_t INVALID
        = std::numeric_limits<std::uint32_t>::max();

    ExpansionStack() = default;

    [[nodiscard]] std::uint32_t push(
        SExprLocRef expr, bool is_stdlib, std::uint32_t parent) {
        auto idx = static_cast<std::uint32_t>(_frames.size());
        _frames.push_back({ expr, is_stdlib, parent });
        return idx;
    }

    [[nodiscard]] const ExpansionFrame& at(std::uint32_t idx) const {
        return _frames[idx];
    }

    [[nodiscard]] std::uint32_t size() const noexcept {
        return static_cast<std::uint32_t>(_frames.size());
    }
};

using ExpStackRef = std::uint32_t;

class LexEnv {
private:
    struct BindingEntry {
        std::set<ScopeID> scopes;
        Binding binding;
    };

    std::unordered_map<std::string, std::vector<BindingEntry>> _bindings;
    ScopeID _next = 0;

public:
    LexEnv() = default;

    ScopeID new_scope() {
        return _next++;
    }

    void add_binding(const LispIdent& id, Binding binding) {
        _bindings[id.name].push_back(BindingEntry {
            .scopes = id.scopes, .binding = std::move(binding) });
    }

    [[nodiscard]] std::optional<Binding> find_binding(
        const LispIdent& id) const noexcept {

        auto it = _bindings.find(id.name);
        if (it == _bindings.end())
            return std::nullopt;

        const BindingEntry* best = nullptr;
        std::size_t best_size = 0;

        for (const auto& entry : it->second) {
            if (std::ranges::includes(id.scopes, entry.scopes)) {
                if (best == nullptr || entry.scopes.size() > best_size) {
                    best = &entry;
                    best_size = entry.scopes.size();
                }
            }
        }

        if (best != nullptr)
            return best->binding;
        return std::nullopt;
    }

    void define_core_syntax(const std::string& name) {
        add_binding(LispIdent(name), Binding { CoreBinding {} });
    }

private:
};

export class ExpandPass final : public Pass {
private:
    std::unique_ptr<SExprArena> _stdlib_arena;
    LexEnv _env;
    ExpansionStack _exp_stack;
    bool _stdlib_loaded = false;
    bool _show_stdlib_expansion = false;
    bool _had_error = false;

    void load_stdlib(SExprArena& user_arena);

public:
    [[nodiscard]] std::string name() const noexcept final {
        return "expand";
    }

    void set_show_stdlib_expansion(bool v) noexcept {
        _show_stdlib_expansion = v;
    }

    [[nodiscard]] SExprLocRef run(
        SExprLocRef root, SExprArena& arena) noexcept final;

    explicit ExpandPass(bool show_stdlib_expansion) noexcept;
    ~ExpandPass() final = default;
};

} // namespace lpc::frontend
