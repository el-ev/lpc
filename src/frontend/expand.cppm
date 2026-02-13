export module lpc.frontend.expand;

import std;
import lpc.frontend.ast;
import lpc.passes;
import lpc.frontend.transformer;
import lpc.utils.tagged_union;

namespace lpc::frontend {

using lpc::utils::TaggedUnion;

struct VarBinding {
    LispIdent id;
};

struct CoreBinding { };
struct MacroBinding {
    std::unique_ptr<Transformer> transformer;
    bool is_core = false;
    std::optional<ScopeID> output_excluded_scope;
};

using Binding = TaggedUnion<VarBinding, CoreBinding, MacroBinding>;

export struct ExpansionFrame {
    SExprLocRef expr;
    bool is_core;
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
        SExprLocRef expr, bool is_core, std::uint32_t parent) {
        auto idx = static_cast<std::uint32_t>(_frames.size());
        _frames.push_back({ expr, is_core, parent });
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
    std::unordered_map<std::string, std::uint32_t> _name_counts;
    ScopeID _next = 0;

public:
    LexEnv() = default;

    ScopeID new_scope() {
        return _next++;
    }

    std::string unique_name(const std::string& name) {
        auto& count = _name_counts[name];
        return name + "." + std::to_string(count++);
    }

    // TODO: signal error when shadowing causes ambiguity
    void add_binding(const LispIdent& id, Binding binding) {
        _bindings[id.name].push_back(BindingEntry {
            .scopes = id.scopes, .binding = std::move(binding) });
    }

    [[nodiscard]] std::optional<std::reference_wrapper<const Binding>> find_exact_binding(const LispIdent& id) const noexcept {
        auto it = _bindings.find(id.name);
        if (it == _bindings.end())
            return std::nullopt;
        for (const auto& entry : it->second) {
            if (entry.scopes == id.scopes)
                return std::cref(entry.binding);
        }
        return std::nullopt;
    }

    [[nodiscard]] std::optional<std::reference_wrapper<const Binding>> find_binding(const LispIdent& id,
        std::optional<ScopeID> exclude_scope = std::nullopt) const noexcept {

        auto it = _bindings.find(id.name);
        if (it == _bindings.end())
            return std::nullopt;

        const BindingEntry* best = nullptr;
        std::size_t best_size = 0;

        for (const auto& entry : it->second) {
            if (exclude_scope && entry.scopes.contains(*exclude_scope)
                && entry.binding.isa<MacroBinding>())
                continue;
            if (std::ranges::includes(id.scopes, entry.scopes)) {
                // If the scopes are the same size, we prefer the newer binding
                if (best == nullptr || entry.scopes.size() >= best_size) {
                    best = &entry;
                    best_size = entry.scopes.size();
                }
            }
        }

        if (best != nullptr)
            return std::cref(best->binding);
        return std::nullopt;
    }

    void define_core_syntax(const std::string& name) {
        add_binding(LispIdent(name), Binding { CoreBinding {} });
    }

private:
};

export class ExpandPass final : public Pass {
private:
    std::unique_ptr<SExprArena> _core_arena;
    LexEnv _env;
    ExpansionStack _exp_stack;
    bool _core_loaded = false;
    bool _show_core_expansion = false;
    std::uint32_t _max_expansion_depth = 1000;
    bool _had_error = false;

    void load_core(SExprArena& user_arena);

public:
    [[nodiscard]] std::string name() const noexcept final {
        return "expand";
    }

    void set_show_core_expansion(bool v) noexcept {
        _show_core_expansion = v;
    }

    void set_max_expansion_depth(std::uint32_t v) noexcept {
        _max_expansion_depth = v;
    }

    [[nodiscard]] SExprLocRef run(
        SExprLocRef root, SExprArena& arena) noexcept final;

    explicit ExpandPass(
        bool show_core_expansion, std::uint32_t max_expansion_depth) noexcept;
    ~ExpandPass() final = default;
};

} // namespace lpc::frontend
