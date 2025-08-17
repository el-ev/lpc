export module lpc.frontend.expand;

import std;
import lpc.frontend.ast;
import lpc.passes;
import lpc.utils.tagged_union;

namespace lpc::frontend {

using lpc::utils::TaggedUnion;

using ScopeID = std::uint32_t;

struct HygienicIdent {
    std::string name;
    std::set<ScopeID> scopes;

    bool operator==(const HygienicIdent& other) const noexcept {
        return name == other.name && scopes == other.scopes;
    }

    struct Hasher {
        std::size_t operator()(const HygienicIdent& ident) const noexcept {
            std::size_t hash = std::hash<std::string>()(ident.name);
            for (const auto& scope : ident.scopes) {
                hash ^= std::hash<ScopeID>()(scope) + 0x9e3779b9 + (hash << 6u)
                    + (hash >> 2u);
            }
            return hash;
        }
    };
};

class Transformer;

struct VarBinding { };
struct CoreBinding { };
struct MacroBinding {
    std::shared_ptr<Transformer> transformer;
};

using Binding = TaggedUnion<VarBinding, CoreBinding, MacroBinding>;

class LexEnv {
private:
    std::vector<
        std::unordered_map<HygienicIdent, Binding, HygienicIdent::Hasher>>
        _scopes;
    ScopeID _next = 0;

public:
    LexEnv() {
        push_scope();
    }

    void push_scope() {
        _scopes.emplace_back();
    }

    void pop_scope() {
        _scopes.pop_back();
    }

    ScopeID new_scope() {
        return _next++;
    }

    void add_binding(const HygienicIdent& id, Binding binding) {
        if (!_scopes.empty())
            _scopes.back()[id] = std::move(binding);
    }

    std::optional<Binding> find_binding(
        const HygienicIdent& id) const noexcept {
        for (const auto& _scope : std::ranges::reverse_view(_scopes)) {
            auto found = _scope.find(id);
            if (found != _scope.end())
                return found->second;
        }
        return std::nullopt;
    }

private:
    void define_core_syntax(const std::string& name) {
        add_binding(HygienicIdent { .name = name, .scopes = {} },
            Binding { CoreBinding {} });
    }
};

export class ExpandPass final : public Pass {
private:
    LexEnv _env;

    template <NodeType T>
    [[nodiscard]] NodeLocRef walk(NodeLocRef node, NodeArena& _arena) noexcept;

public:
    [[nodiscard]] std::string name() const noexcept final {
        return "expand";
    }

    [[nodiscard]] NodeLocRef run(
        NodeLocRef root, NodeArena& arena) noexcept final;

    // [[nodiscard]] int try_add_syntax_def(
    //     NodeLocRef list_node_ref, NodeArena& arena) noexcept;
    ExpandPass() = default;
    ~ExpandPass() final = default;
};

} // namespace lpc::frontend
