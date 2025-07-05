export module lpc.frontend.canonicalize;

import std;
import lpc.frontend.ast;
import lpc.passes;
import lpc.frontend.builtin;

namespace lpc::frontend {

class SymbolMapping {
private:
    std::vector<std::map<ASTNodeRef, ASTNodeRef>> _mappings;

public:
    SymbolMapping() = default;

    void push_scope() {
        _mappings.emplace_back();
    }

    void pop_scope() {
        if (!_mappings.empty())
            _mappings.pop_back();
    }

    void add_mapping(ASTNodeRef from, ASTNodeRef to) {
        if (!_mappings.empty())
            _mappings.back()[from] = to;
    }

    [[nodiscard]] std::optional<ASTNodeRef> get_mapping(ASTNodeRef from) const {
        for (const auto& _mapping : std::ranges::reverse_view(_mappings)) {
            auto found = _mapping.find(from);
            if (found != _mapping.end())
                return found->second;
        }
        return std::nullopt;
    }
};

export class CanonicalizePass final : public Pass {
private:
    std::size_t _counter = 0;
    SymbolMapping _symbol_mapping;
    BuiltinFunctions _builtins;

    [[nodiscard]] NodeLocRef visit(
        NodeLocRef node, NodeArena& arena, bool top_level) noexcept;

public:
    [[nodiscard]] std::string name() const noexcept final {
        return "canonicalize";
    }

    [[nodiscard]] NodeLocRef run(
        NodeLocRef root, NodeArena& arena) noexcept final;

    CanonicalizePass() = default;
    ~CanonicalizePass() final = default;
};

} // namespace lpc::frontend
