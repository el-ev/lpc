export module lpc.frontend.transformer;

import std;
import lpc.frontend.ast;
import lpc.utils.arena;

namespace lpc::frontend {

export struct BindingValue {
    std::vector<SExprLocRef> values;
    bool is_list = false;

    static BindingValue single(SExprLocRef ref) {
        return BindingValue { .values = { ref }, .is_list = false };
    }

    static BindingValue list(std::vector<SExprLocRef> refs) {
        return BindingValue { .values = std::move(refs), .is_list = true };
    }
};

export using Bindings = std::unordered_map<std::string, BindingValue>;

export class Transformer {
public:
    struct SyntaxRule {
        SExprLocRef pattern;
        SExprLocRef template_;
    };

private:
    std::vector<SyntaxRule> _rules;
    SExprArena& _def_arena;

public:
    explicit Transformer(std::vector<SyntaxRule> rules, SExprArena& def_arena)
        : _rules(std::move(rules))
        , _def_arena(def_arena) { }

    [[nodiscard]] SExprLocRef transcribe(
        SExprLocRef input, SExprArena& input_arena) const;
};

} // namespace lpc::frontend
