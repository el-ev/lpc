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
        SExprLocRef pattern_tail;
        SExprLocRef template_;
    };

private:
    std::vector<SyntaxRule> _rules;
    SExprArena& _arena;
    std::set<std::string> _literals;

public:
    explicit Transformer(std::vector<SyntaxRule> rules,
        std::vector<std::string> literals, SExprArena& arena)
        : _rules(std::move(rules))
        , _arena(arena)
        , _literals(std::make_move_iterator(literals.begin()),
              std::make_move_iterator(literals.end())) {
    }

    [[nodiscard]] SExprLocRef transcribe(SExprLocRef input) const;

private:
    [[nodiscard]] bool match(
        SExprLocRef pattern, SExprLocRef input, Bindings& bindings) const;

    [[nodiscard]] SExprLocRef instantiate(SExprLocRef element,
        const Bindings& bindings, LocRef call_site_loc) const;
};

} // namespace lpc::frontend
