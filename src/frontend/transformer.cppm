export module lpc.frontend.transformer;

import std;

import lpc.frontend.ast;
import lpc.frontend.arenas;
import lpc.frontend.refs;

namespace lpc::frontend {

export struct BindingValue {
    std::vector<SpanRef> values;
    bool is_list = false;

    static BindingValue single(SpanRef ref) {
        return BindingValue { .values = { ref }, .is_list = false };
    }

    static BindingValue list(std::vector<SpanRef> refs) {
        return BindingValue { .values = std::move(refs), .is_list = true };
    }
};

export using Bindings = std::unordered_map<std::string, BindingValue>;

export class Transformer {
public:
    struct SyntaxRule {
        SpanRef pattern_tail;
        SpanRef template_;
    };

private:
    std::vector<SyntaxRule> _rules;
    SpanArena& _arena;
    std::set<std::string> _literals;

public:
    explicit Transformer(std::vector<SyntaxRule> rules,
        std::vector<std::string> literals, SpanArena& arena)
        : _rules(std::move(rules))
        , _arena(arena)
        , _literals(std::make_move_iterator(literals.begin()),
              std::make_move_iterator(literals.end())) {
    }

    [[nodiscard]] SpanRef transcribe(SpanRef input, SpanRef parent) const;

private:
    [[nodiscard]] bool match(
        SpanRef pattern, SpanRef input, Bindings& bindings) const;

    [[nodiscard]] SpanRef instantiate(SpanRef element,
        const Bindings& bindings, LocRef call_site_loc, SpanRef parent) const;
};

} // namespace lpc::frontend
