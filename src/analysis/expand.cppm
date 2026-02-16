export module lpc.analysis.expand;

import std;

import lpc.analysis.transformer;
import lpc.context;
import lpc.passes;
import lpc.syntax.arenas;
import lpc.syntax.ast;
import lpc.syntax.refs;
import lpc.utils.tagged_union;

namespace lpc::analysis {

using namespace lpc::syntax;

using lpc::utils::TaggedUnion;

struct VarBinding {
    LispIdent id;
};

struct CoreBinding { };
struct MacroBinding {
    std::unique_ptr<Transformer> transformer;
    std::optional<ScopeID> output_excluded_scope;
};

using Binding = TaggedUnion<VarBinding, CoreBinding, MacroBinding>;

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

    void add_binding(const std::string& name, const std::set<ScopeID>& scopes,
        Binding binding) {
        _bindings[name].push_back(
            BindingEntry { .scopes = scopes, .binding = std::move(binding) });
    }

    [[nodiscard]] const Binding* find_exact_binding(const std::string& name,
        const std::set<ScopeID>& scopes) const noexcept {
        auto it = _bindings.find(name);
        if (it == _bindings.end())
            return nullptr;
        for (const auto& entry : it->second)
            if (entry.scopes == scopes)
                return &entry.binding;
        return nullptr;
    }

    [[nodiscard]] const Binding* find_binding(const std::string& name,
        const std::set<ScopeID>& scopes,
        std::optional<ScopeID> exclude_scope = std::nullopt) const noexcept {

        auto it = _bindings.find(name);
        if (it == _bindings.end())
            return nullptr;

        const BindingEntry* best = nullptr;

        for (const auto& entry : it->second) {
            if (exclude_scope && entry.scopes.contains(*exclude_scope)
                && entry.binding.isa<MacroBinding>())
                continue;

            if (std::ranges::includes(scopes, entry.scopes)) {
                if (best == nullptr
                    || std::ranges::includes(entry.scopes, best->scopes)) {
                    // prefer later bindings over earlier ones
                    best = &entry;
                }
                // else if (std::ranges::includes(best->scopes, entry.scopes))
                // { best is a superset of entry, so best is already better
                // }
                else {
                    // ambiguous reference
                    // todo do something?
                }
            }
        }

        if (best != nullptr)
            return &best->binding;
        return nullptr;
    }

    void define_core_syntax(const std::string& name) {
        add_binding(name, {}, Binding { CoreBinding {} });
    }

private:
};

class Expander {
public:
    Expander(LexEnv& env, CompilerContext& ctx, bool& had_error)
        : _env(env)
        , _arena(ctx.span_arena())
        , _had_error(had_error)
        , _show_core(ctx.options().show_core_expansion)
        , _max_depth(ctx.options().max_expansion_depth) {
    }

    std::vector<SpanRef> expand(SpanRef root);

private:
    LexEnv& _env;
    SpanArena& _arena;
    bool& _had_error;

    SpanRef _parent = SpanRef::invalid();
    bool _show_core = false;
    bool _is_top_level = true;
    std::uint32_t _max_depth = 1000;
    std::uint32_t _current_depth = 0;
    std::optional<ScopeID> _output_excluded_scope;

    [[nodiscard]] SpanRef add_scope(SpanRef expr, ScopeID scope);

    template <typename T>
    SpanRef expand_list_like(SpanRef expr, const T& container, ScopeID scope) {
        std::vector<SpanRef> v;
        v.reserve(container.elem.size());
        bool changed = false;
        for (const auto& el : container.elem) {
            auto new_el = add_scope(el, scope);
            if (new_el != el)
                changed = true;
            v.push_back(new_el);
        }
        if (!changed)
            return expr;
        return _arena.expand(_arena.loc_ref(expr), _parent,
            _arena.scope_arena().empty_set(), T(std::move(v)));
    }

    bool report_error(SpanRef failed_expr, std::string_view msg);
    void dump_backtrace();

    template <typename... Args>
    bool report_error(
        SpanRef failed_expr, std::format_string<Args...> fmt, Args&&... args) {
        return report_error(
            failed_expr, std::format(fmt, std::forward<Args>(args)...));
    }

    bool check_arity(SpanRef el, const SExprList& list, std::size_t min_arity,
        std::size_t max_arity);
    bool is_identifier_active(SpanRef id_ref);

    std::vector<SpanRef> expand_lambda(const SExprList& list, SpanRef root);
    std::vector<SpanRef> expand_quote(const SExprList& list, SpanRef root);
    std::vector<SpanRef> expand_if(const SExprList& list, SpanRef root);
    std::vector<SpanRef> expand_begin(const SExprList& list, SpanRef root);
    std::vector<SpanRef> expand_set(const SExprList& list, SpanRef root);
    std::vector<SpanRef> expand_define(const SExprList& list, SpanRef root);
    std::vector<SpanRef> expand_define_syntax(
        const SExprList& list, SpanRef root);
    std::vector<SpanRef> expand_let_letrec_syntax(
        const SExprList& list, SpanRef root, bool is_letrec);
    std::vector<SpanRef> expand_macro(SpanRef root, const MacroBinding& macro);

    std::optional<std::unique_ptr<Transformer>> parse_syntax_rules(
        SpanRef transformer_spec,
        std::string_view form_prefix = "define-syntax");

public:
    [[nodiscard]] Expander with_parent(SpanRef parent) const {
        Expander next = *this;
        next._parent = parent;
        return next;
    }

    [[nodiscard]] Expander with_depth(std::uint32_t depth) const {
        Expander next = *this;
        next._current_depth = depth;
        return next;
    }

    [[nodiscard]] Expander as_sub_expression() const {
        Expander next = *this;
        next._is_top_level = false;
        return next;
    }

    [[nodiscard]] Expander with_excluded_scope(
        std::optional<ScopeID> scope) const {
        Expander next = *this;
        next._output_excluded_scope = scope;
        return next;
    }
};

export class ExpandPass final : public Pass<SpanRef, SpanRef> {
private:
    LexEnv _env;
    bool _core_loaded = false;
    bool _had_error = false;

    void load_core(CompilerContext& ctx);

public:
    [[nodiscard]] std::string name() const noexcept final {
        return "expand";
    }

    [[nodiscard]] SpanRef run(
        SpanRef root, CompilerContext& ctx) noexcept final;

    [[nodiscard]] std::string dump(
        const SpanRef& result, CompilerContext& ctx) const noexcept final {
        return ctx.span_arena().dump_root(result);
    }

    [[nodiscard]] bool is_failed() const noexcept final {
        return _had_error;
    }

    explicit ExpandPass() noexcept;
};

} // namespace lpc::analysis
