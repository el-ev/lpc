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
        Scope* parent = nullptr;
    };

    std::deque<Scope> _scopes;
    Scope* _current = nullptr;
    std::uint32_t _next_id = 0;
    std::unordered_map<std::uint32_t, Arity> _builtins;
    std::set<std::uint32_t> _defined_globals;
    std::set<VarId> _referenced_globals;

public:
    explicit SymbolTable() {
        push_scope(); // Builtin scope
        init_builtins();
        push_scope(); // Global scope
    }

    [[nodiscard]] bool is_global_scope() const noexcept {
        return _scopes.size() == 2;
    }

    VarId declare_ref(const std::string& name) {
        if (auto it = _scopes[1].bindings.find(name);
            it != _scopes[1].bindings.end())
            return it->second;

        VarId id { .id = _next_id++, .debug_name = name, .kind = VarKind::Global };
        _scopes[1].bindings[name] = id;
        return id;
    }

    VarId define(const std::string& name, VarKind kind = VarKind::Local) {
        if (is_global_scope()) {
            if (auto it = _current->bindings.find(name);
                it != _current->bindings.end()) {
                auto& existing = it->second;
                _defined_globals.insert(existing.id);
                return existing;
            }
            kind = VarKind::Global;
        }

        VarId id { .id = _next_id++, .debug_name = name, .kind = kind };
        _current->bindings[name] = id;

        if (kind == VarKind::Global)
            _defined_globals.insert(id.id);
    
        return id;
    }

    void define_builtin(const std::string& name, Arity arity) {
        VarId id { .id = _next_id++, .debug_name = name, .kind = VarKind::Builtin };
        _scopes.front().bindings[name] = id;
        _builtins[id.id] = arity;
        _defined_globals.insert(id.id);
    }

    [[nodiscard]] std::optional<Arity> get_builtin_arity(const VarId& id) const {
        if (auto it = _builtins.find(id.id); it != _builtins.end())
            return it->second;
        return std::nullopt;
    }

    [[nodiscard]] std::optional<VarId> resolve(
        const std::string& name) const noexcept {
        for (auto* s = _current; s != nullptr; s = s->parent)
            if (auto it = s->bindings.find(name); it != s->bindings.end())
                return it->second;
        return std::nullopt;
    }

    void mark_referenced(const VarId& id) {
        if (id.kind == VarKind::Global)
            _referenced_globals.insert(id);
    }

    [[nodiscard]] std::vector<std::string> get_undefined_globals() const {
        return std::ranges::filter_view(_referenced_globals, [this](const VarId& var) {
            return !_defined_globals.contains(var.id);
        }) | std::views::transform([](const VarId& var){
            return var.debug_name;
        }) | std::ranges::to<std::vector>();
    }

    void push_scope() {
        _scopes.push_back(Scope { .bindings = {}, .parent = _current });
        _current = &_scopes.back();
    }

    void pop_scope() {
        _current = _current->parent;
    }

private:
    void init_builtins();
};

class Lowerer {
public:
    Lowerer(SpanArena& spans, CoreExprArena& core, bool show_core = false)
        : _spans(spans)
        , _core(core)
        , _show_core(show_core) {
    }

    CoreExprRef lower_program(SpanRef root);

    [[nodiscard]] bool had_error() const noexcept {
        return _had_error;
    }

private:
    SpanArena& _spans;
    CoreExprArena& _core;
    SymbolTable _syms;
    bool _show_core = false;
    bool _had_error = false;

    void report_error(SpanRef ref, std::string_view msg);

    template <typename... Args>
    void report_error(
        SpanRef ref, std::format_string<Args...> fmt, Args&&... args) {
        report_error(ref, std::format(fmt, std::forward<Args>(args)...));
    }

    [[nodiscard]] const std::string* head_name(const SExprList& list) const;

    CoreExprRef lower(SpanRef ref);
    CoreExprRef lower_variable(SpanRef ref, const LispIdent& id);
    CoreExprRef lower_literal(SpanRef ref);
    CoreExprRef lower_list(SpanRef ref, const SExprList& list);

    CoreExprRef lower_lambda(SpanRef ref, const SExprList& list);
    CoreExprRef lower_if(SpanRef ref, const SExprList& list);
    CoreExprRef lower_set(SpanRef ref, const SExprList& list);
    CoreExprRef lower_define(SpanRef ref, const SExprList& list);
    CoreExprRef lower_quote(SpanRef ref, const SExprList& list);
    CoreExprRef lower_begin(SpanRef ref, const SExprList& list);
    CoreExprRef lower_application(SpanRef ref, const SExprList& list);
};

export class SemaPass final : public Pass<SpanRef, CoreExprRef> {
private:
    bool _failed = false;

public:
    [[nodiscard]] std::string name() const noexcept final {
        return "sema";
    }

    [[nodiscard]] CoreExprRef run(
        SpanRef root, CompilerContext& ctx) noexcept final;

    [[nodiscard]] std::string dump(
        const CoreExprRef& result, CompilerContext& ctx) const noexcept final;

    [[nodiscard]] bool is_failed() const noexcept final {
        return _failed;
    }

    explicit SemaPass() noexcept = default;
};

} // namespace lpc::analysis
