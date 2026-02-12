module lpc.frontend.expand;

import std;
import lpc.utils.logging;
import lpc.frontend.lexer;
import lpc.frontend.syntax;

namespace lpc::frontend {

using lpc::utils::Error;

struct ExpCtx {
    LexEnv& env;
    SExprArena& arena;
    ExpansionStack& stack;
    ExpStackRef parent;
    bool is_stdlib;
    bool show_stdlib;
    bool is_top_level;
    bool& had_error;
};

[[nodiscard]] static SExprLocRef add_scope(
    SExprLocRef expr, ScopeID scope, SExprArena& arena) {
    if (!expr.is_valid())
        return expr;
    const auto& sexpr = arena.at(expr);
    if (sexpr.holds_alternative<LispIdent>()) {
        auto ident = sexpr.get_unchecked<LispIdent>();
        ident.scopes.insert(scope);
        return arena.emplace(expr.loc_ref(), std::move(ident));
    }
    if (sexpr.holds_alternative<SExprList>()) {
        auto elems = sexpr.get_unchecked<SExprList>().elem; // copy
        std::vector<SExprLocRef> v;
        v.reserve(elems.size());
        for (const auto& el : elems)
            v.push_back(add_scope(el, scope, arena));
        return arena.emplace(expr.loc_ref(), SExprList(std::move(v)));
    }
    if (sexpr.holds_alternative<SExprVector>()) {
        auto elems = sexpr.get_unchecked<SExprVector>().elem; // copy
        std::vector<SExprLocRef> v;
        v.reserve(elems.size());
        for (const auto& el : elems)
            v.push_back(add_scope(el, scope, arena));
        return arena.emplace(expr.loc_ref(), SExprVector(std::move(v)));
    }
    return expr;
}

[[nodiscard]] static SExprLocRef strip_scopes(
    SExprLocRef expr, SExprArena& arena) {
    if (!expr.is_valid())
        return expr;
    const auto& sexpr = arena.at(expr);
    if (sexpr.holds_alternative<LispIdent>()) {
        auto ident = sexpr.get_unchecked<LispIdent>();
        ident.scopes.clear();
        return arena.emplace(expr.loc_ref(), std::move(ident));
    }
    if (sexpr.holds_alternative<SExprList>()) {
        auto elems = sexpr.get_unchecked<SExprList>().elem;
        std::vector<SExprLocRef> v;
        v.reserve(elems.size());
        for (const auto& el : elems)
            v.push_back(strip_scopes(el, arena));
        return arena.emplace(expr.loc_ref(), SExprList(std::move(v)));
    }
    return expr;
}

struct ScopeKey {
    std::string name;
    std::set<ScopeID> scopes;
    auto operator<=>(const ScopeKey&) const = default;
};

static void collect_idents(SExprLocRef root, SExprArena& arena,
    std::map<std::string, std::set<std::set<ScopeID>>>& groups) {
    if (!root.is_valid())
        return;
    const auto& expr = arena.at(root);
    if (expr.holds_alternative<LispIdent>()) {
        const auto& id = expr.get_unchecked<LispIdent>();
        groups[id.name].insert(id.scopes);
    } else if (expr.holds_alternative<SExprList>()) {
        for (const auto& el : expr.get_unchecked<SExprList>().elem)
            collect_idents(el, arena, groups);
    } else if (expr.holds_alternative<SExprVector>()) {
        for (const auto& el : expr.get_unchecked<SExprVector>().elem)
            collect_idents(el, arena, groups);
    }
}

static SExprLocRef apply_names(SExprLocRef root, SExprArena& arena,
    const std::map<ScopeKey, std::string>& name_map) {
    if (!root.is_valid())
        return root;
    const auto& expr = arena.at(root);
    if (expr.holds_alternative<LispIdent>()) {
        const auto& id = expr.get_unchecked<LispIdent>();
        auto it
            = name_map.find(ScopeKey { .name = id.name, .scopes = id.scopes });
        if (it != name_map.end())
            return arena.emplace(root.loc_ref(), LispIdent(it->second));
        return root;
    }
    if (expr.holds_alternative<SExprList>()) {
        const auto& list = expr.get_unchecked<SExprList>();
        std::vector<SExprLocRef> v;
        v.reserve(list.elem.size());
        for (const auto& el : list.elem)
            v.push_back(apply_names(el, arena, name_map));
        return arena.emplace(root.loc_ref(), SExprList(std::move(v)));
    }
    if (expr.holds_alternative<SExprVector>()) {
        const auto& vec = expr.get_unchecked<SExprVector>();
        std::vector<SExprLocRef> v;
        v.reserve(vec.elem.size());
        for (const auto& el : vec.elem)
            v.push_back(apply_names(el, arena, name_map));
        return arena.emplace(root.loc_ref(), SExprVector(std::move(v)));
    }
    return root;
}

[[nodiscard]] static SExprLocRef resolve_names(
    SExprLocRef root, SExprArena& arena) {
    std::map<std::string, std::set<std::set<ScopeID>>> groups;
    collect_idents(root, arena, groups);

    std::map<ScopeKey, std::string> name_map;
    for (const auto& [name, scope_sets] : groups) {
        if (scope_sets.size() == 1) {
            name_map[{ name, *scope_sets.begin() }] = name;
            continue;
        }
        bool bare_assigned = false;
        int suffix = 0;
        for (const auto& scopes : scope_sets) {
            if (scopes.empty() && !bare_assigned) {
                name_map[{ name, scopes }] = name;
                bare_assigned = true;
            } else {
                name_map[{ name, scopes }]
                    = name + "." + std::to_string(suffix++);
            }
        }
        if (!bare_assigned) {
            auto first_it = scope_sets.begin();
            name_map[{ name, *first_it }] = name;
        }
    }
    return apply_names(root, arena, name_map);
}

[[nodiscard]] static SExprLocRef make_canonical(
    LocRef loc, const std::string& name, SExprArena& arena) {
    return arena.emplace(loc, LispIdent(name));
}

[[nodiscard]] static SExprLocRef expand(SExprLocRef root, ExpCtx ctx);

static SExprLocRef expand_lambda(
    const SExprList& list, SExprLocRef root, ExpCtx ctx) {
    // (lambda (params…) body…)   or   (lambda rest-arg body…)
    if (list.elem.size() < 3)
        return root;

    ScopeID scope = ctx.env.new_scope();
    auto scoped_params = add_scope(list.elem[1], scope, ctx.arena);

    auto bind = [&](SExprLocRef p) {
        if (ctx.arena.at(p).holds_alternative<LispIdent>()) {
            auto id = ctx.arena.at(p).get_unchecked<LispIdent>();
            ctx.env.add_binding(id, Binding(VarBinding { id }));
        }
    };
    const auto& p_expr = ctx.arena.at(scoped_params);
    if (p_expr.holds_alternative<SExprList>()) {
        for (const auto& p : p_expr.get_unchecked<SExprList>().elem)
            bind(p);
    } else if (p_expr.holds_alternative<LispIdent>()) {
        bind(scoped_params);
    }

    std::vector<SExprLocRef> out;
    out.push_back(make_canonical(list.elem[0].loc_ref(), "lambda", ctx.arena));
    out.push_back(scoped_params);
    for (std::size_t i = 2; i < list.elem.size(); ++i) {
        auto body_ctx = ctx;
        body_ctx.is_top_level = false;
        auto r = expand(add_scope(list.elem[i], scope, ctx.arena), body_ctx);
        if (!r.is_valid())
            return r;
        out.push_back(r);
    }
    return ctx.arena.emplace(root.loc_ref(), SExprList(std::move(out)));
}

static SExprLocRef expand_quote(
    const SExprList& list, SExprLocRef root, SExprArena& arena) {
    if (list.elem.size() < 2)
        return root;
    std::vector<SExprLocRef> out;
    out.push_back(make_canonical(list.elem[0].loc_ref(), "quote", arena));
    out.push_back(strip_scopes(list.elem[1], arena));
    out.push_back(arena.emplace(root.loc_ref(), LispNil()));
    return arena.emplace(root.loc_ref(), SExprList(std::move(out)));
}

static SExprLocRef expand_simple(const std::string& kw, const SExprList& list,
    SExprLocRef root, ExpCtx ctx) {
    std::vector<SExprLocRef> out;
    out.push_back(make_canonical(list.elem[0].loc_ref(), kw, ctx.arena));
    for (std::size_t i = 1; i < list.elem.size(); ++i) {
        auto sub_ctx = ctx;
        if (kw != "begin")
            sub_ctx.is_top_level = false;
        // else: begin at top-level stays top-level (simplification)

        auto r = expand(list.elem[i], sub_ctx);
        if (!r.is_valid())
            return r;
        out.push_back(r);
    }
    return ctx.arena.emplace(root.loc_ref(), SExprList(std::move(out)));
}

static SExprLocRef expand_set(
    const SExprList& list, SExprLocRef root, ExpCtx ctx) {
    // (set! variable expression)
    if (list.elem.size() < 3)
        return root;
    std::vector<SExprLocRef> out;
    out.push_back(make_canonical(list.elem[0].loc_ref(), "set!", ctx.arena));
    for (std::size_t i = 1; i < list.elem.size(); ++i) {
        auto val_ctx = ctx;
        val_ctx.is_top_level = false;
        auto r = expand(list.elem[i], val_ctx);
        if (!r.is_valid())
            return r;
        out.push_back(r);
    }
    return ctx.arena.emplace(root.loc_ref(), SExprList(std::move(out)));
}

static SExprLocRef expand_define(
    const SExprList& list, SExprLocRef root, ExpCtx ctx) {
    if (list.elem.size() < 3)
        return root;

    auto var = list.elem[1];

    if (ctx.arena.at(var).holds_alternative<SExprList>()) {
        const auto& var_list = ctx.arena.at(var).get_unchecked<SExprList>().elem;
        if (var_list.empty())
            return root;

        auto func_name = var_list[0];

        std::vector<SExprLocRef> params;
        std::size_t var_logical = var_list.size();
        if (!var_list.empty()
            && ctx.arena.at(var_list.back()).holds_alternative<LispNil>())
            var_logical--;
        for (std::size_t i = 1; i < var_logical; ++i)
            params.push_back(var_list[i]);
        params.push_back(ctx.arena.emplace(var.loc_ref(), LispNil()));
        auto params_node
            = ctx.arena.emplace(var.loc_ref(), SExprList(std::move(params)));

        std::vector<SExprLocRef> lam;
        lam.push_back(
            make_canonical(list.elem[0].loc_ref(), "lambda", ctx.arena));
        lam.push_back(params_node);
        for (std::size_t i = 2; i < list.elem.size(); ++i)
            lam.push_back(list.elem[i]);
        auto lam_node
            = ctx.arena.emplace(var.loc_ref(), SExprList(std::move(lam)));

        std::vector<SExprLocRef> def;
        def.push_back(
            make_canonical(list.elem[0].loc_ref(), "define", ctx.arena));
        def.push_back(func_name);
        def.push_back(lam_node);
        def.push_back(ctx.arena.emplace(root.loc_ref(), LispNil()));
        auto desugared
            = ctx.arena.emplace(root.loc_ref(), SExprList(std::move(def)));
        return expand(desugared, ctx);
    }

    // (define var expr)
    if (ctx.arena.at(var).holds_alternative<LispIdent>()) {
        ctx.env.add_binding(ctx.arena.at(var).get_unchecked<LispIdent>(),
            Binding(VarBinding { ctx.arena.at(var).get_unchecked<LispIdent>() }));
    }

    std::vector<SExprLocRef> out;
    out.push_back(make_canonical(list.elem[0].loc_ref(), "define", ctx.arena));
    out.push_back(var);
    for (std::size_t i = 2; i < list.elem.size(); ++i) {
        auto val_ctx = ctx;
        val_ctx.is_top_level = false;
        auto r = expand(list.elem[i], val_ctx);
        if (!r.is_valid())
            return r;
        out.push_back(r);
    }
    return ctx.arena.emplace(root.loc_ref(), SExprList(std::move(out)));
}

static void report_syntax_error(
    const std::string& message, SExprLocRef expr, const ExpCtx& ctx) {
    ctx.had_error = true;
    Error("{}", message);

    auto loc = ctx.arena.location(expr.loc_ref());

    auto cur = ctx.parent;
    if (cur != ExpansionStack::INVALID) {
        std::println(std::cerr, "  in: {}", ctx.arena.dump(expr.expr_ref()));
        std::println(std::cerr, "  at {}", loc.source_location());
    } else {
        std::println(std::cerr, "  at {}", loc.source_location());
    }

    int stdlib_omitted = 0;
    while (cur != ExpansionStack::INVALID) {
        const auto& f = ctx.stack.at(cur);
        if (f.is_stdlib && !ctx.show_stdlib) {
            stdlib_omitted++;
            cur = f.parent;
            continue;
        }
        if (stdlib_omitted > 0) {
            std::println(std::cerr, "  ({} frames omitted)", stdlib_omitted);
            stdlib_omitted = 0;
        }
        auto r = resolve_names(f.expr, ctx.arena);
        std::println(
            std::cerr, "  in expansion of: {}", ctx.arena.dump(r.expr_ref()));
        cur = f.parent;
    }
    if (stdlib_omitted > 0)
        std::println(std::cerr, "  ({} frames omitted)", stdlib_omitted);
}

static SExprLocRef expand_define_syntax(
    const SExprList& list, SExprLocRef root, ExpCtx ctx) {
    // (define-syntax name (syntax-rules (literals…) (pattern template) …))
    if (!ctx.is_top_level) {
        report_syntax_error("define-syntax allowed only at top level",
            resolve_names(root, ctx.arena), ctx);
        return root;
    }
    if (list.elem.size() < 3) {
        report_syntax_error(
            "define-syntax: bad syntax (missing name or transformer path)",
            root, ctx);
        return root;
    }

    auto name_ex = list.elem[1];
    auto transformer_spec = list.elem[2];

    if (!ctx.arena.at(name_ex).holds_alternative<LispIdent>()) {
        report_syntax_error(
            "define-syntax: expected identifier for macro name", name_ex, ctx);
        return root;
    }
    auto macro_name = ctx.arena.at(name_ex).get<LispIdent>()->get();

    if (!ctx.arena.at(transformer_spec).holds_alternative<SExprList>()) {
        report_syntax_error("define-syntax: expected (syntax-rules ...)",
            transformer_spec, ctx);
        return root;
    }
    const auto& spec_list
        = ctx.arena.at(transformer_spec).get<SExprList>()->get().elem;
    if (spec_list.empty()) {
        report_syntax_error("define-syntax: expected (syntax-rules ...)",
            transformer_spec, ctx);
        return root;
    }

    if (!ctx.arena.at(spec_list[0]).holds_alternative<LispIdent>()) {
        report_syntax_error(
            "define-syntax: expected syntax-rules keyword", spec_list[0], ctx);
        return root;
    }
    if (ctx.arena.at(spec_list[0]).get<LispIdent>()->get().name
        != "syntax-rules") {
        report_syntax_error(
            "define-syntax: expected syntax-rules keyword", spec_list[0], ctx);
        return root;
    }

    // spec_list[1]: literals list
    // spec_list[2…]: rules
    std::vector<std::string> literals;
    if (spec_list.size() >= 2) {
        if (ctx.arena.at(spec_list[1]).holds_alternative<SExprList>()) {
            const auto& lit_list
                = ctx.arena.at(spec_list[1]).get_unchecked<SExprList>().elem;
            for (const auto& lit : lit_list) {
                if (ctx.arena.at(lit).holds_alternative<LispIdent>()) {
                    literals.push_back(
                        ctx.arena.at(lit).get_unchecked<LispIdent>().name);
                }
            }
        }
    }

    std::vector<Transformer::SyntaxRule> rules;
    for (std::size_t i = 2; i < spec_list.size(); ++i) {
        if (!ctx.arena.at(spec_list[i]).holds_alternative<SExprList>())
            continue;
        const auto& rule_parts
            = ctx.arena.at(spec_list[i]).get_unchecked<SExprList>().elem;
        if (rule_parts.size() >= 2)
            rules.push_back({ rule_parts[0], rule_parts[1] });
    }

    auto transformer = std::make_shared<Transformer>(
        std::move(rules), std::move(literals), ctx.arena);
    ctx.env.add_binding(macro_name,
        Binding(MacroBinding {
            .transformer = transformer, .is_stdlib = ctx.is_stdlib }));
    return ctx.arena.emplace(root.loc_ref(), LispNil());
}

static void report_expansion_error(SExprLocRef failed_expr,
    ExpansionStack& stack, ExpStackRef frame, SExprArena& arena,
    bool show_stdlib, bool& had_error,
    std::string_view msg = "no syntax-rules pattern matched") {
    had_error = true;

    auto resolved = resolve_names(failed_expr, arena);
    auto failed_str = arena.dump(resolved.expr_ref());

    Error("macro expansion failed: {}", msg);
    std::println(std::cerr, "  for: {}", failed_str);

    auto cur = frame;
    int stdlib_omitted = 0;
    while (cur != ExpansionStack::INVALID) {
        const auto& f = stack.at(cur);
        if (f.is_stdlib && !show_stdlib) {
            stdlib_omitted++;
            cur = f.parent;
            continue;
        }
        if (stdlib_omitted > 0) {
            std::println(std::cerr, "  ({} frames omitted)", stdlib_omitted);
            stdlib_omitted = 0;
        }
        auto r = resolve_names(f.expr, arena);
        std::println(
            std::cerr, "  in expansion of: {}", arena.dump(r.expr_ref()));
        cur = f.parent;
    }
    if (stdlib_omitted > 0)
        std::println(std::cerr, "  ({} frames omitted)", stdlib_omitted);

    auto loc = arena.location(failed_expr.loc_ref());
    std::println(std::cerr, "  at {}", loc.source_location());
}

static SExprLocRef expand_macro(
    SExprLocRef root, const MacroBinding& macro, ExpCtx ctx) {
    ScopeID intro = ctx.env.new_scope();
    auto scoped_in = add_scope(root, intro, ctx.arena);
    auto result = macro.transformer->transcribe(scoped_in, ctx.arena);
    if (!result.is_valid()) {
        report_expansion_error(root, ctx.stack, ctx.parent, ctx.arena,
            ctx.show_stdlib, ctx.had_error);
        return root;
    }
    auto new_ctx = ctx;
    new_ctx.is_stdlib = macro.is_stdlib;
    return expand(add_scope(result, intro, ctx.arena), new_ctx);
}

[[nodiscard]] static SExprLocRef expand(SExprLocRef root, ExpCtx ctx) {
    if (!root.is_valid())
        return root;

    const auto& sexpr = ctx.arena.at(root);

    if (sexpr.holds_alternative<LispIdent>()) {
        const auto& ident = sexpr.get_unchecked<LispIdent>();
        auto binding = ctx.env.find_binding(ident);
        if (!binding) {
            auto clean = ident;
            clean.scopes.clear();
            return ctx.arena.emplace(root.loc_ref(), std::move(clean));
        }
        if (binding->holds_alternative<VarBinding>())
            return ctx.arena.emplace(
                root.loc_ref(), binding->get_unchecked<VarBinding>().id);
        return root;
    }

    if (sexpr.holds_alternative<SExprList>()) {
        auto list = sexpr.get_unchecked<SExprList>();
        if (list.elem.empty())
            return root;

        const auto& head_expr = ctx.arena.at(list.elem[0]);
        if (head_expr.holds_alternative<LispIdent>()) {
            auto head_id = head_expr.get_unchecked<LispIdent>();
            auto binding = ctx.env.find_binding(head_id);

            if (binding && binding->holds_alternative<CoreBinding>()) {
                const auto& name = head_id.name;
                if (name == "lambda")
                    return expand_lambda(list, root, ctx);
                if (name == "quote")
                    return expand_quote(list, root, ctx.arena);
                if (name == "if" || name == "begin")
                    return expand_simple(name, list, root, ctx);
                if (name == "set!")
                    return expand_set(list, root, ctx);
                if (name == "define")
                    return expand_define(list, root, ctx);
                if (name == "define-syntax")
                    return expand_define_syntax(list, root, ctx);
                if (name == "syntax-error") {
                    std::string msg;
                    if (list.elem.size() >= 3) {
                        auto msg_expr = ctx.arena.at(list.elem[2]);
                        if (msg_expr.holds_alternative<LispString>()) {
                            msg = msg_expr.get_unchecked<LispString>();
                        }
                    }
                    report_expansion_error(root, ctx.stack, ctx.parent,
                        ctx.arena, ctx.show_stdlib, ctx.had_error, msg);
                    return root;
                }
            }

            if (binding && binding->holds_alternative<MacroBinding>()) {
                bool is_stdlib = binding->get_unchecked<MacroBinding>().is_stdlib;
                ExpStackRef frame = ctx.stack.push(root, is_stdlib, ctx.parent);
                auto macro_ctx = ctx;
                macro_ctx.parent = frame;
                return expand_macro(
                    root, binding->get<MacroBinding>()->get(), macro_ctx);
            }
        }

        std::vector<SExprLocRef> out;
        out.reserve(list.elem.size());
        for (const auto& el : list.elem) {
            auto sub_ctx = ctx;
            sub_ctx.is_top_level = false;
            auto r = expand(el, sub_ctx);
            if (!r.is_valid())
                return r;
            out.push_back(r);
        }
        return ctx.arena.emplace(root.loc_ref(), SExprList(std::move(out)));
    }

    return root;
}

static constexpr std::string_view STDLIB_SOURCE = R"STDLIB(

(define-syntax and
  (syntax-rules ()
    ((and) #t)
    ((and test) test)
    ((and test1 test2 ...)
     (if test1 (and test2 ...) #f))))

(define-syntax or
  (syntax-rules ()
    ((or) #f)
    ((or test) test)
    ((or test1 test2 ...)
     (let ((x test1))
       (if x x (or test2 ...))))))

(define-syntax when
  (syntax-rules ()
    ((when test body ...)
     (if test (begin body ...)))))

(define-syntax unless
  (syntax-rules ()
    ((unless test body ...)
     (if test (void) (begin body ...)))))

(define-syntax let
  (syntax-rules ()
    ((let ((name val) ...) body1 body2 ...)
     ((lambda (name ...) body1 body2 ...)
      val ...))))

(define-syntax let*
  (syntax-rules ()
    ((let* () body1 body2 ...)
     (let () body1 body2 ...))
    ((let* ((name1 val1) rest ...) body1 body2 ...)
     (let ((name1 val1))
       (let* (rest ...) body1 body2 ...)))))

(define-syntax cond
  (syntax-rules (else)
    ((cond (else result1 result2 ...))
     (begin result1 result2 ...))
    ((cond (test result1 result2 ...))
     (if test (begin result1 result2 ...)))
    ((cond (test result1 result2 ...) clause1 clause2 ...)
     (if test
         (begin result1 result2 ...)
         (cond clause1 clause2 ...)))))

; TODO missing begin letrec

(define-syntax quasiquote
  (syntax-rules 
    (unquote unquote-splicing)
    ((_ x) 
     (__qq-expand () x))))

(define-syntax __qq-expand
  (syntax-rules
    (unquote unquote-splicing quasiquote)
    ((_ () (unquote x))
     x)
    
    ((_ () (unquote-splicing x))
     (syntax-error "unquote-splicing at level 0"))

    ((_ () (quasiquote x))
     (list 'quasiquote (__qq-expand (x) x)))
    
    ((_ (x . d) (unquote e))
     (list 'unquote (__qq-expand d e)))
    
    ((_ (x . d) (quasiquote e))
     (list 'quasiquote (__qq-expand (x x . d) e)))

    ((_ d #(e ...))
     (list->vector (__qq-list d (e ...))))

    ((_ d (e . f))
     (__qq-list d (e . f)))

    ((_ d atom)
     'atom)))

(define-syntax __qq-list
  (syntax-rules (unquote unquote-splicing quasiquote)
    ((_ () ((unquote-splicing x) . rest))
     (append x (__qq-list () rest)))
    
    ((_ (x . d) ((unquote-splicing e) . rest))
     (cons (list 'unquote-splicing (__qq-expand d e))
           (__qq-list (x . d) rest)))
    
    ((_ () (unquote e))
     e)
    
    ((_ (x . d) (unquote e))
     (list 'unquote (__qq-expand d e)))
    
    ((_ d (head . tail))
     (cons (__qq-expand d head) (__qq-list d tail)))
    
    ((_ d atom)
     'atom)))

(define-syntax unquote
  (syntax-rules
    ()
    ((_ x)
     (syntax-error "unquote outside of quasiquote"))))

(define-syntax unquote-splicing
    (syntax-rules
      ()
      ((_ x)
       (syntax-error "unquote-splicing outside of quasiquote"))))

(define-syntax delay
  (syntax-rules ()
    ((delay expr)
     (__memo (lambda () expr)))))

(define (force promise)
  (promise))

(define (__memo proc)
  (let ((run_once? #f)
        (result #f))
    (lambda ()
      (if (not run_once?)
          (begin (set! result (proc))
                 (set! run_once? #t)
                 result)
          result))))

)STDLIB";

void ExpandPass::load_stdlib(SExprArena& /* user_arena */) {
    Lexer lexer("<stdlib>", STDLIB_SOURCE);
    if (lexer.is_failed())
        return;

    Parser parser(lexer.tokens(), lexer.loc_arena());
    if (parser.is_failed())
        return;

    auto stdlib_root = parser.root();
    _stdlib_arena = std::make_unique<SExprArena>(std::move(parser.arena()));

    const auto& root_expr = _stdlib_arena->at(stdlib_root);
    if (root_expr.holds_alternative<SExprList>()) {
        bool dummy_error = false;
        for (const auto& form : root_expr.get_unchecked<SExprList>().elem) {
            ExpCtx ctx {
                .env = _env,
                .arena = *_stdlib_arena,
                .stack = _exp_stack,
                .parent = ExpansionStack::INVALID,
                .is_stdlib = true,
                .show_stdlib = _show_stdlib_expansion,
                .is_top_level = true,
                .had_error = dummy_error,
            };
            (void)expand(form, ctx);
        }
    }
    _stdlib_loaded = true;
}

// FIXME missing let-syntax,letrec_syntax
[[nodiscard]] SExprLocRef ExpandPass::run(
    SExprLocRef root, SExprArena& arena) noexcept {
    if (!_stdlib_loaded)
        load_stdlib(arena);

    _had_error = false;
    ExpCtx ctx {
        .env = _env,
        .arena = arena,
        .stack = _exp_stack,
        .parent = ExpansionStack::INVALID,
        .is_stdlib = false,
        .show_stdlib = _show_stdlib_expansion,
        .is_top_level = true,
        .had_error = _had_error,
    };

    if (arena.at(root).holds_alternative<SExprList>()) {
        const auto& list = arena.at(root).get_unchecked<SExprList>();
        std::vector<SExprLocRef> out;
        out.reserve(list.elem.size());
        for (const auto& el : list.elem) {
            auto r = expand(el, ctx);
            if (!r.is_valid()) {
                _had_error = true;
                return SExprLocRef::invalid();
            }
            out.push_back(r);
        }
        auto expanded
            = arena.emplace(root.loc_ref(), SExprList(std::move(out)));
        if (_had_error)
            return SExprLocRef::invalid();
        return resolve_names(expanded, arena);
    }
    __builtin_unreachable();
}

ExpandPass::ExpandPass(bool show_stdlib_expansion) noexcept
    : _show_stdlib_expansion(show_stdlib_expansion) {
    _env.define_core_syntax("lambda");
    _env.define_core_syntax("quote");
    _env.define_core_syntax("if");
    _env.define_core_syntax("set!");
    // TODO: begin could be derived
    _env.define_core_syntax("begin");
    _env.define_core_syntax("define");
    _env.define_core_syntax("define-syntax");
    _env.define_core_syntax("syntax-error");
}

} // namespace lpc::frontend
