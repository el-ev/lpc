
import std;
import lpc.backend.interp;
import lpc.cps.ir;
import lpc.syntax.arenas;
import lpc.sema.core_form;
import lpc.syntax.ast;
import lpc.syntax.span;

using namespace lpc::cps;
using namespace lpc::sema;
using namespace lpc::syntax;
using namespace lpc::backend;

// Helper to create vars
CpsVar make_var(std::uint32_t id, std::string name) {
    return CpsVar { VarId { id, name } };
}

int main() {
    SExprArena expr_arena;
    LocationArena loc_arena;
    SpanArena span_arena(std::move(expr_arena), std::move(loc_arena));
    CpsArena cps_arena;
    Interpreter interpreter(cps_arena, span_arena);

    // Test 1: Identity function
    // ((lambda (x k) (k x)) 42 Halt)
    // Actually Halt is special form, but usually we pass a continuation.
    // CPS converter uses Halt in the top-level continuation.
    // Here we can just run a CpsApp that applies a lambda.
    // Or simpler: CpsHalt(42).

    std::cout << "Test 1: Halt(42)" << std::endl;
    {
        // 42 constant
        auto val_42 = span_arena.from_loc(LocRef::invalid(),
            span_arena.expr_arena().emplace(
                SExpr(static_cast<LispNumber>(42))));

        // Halt(42)
        auto halt_expr
            = cps_arena.emplace(CpsHalt { CpsAtom(CpsConstant { val_42 }) });

        Value result = interpreter.run(halt_expr);
        std::cout << "Result: " << result << std::endl;
        if (!result.isa<std::int64_t>() || *result.get<std::int64_t>() != 42) {
            std::cerr << "FAILED Test 1" << std::endl;
            return 1;
        }
    }

    std::cout << "Test 2: ((lambda (x k) (k x)) 42 K_Halt)" << std::endl;
    {
        // Setup vars
        auto x = make_var(1, "x");
        auto k = make_var(2, "k");
        auto k_halt = make_var(3, "k_halt");

        // Body of lambda: (k x)
        // We need to support calling k. k is a variable.
        // App(k, {x})
        auto lambda_body
            = cps_arena.emplace(CpsApp { CpsAtom(k), { CpsAtom(x) } });

        // Lambda(x, k) -> (k x)
        // Name of lambda itself (for recursion) - not needed here?
        // CpsLambda has 'name', 'params', 'body'.
        auto lambda_name = make_var(4, "f");
        auto lambda = cps_arena.emplace(
            CpsLambda { lambda_name, { x, k }, lambda_body });

        // Constant 42
        auto val_42_span = span_arena.from_loc(LocRef::invalid(),
            span_arena.expr_arena().emplace(
                SExpr(static_cast<LispNumber>(42))));

        // K_Halt: lambda (r) (halt r)
        auto r = make_var(5, "r");
        auto halt_r = cps_arena.emplace(CpsHalt { CpsAtom(r) });
        auto closure_halt_name = make_var(6, "halt_closure");
        auto closure_halt
            = cps_arena.emplace(CpsLambda { closure_halt_name, { r }, halt_r });

        // Result of constructing the lambda and closure.
        // We need to define them?
        // In CPS, we usually use Fix to define functions, then call them.
        // (fix ((f (lambda (x k) ...)) (cont (lambda (r) ...))) (f 42 cont))

        // App(f, {42, cont})
        auto main_app = cps_arena.emplace(CpsApp { CpsAtom(lambda_name),
            { CpsAtom(CpsConstant { val_42_span }),
                CpsAtom(closure_halt_name) } });

        // Fix defines f and cont
        auto fix
            = cps_arena.emplace(CpsFix { { lambda, closure_halt }, main_app });

        Value result = interpreter.run(fix);
        std::cout << "Result: " << result << std::endl;
        if (!result.isa<std::int64_t>() || *result.get<std::int64_t>() != 42) {
            std::cerr << "FAILED Test 2" << std::endl;
            return 1;
        }
    }

    std::cout << "Test 3: PrimOp Add" << std::endl;
    {
        // (let ((r (+ 1 2))) (halt r))

        auto r = make_var(10, "r");

        auto val_1 = span_arena.from_loc(LocRef::invalid(),
            span_arena.expr_arena().emplace(SExpr(static_cast<LispNumber>(1))));
        auto val_2 = span_arena.from_loc(LocRef::invalid(),
            span_arena.expr_arena().emplace(SExpr(static_cast<LispNumber>(2))));

        auto let = cps_arena.emplace(CpsLet { .target = r,
            .op = PrimOp::FxAdd,
            .args = { CpsAtom(CpsConstant { val_1 }),
                CpsAtom(CpsConstant { val_2 }) },
            .body = cps_arena.emplace(CpsHalt { CpsAtom(r) }) });

        Value result = interpreter.run(let);
        std::cout << "Result: " << result << std::endl;
        if (!result.isa<std::int64_t>() || *result.get<std::int64_t>() != 3) {
            std::cerr << "FAILED Test 3" << std::endl;
            return 1;
        }
    }

    std::cout << "All tests passed!" << std::endl;
    return 0;
}
