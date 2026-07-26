import std;
import lpc.backend.interp;
import lpc.cps.ir;
import lpc.sema.core_form;
import lpc.syntax.arenas;
import lpc.syntax.ast;
import lpc.test.framework;

#include "utils/macros.hpp"

using namespace lpc::backend;
using namespace lpc::cps;
using namespace lpc::sema;
using namespace lpc::syntax;

namespace {

CpsVar make_var(std::uint32_t id, std::string name) {
    return CpsVar { VarId { id, std::move(name) } };
}

SpanRef make_integer(SpanArena& arena, std::int64_t value) {
    return arena.from_loc(LocRef::invalid(),
        arena.expr_arena().emplace(SExpr(LispNumber(value))));
}

struct VariadicProgram {
    CpsArena cps_arena;
    SpanArena span_arena;

    VariadicProgram()
        : span_arena(SExprArena { }, LocationArena { }) {
    }

    Value run(std::vector<std::int64_t> arguments,
        bool with_continuation = true, bool with_fixed_parameter = true) {
        const auto fixed = make_var(1, "fixed");
        const auto rest = make_var(2, "rest");
        const auto continuation = make_var(3, "continuation");
        const auto function = make_var(4, "function");
        const auto halt_continuation = make_var(5, "halt_continuation");
        const auto result = make_var(6, "result");

        const auto halt_body = cps_arena.emplace(CpsHalt { CpsAtom(result) });
        const auto halt_lambda = cps_arena.emplace(CpsLambda {
            .name = halt_continuation,
            .params = { result },
            .body = halt_body,
        });

        const auto function_body = cps_arena.emplace(
            CpsApp { CpsAtom(continuation), { CpsAtom(rest) } });

        std::vector<CpsVar> parameters;
        if (with_fixed_parameter)
            parameters.push_back(fixed);
        parameters.push_back(rest);
        parameters.push_back(continuation);

        const auto function_lambda = cps_arena.emplace(CpsLambda {
            .name = function,
            .params = std::move(parameters),
            .body = function_body,
            .is_variadic = true,
        });

        std::vector<CpsAtom> call_arguments;
        call_arguments.reserve(arguments.size() + (with_continuation ? 1 : 0));
        for (const auto argument : arguments) {
            call_arguments.emplace_back(
                CpsConstant { make_integer(span_arena, argument) });
        }
        if (with_continuation)
            call_arguments.emplace_back(halt_continuation);

        const auto call = cps_arena.emplace(CpsApp {
            .func = CpsAtom(function),
            .args = std::move(call_arguments),
        });
        const auto program = cps_arena.emplace(CpsFix {
            .functions = { function_lambda, halt_lambda }, .body = call });

        Interpreter interpreter(cps_arena, span_arena);
        return interpreter.run(program);
    }
};

void assert_integer_list(
    const Value& value, std::span<const std::int64_t> expected) {
    const Value* current = &value;
    for (const auto expected_value : expected) {
        const auto* pair = current->get<std::shared_ptr<Cons>>();
        ASSERT_TRUE(pair != nullptr);
        if (pair == nullptr)
            return;

        const auto* integer = (*pair)->car.get<std::int64_t>();
        ASSERT_TRUE(integer != nullptr);
        if (integer == nullptr)
            return;
        ASSERT_EQ(*integer, expected_value);
        current = &(*pair)->cdr;
    }
    ASSERT_TRUE(current->isa<Nil>());
}

} // namespace

TEST(variadic_lambda_collects_extra_arguments) {
    VariadicProgram program;
    const Value result = program.run({ 1, 2, 3 });
    const std::array<std::int64_t, 2> expected { 2, 3 };
    assert_integer_list(result, expected);
}

TEST(variadic_lambda_accepts_an_empty_rest_list) {
    VariadicProgram program;
    const Value result = program.run({ 1 });
    assert_integer_list(result, { });
}

TEST(variadic_lambda_without_fixed_parameters_collects_all_arguments) {
    VariadicProgram program;
    const Value result = program.run({ 2, 3 }, true, false);
    const std::array<std::int64_t, 2> expected { 2, 3 };
    assert_integer_list(result, expected);
}

TEST(variadic_lambda_requires_a_continuation_argument) {
    VariadicProgram program;
    ASSERT_THROW(program.run({ 1 }, false), std::runtime_error);
}

auto main() -> int {
    return lpc::test::get_runner().run_summary();
}
