module lpc.backend.interp;

import lpc.utils.error_handler;

namespace lpc::backend {

using lpc::utils::Assert;

namespace {
    constexpr std::size_t inline_argument_capacity = 4;
    constexpr std::size_t expected_local_bindings = 8;

    [[nodiscard]] bool is_truthy(const Value& value) {
        if (const auto* boolean_value = value.get<bool>())
            return *boolean_value;
        return true;
    }

    class EvaluatedArgs {
    public:
        explicit EvaluatedArgs(std::size_t expected_size)
            : _inline(expected_size <= inline_argument_capacity) {
            if (!_inline) {
                _overflow_values.reserve(expected_size);
                _overflow_refs.reserve(expected_size);
            }
        }

        void push_borrowed(const Value& value) {
            if (_inline) {
                _inline_refs[_size] = &value;
            } else {
                _overflow_refs.push_back(&value);
            }
            ++_size;
        }

        void push_owned(Value value) {
            if (_inline) {
                _inline_values[_size].emplace(std::move(value));
                _inline_refs[_size] = &*_inline_values[_size];
            } else {
                _overflow_values.push_back(std::move(value));
                _overflow_refs.push_back(&_overflow_values.back());
            }
            ++_size;
        }

        [[nodiscard]] bool empty() const noexcept {
            return _size == 0;
        }
        [[nodiscard]] std::size_t size() const noexcept {
            return _size;
        }

        [[nodiscard]] Value& operator[](std::size_t index) {
            return const_cast<Value&>(std::as_const(*this)[index]);
        }

        [[nodiscard]] const Value& operator[](std::size_t index) const {
            return *(_inline ? _inline_refs[index] : _overflow_refs[index]);
        }

    private:
        std::array<std::optional<Value>, inline_argument_capacity>
            _inline_values;
        std::array<const Value*, inline_argument_capacity> _inline_refs { };
        std::vector<Value> _overflow_values;
        std::vector<const Value*> _overflow_refs;
        std::size_t _size = 0;
        bool _inline;
    };

} // namespace

Interp::Interp(const CpsArena& cps_arena, const SpanArena& span_arena)
    : _cps_arena(cps_arena)
    , _span_arena(span_arena) {
}

Value Interp::run(CpsExprRef root) {
    auto root_env = std::make_shared<Env>(expected_local_bindings);
    return eval(root, std::move(root_env));
}

std::int64_t Interp::as_int(const Value& value) {
    if (const auto* int_value = value.get<std::int64_t>())
        return *int_value;

    std::stringstream message;
    message << "Expected integer, got: " << value;
    throw std::runtime_error(message.str());
}

Value& Interp::lookup_variable(
    const CpsVar& variable, const std::shared_ptr<Env>& env) {
    if (Value* bound = env->lookup(variable.var))
        return *bound;
    throw std::runtime_error("Variable not found: " + variable.var.debug_name);
}

Value Interp::eval_atom(
    const CpsAtom& atom, const std::shared_ptr<Env>& env) const {
    return atom.visit(overloaded { [&](const CpsVar& variable) -> Value {
                                      return lookup_variable(variable, env);
                                  },
        [&](const CpsConstant& constant) -> Value {
            const auto& sexpr = _span_arena.expr(constant.value);
            return sexpr.visit(overloaded {
                [](LispNumber number) -> Value {
                    return Value(static_cast<std::int64_t>(number));
                },
                [](bool boolean_value) -> Value {
                    return Value(boolean_value);
                },
                [](LispChar character) -> Value {
                    return Value(static_cast<char>(character));
                },
                [](const LispIdent& ident) -> Value { return Value(ident); },
                [](const LispString& text) -> Value { return Value(text); },
                [](const LispNil&) -> Value { return Value(Nil { }); },
                [&](const SExprList& list) -> Value {
                    Value tail = eval_atom(
                        CpsAtom(CpsConstant { list.elem.back() }), env);
                    for (std::size_t index = list.elem.size() - 1; index > 0;
                        --index) {
                        auto pair = std::make_shared<Cons>();
                        pair->car = eval_atom(
                            CpsAtom(CpsConstant { list.elem[index - 1] }), env);
                        pair->cdr = std::move(tail);
                        tail = Value(pair);
                    }

                    return tail;
                },
                [&](const SExprVector& vector_value) -> Value {
                    auto vector_ref = std::make_shared<Vector>();
                    vector_ref->tag = 0;
                    vector_ref->elements.reserve(vector_value.elem.size());
                    for (SpanRef element : vector_value.elem)
                        vector_ref->elements.push_back(
                            eval_atom(CpsAtom(CpsConstant { element }), env));
                    return Value(vector_ref);
                },
                [&](const auto& unsupported) -> Value {
                    std::stringstream message;
                    message << "Unsupported constant type: "
                            << typeid(unsupported).name();
                    throw std::runtime_error(message.str());
                } });
        },
        [](const CpsUnit&) -> Value { return Value(Undefined { }); } });
}

Value Interp::eval(
    CpsExprRef expr_ref, std::shared_ptr<Env> env) const {
    while (true) {
        const auto& expr = _cps_arena.get(expr_ref);
        bool jumped = false;

        Value result = expr.visit(overloaded {
            [&](const CpsApp& app) -> Value {
                std::optional<Value> owned_function_value;
                const Value* function_value;
                if (const auto* variable = app.func.get<CpsVar>()) {
                    function_value = &lookup_variable(*variable, env);
                } else {
                    owned_function_value.emplace(eval_atom(app.func, env));
                    function_value = &*owned_function_value;
                }

                const Closure* closure = function_value->get<Closure>();
                if (closure == nullptr) {
                    std::stringstream message;
                    message << "Application target is not a closure: "
                            << *function_value;
                    throw std::runtime_error(message.str());
                }

                const auto* lambda
                    = _cps_arena.get(closure->lambda_ref).get<CpsLambda>();
                Assert(lambda != nullptr);

                auto call_env = std::make_shared<Env>(
                    lambda->params.size() + expected_local_bindings);
                call_env->parent = closure->env;

                if (lambda->is_variadic) {
                    Assert(lambda->params.size() >= 2);

                    const std::size_t fixed_param_count
                        = lambda->params.size() - 2;
                    if (app.args.size() < fixed_param_count + 1) {
                        throw std::runtime_error(
                            "Arity mismatch in variadic application");
                    }

                    for (std::size_t index = 0; index < fixed_param_count;
                        ++index) {
                        call_env->bind(lambda->params[index].var,
                            eval_atom(app.args[index], env));
                    }

                    Value rest(Value(Nil { }));
                    for (std::size_t index = app.args.size() - 1;
                        index > fixed_param_count; --index) {
                        auto pair = std::make_shared<Cons>();
                        pair->car = eval_atom(app.args[index - 1], env);
                        pair->cdr = std::move(rest);
                        rest = Value(pair);
                    }

                    call_env->bind(
                        lambda->params[fixed_param_count].var, std::move(rest));
                    call_env->bind(lambda->params.back().var,
                        eval_atom(app.args.back(), env));
                } else {
                    if (lambda->params.size() != app.args.size()) {
                        std::stringstream message;
                        message << "Arity mismatch: expected "
                                << lambda->params.size() << ", got "
                                << app.args.size();
                        throw std::runtime_error(message.str());
                    }

                    for (std::size_t index = 0; index < lambda->params.size();
                        ++index) {
                        call_env->bind(lambda->params[index].var,
                            eval_atom(app.args[index], env));
                    }
                }

                expr_ref = lambda->body;
                env = std::move(call_env);
                jumped = true;
                return Value(Undefined { });
            },
            [&](const CpsLet& let_expr) -> Value {
                EvaluatedArgs args(let_expr.args.size());
                for (const CpsAtom& arg : let_expr.args) {
                    if (const auto* variable = arg.get<CpsVar>())
                        args.push_borrowed(lookup_variable(*variable, env));
                    else
                        args.push_owned(eval_atom(arg, env));
                }

                auto expect_arity = [&]([[maybe_unused]] std::size_t expected) {
                    Assert(args.size() == expected);
                };

                Value primop_result;
                switch (let_expr.op) {
                case PrimOp::FxAdd:
                    expect_arity(2);
                    primop_result = Value(as_int(args[0]) + as_int(args[1]));
                    break;
                case PrimOp::FxSub:
                    expect_arity(2);
                    primop_result = Value(as_int(args[0]) - as_int(args[1]));
                    break;
                case PrimOp::FxMul:
                    expect_arity(2);
                    primop_result = Value(as_int(args[0]) * as_int(args[1]));
                    break;
                case PrimOp::FxDiv:
                    expect_arity(2);
                    if (as_int(args[1]) == 0)
                        throw std::runtime_error("Division by zero");
                    primop_result = Value(as_int(args[0]) / as_int(args[1]));
                    break;
                case PrimOp::FxMod:
                    expect_arity(2);
                    if (as_int(args[1]) == 0)
                        throw std::runtime_error("Modulo by zero");
                    primop_result = Value(as_int(args[0]) % as_int(args[1]));
                    break;
                case PrimOp::FxLogAnd:
                    expect_arity(2);
                    primop_result = Value(as_int(args[0]) & as_int(args[1]));
                    break;
                case PrimOp::FxLogOr:
                    expect_arity(2);
                    primop_result = Value(as_int(args[0]) | as_int(args[1]));
                    break;
                case PrimOp::FxLogNot:
                    expect_arity(1);
                    primop_result = Value(~as_int(args[0]));
                    break;
                case PrimOp::FxLogXor:
                    expect_arity(2);
                    primop_result = Value(as_int(args[0]) ^ as_int(args[1]));
                    break;
                case PrimOp::FxShl:
                    expect_arity(2);
                    {
                        const auto lhs = as_int(args[0]);
                        const auto shift = as_int(args[1]);
                        if (shift < 0
                            || shift
                                >= std::numeric_limits<std::int64_t>::digits) {
                            throw std::runtime_error(
                                "Shift count out of range");
                        }
                        primop_result = Value(lhs << shift);
                    }
                    break;
                case PrimOp::FxShr:
                    expect_arity(2);
                    {
                        const auto lhs = as_int(args[0]);
                        const auto shift = as_int(args[1]);
                        if (shift < 0
                            || shift
                                >= std::numeric_limits<std::int64_t>::digits) {
                            throw std::runtime_error(
                                "Shift count out of range");
                        }
                        primop_result = Value(lhs >> shift);
                    }
                    break;
                case PrimOp::FxLt:
                    expect_arity(2);
                    primop_result = Value(as_int(args[0]) < as_int(args[1]));
                    break;
                case PrimOp::FxLe:
                    expect_arity(2);
                    primop_result = Value(as_int(args[0]) <= as_int(args[1]));
                    break;
                case PrimOp::FxEq:
                    expect_arity(2);
                    primop_result = Value(as_int(args[0]) == as_int(args[1]));
                    break;
                case PrimOp::IsPair:
                    expect_arity(1);
                    primop_result = Value(args[0].isa<std::shared_ptr<Cons>>());
                    break;
                case PrimOp::IsSymbol:
                    expect_arity(1);
                    primop_result = Value(args[0].isa<LispIdent>());
                    break;
                case PrimOp::IsVector:
                    expect_arity(1);
                    primop_result
                        = Value(args[0].isa<std::shared_ptr<Vector>>());
                    break;
                case PrimOp::IsNil:
                    expect_arity(1);
                    primop_result = Value(args[0].isa<Nil>());
                    break;
                case PrimOp::IsBoolean:
                    expect_arity(1);
                    primop_result = Value(args[0].isa<bool>());
                    break;
                case PrimOp::IsFixnum:
                    expect_arity(1);
                    primop_result = Value(args[0].isa<std::int64_t>());
                    break;
                case PrimOp::IsChar:
                    expect_arity(1);
                    primop_result = Value(args[0].isa<char>());
                    break;
                case PrimOp::IsString:
                    expect_arity(1);
                    primop_result = Value(args[0].isa<std::string>());
                    break;
                case PrimOp::IsProcedure:
                    expect_arity(1);
                    primop_result = Value(args[0].isa<Closure>());
                    break;
                case PrimOp::MakeVector: {
                    Assert(!args.empty() && args.size() <= 2);

                    const std::int64_t size = as_int(args[0]);
                    if (size < 0)
                        throw std::runtime_error(
                            "make-vector size must be non-negative");

                    Value fill
                        = args.size() > 1 ? args[1] : Value(Undefined { });
                    auto vector_ref = std::make_shared<Vector>();
                    vector_ref->tag = 0;
                    vector_ref->elements.resize(
                        static_cast<std::size_t>(size), fill);
                    primop_result = Value(vector_ref);
                    break;
                }
                case PrimOp::Alloc: {
                    Assert(args.size() >= 2);

                    const std::int64_t tag = as_int(args[0]);
                    const std::int64_t size = as_int(args[1]);
                    if (tag < 0)
                        throw std::runtime_error(
                            "alloc tag must be non-negative");
                    if (size < 0)
                        throw std::runtime_error(
                            "alloc size must be non-negative");

                    const auto field_count = args.size() - 2;
                    Assert(static_cast<std::size_t>(size) == field_count);

                    if (tag == 0) {
                        if (size != 2) {
                            throw std::runtime_error(
                                "pair allocation requires two fields");
                        }

                        auto pair = std::make_shared<Cons>();
                        pair->car = args[2];
                        pair->cdr = args[3];
                        primop_result = Value(pair);
                    } else {
                        auto vector_ref = std::make_shared<Vector>();
                        vector_ref->tag = static_cast<std::uint64_t>(tag);
                        vector_ref->elements.reserve(field_count);
                        for (std::size_t index = 2; index < args.size();
                            ++index)
                            vector_ref->elements.push_back(args[index]);

                        primop_result = Value(vector_ref);
                    }
                    break;
                }
                case PrimOp::Load: {
                    expect_arity(2);
                    const std::int64_t index = as_int(args[1]);

                    if (args[0].isa<std::shared_ptr<Cons>>()) {
                        if (index == 0)
                            primop_result
                                = (*args[0].get<std::shared_ptr<Cons>>())->car;
                        else if (index == 1)
                            primop_result
                                = (*args[0].get<std::shared_ptr<Cons>>())->cdr;
                        else
                            throw std::runtime_error(
                                "Cons index out of bounds");
                    } else if (args[0].isa<std::shared_ptr<Vector>>()) {
                        if (index < 0) {
                            throw std::runtime_error(
                                "Vector index out of bounds");
                        }

                        auto* vector_ref
                            = args[0].get<std::shared_ptr<Vector>>();
                        if (static_cast<std::size_t>(index)
                            >= (*vector_ref)->elements.size()) {
                            throw std::runtime_error(
                                "Vector index out of bounds");
                        }

                        primop_result = (*vector_ref)->elements[index];
                    } else {
                        throw std::runtime_error(
                            "Load expects a pair or vector");
                    }
                    break;
                }
                case PrimOp::Store: {
                    expect_arity(3);
                    const std::int64_t index = as_int(args[1]);

                    if (args[0].isa<std::shared_ptr<Cons>>()) {
                        auto* pair = args[0].get<std::shared_ptr<Cons>>();
                        if (index == 0)
                            (*pair)->car = args[2];
                        else if (index == 1)
                            (*pair)->cdr = args[2];
                        else
                            throw std::runtime_error(
                                "Cons index out of bounds");
                    } else if (args[0].isa<std::shared_ptr<Vector>>()) {
                        if (index < 0) {
                            throw std::runtime_error(
                                "Vector index out of bounds");
                        }

                        auto* vector_ref
                            = args[0].get<std::shared_ptr<Vector>>();
                        if (static_cast<std::size_t>(index)
                            >= (*vector_ref)->elements.size()) {
                            throw std::runtime_error(
                                "Vector index out of bounds");
                        }

                        (*vector_ref)->elements[index] = args[2];
                    } else {
                        throw std::runtime_error(
                            "Store expects a pair or vector");
                    }

                    primop_result = Value(Undefined { });
                    break;
                }
                case PrimOp::Box:
                    expect_arity(1);
                    primop_result
                        = Value(std::make_shared<Box>(Box { .val = args[0] }));
                    break;
                case PrimOp::BoxGet:
                    expect_arity(1);
                    if (const auto* box_ref
                        = args[0].get<std::shared_ptr<Box>>())
                        primop_result = (*box_ref)->val;
                    else
                        throw std::runtime_error("box-get expects a box");
                    break;
                case PrimOp::BoxSet:
                    expect_arity(2);
                    if (const auto* box_ref
                        = args[0].get<std::shared_ptr<Box>>())
                        (*box_ref)->val = args[1];
                    else
                        throw std::runtime_error("box-set expects a box");
                    primop_result = Value(Undefined { });
                    break;
                case PrimOp::Length:
                    expect_arity(1);
                    if (args[0].isa<std::shared_ptr<Vector>>()) {
                        primop_result = Value(static_cast<std::int64_t>(
                            (*args[0].get<std::shared_ptr<Vector>>())
                                ->elements.size()));
                    } else if (args[0].isa<std::string>()) {
                        primop_result = Value(static_cast<std::int64_t>(
                            args[0].get<std::string>()->size()));
                    } else {
                        throw std::runtime_error(
                            "length expects a vector or string");
                    }
                    break;
                case PrimOp::Print:
                    expect_arity(1);
                    std::cout << args[0] << "\n";
                    primop_result = Value(Undefined { });
                    break;
                case PrimOp::Exit:
                    expect_arity(1);
                    std::quick_exit(static_cast<int>(as_int(args[0])));
                case PrimOp::Exception: {
                    expect_arity(1);
                    std::stringstream message;
                    message << args[0];
                    throw std::runtime_error("Exception: " + message.str());
                }
                case PrimOp::Eq:
                    expect_arity(2);
                    primop_result = Value(args[0] == args[1]);
                    break;
                default:
                    throw std::runtime_error("Unknown primop: "
                        + std::to_string(static_cast<int>(let_expr.op)));
                }

                env->bind(let_expr.target.var, std::move(primop_result));
                expr_ref = let_expr.body;
                jumped = true;
                return Value(Undefined { });
            },
            [&](const CpsIf& if_expr) -> Value {
                std::optional<Value> owned_condition;
                const Value* condition;
                if (const auto* variable = if_expr.condition.get<CpsVar>())
                    condition = &lookup_variable(*variable, env);
                else {
                    owned_condition.emplace(eval_atom(if_expr.condition, env));
                    condition = &*owned_condition;
                }

                expr_ref = is_truthy(*condition) ? if_expr.then_branch
                                                 : if_expr.else_branch;
                jumped = true;
                return Value(Undefined { });
            },
            [&](const CpsFix& fix) -> Value {
                auto fix_env = std::make_shared<Env>(
                    fix.functions.size() + expected_local_bindings);
                fix_env->parent = env;

                for (CpsExprRef function_ref : fix.functions) {
                    const auto& function_expr = _cps_arena.get(function_ref);
                    const auto* lambda = function_expr.get<CpsLambda>();
                    Assert(lambda != nullptr);

                    fix_env->bind(lambda->name.var,
                        Value(Closure {
                            .lambda_ref = function_ref, .env = fix_env }));
                }

                expr_ref = fix.body;
                env = std::move(fix_env);
                jumped = true;
                return Value(Undefined { });
            },
            [&](const CpsHalt& halt) -> Value {
                return eval_atom(halt.value, env);
            },
            [&](const CpsLambda&) -> Value {
                return Value(Closure { .lambda_ref = expr_ref, .env = env });
            } });

        if (!jumped)
            return result;
    }
}
}
