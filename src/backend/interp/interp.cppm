export module lpc.backend.interp;

import std;

import lpc.cps.ir;
import lpc.sema.core_form;
import lpc.syntax.arenas;
import lpc.syntax.ast;
import lpc.utils.tagged_union;

namespace lpc::backend {

using namespace lpc::cps;
using namespace lpc::sema;
using namespace lpc::syntax;
using namespace lpc::utils;

export struct Env;
export struct Cons;
export struct Box;
export struct Vector;

export struct Closure {
    CpsExprRef lambda_ref;
    Env* env = nullptr;

    [[nodiscard]] bool operator==(const Closure& other) const;
};

export struct Nil {
    [[nodiscard]] bool operator==(const Nil&) const;
};

export struct Undefined {
    [[nodiscard]] bool operator==(const Undefined&) const;
};

export class Value
    : public TaggedUnion<Undefined, Nil, std::int64_t, bool, char, Closure,
          std::shared_ptr<Cons>, std::shared_ptr<Box>, std::shared_ptr<Vector>,
          LispIdent, std::shared_ptr<std::string>> {
public:
    using TaggedUnion::TaggedUnion;
};

export struct Cons {
    Value car;
    Value cdr;
};

export struct Box {
    Value val;
};

export struct Vector {
    std::uint64_t tag;
    std::vector<Value> elements;
};

export enum class PrintMode : std::uint8_t { Display, Write };

export void print_value(std::ostream& os, const Value& value, PrintMode mode);

export std::ostream& operator<<(std::ostream& os, const Value& value);

export struct Env {
    explicit Env(std::size_t expected_bindings = 0) {
        values.reserve(expected_bindings);
    }

    std::vector<std::pair<std::uint32_t, Value>> values;
    Env* parent = nullptr;

    [[nodiscard]] Value* lookup(const VarId& id);
    void bind(const VarId& id, Value value);
};

export class Interp {
public:
    Interp(const CpsArena& cps_arena, const SpanArena& span_arena);

    [[nodiscard]] Value run(CpsExprRef root);

private:
    const CpsArena& _cps_arena;
    const SpanArena& _span_arena;
    mutable std::deque<Env> _envs;

    [[nodiscard]] Env* make_env(std::size_t expected_bindings, Env* parent) const {
        auto& env = _envs.emplace_back(expected_bindings);
        env.parent = parent;
        return &env;
    }

    [[nodiscard]] static std::int64_t as_int(const Value& value);
    [[nodiscard]] static char as_char(const Value& value);
    [[nodiscard]] static std::string& as_string(Value& value);
    [[nodiscard]] static Value& lookup_variable(
        const CpsVar& variable, Env* env);
    template <typename Args>
    [[nodiscard]] Env* bind_call(const Closure& closure, const Args& args) const;
    [[nodiscard]] Value eval_atom(const CpsAtom& atom, Env* env) const;
    [[nodiscard]] Value eval(CpsExprRef expr_ref, Env* env) const;
};

export using Interpreter = Interp;

namespace interp { } // namespace interp
} // namespace lpc::backend
