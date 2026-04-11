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

export struct Environment;
export struct Cons;
export struct Box;
export struct Vector;

export struct Closure {
    CpsLambda lambda;
    std::shared_ptr<Environment> env;

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
          LispIdent, std::string> {
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

export std::ostream& operator<<(std::ostream& os, const Value& value);

export struct Environment {
    std::unordered_map<VarId, Value> values;
    std::shared_ptr<Environment> parent;

    [[nodiscard]] Value* lookup(const VarId& id);
    void bind(VarId id, Value value);
};

export class Interp {
public:
    Interp(const CpsArena& cps_arena, const SpanArena& span_arena);

    [[nodiscard]] Value run(CpsExprRef root);

private:
    const CpsArena& _cps_arena;
    const SpanArena& _span_arena;

    [[nodiscard]] std::int64_t as_int(const Value& value) const;
    [[nodiscard]] Value eval_atom(
        const CpsAtom& atom, const std::shared_ptr<Environment>& env) const;
    [[nodiscard]] Value eval(
        CpsExprRef expr_ref, std::shared_ptr<Environment> env) const;
};

export using Interpreter = Interp;

namespace interp { } // namespace interp
} // namespace lpc::backend
