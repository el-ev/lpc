module lpc.backend.interp;

import std;

import lpc.cps.ir;
import lpc.sema.core_form;
import lpc.syntax.arenas;
import lpc.syntax.ast;
import lpc.utils.tagged_union;

namespace lpc::backend {

using namespace lpc::cps;
using namespace lpc::utils;
using namespace lpc::syntax;
using namespace lpc::sema;



bool Closure::operator==(const Closure& other) const {
    return lambda.name == other.lambda.name && env.get() == other.env.get();
}

bool Nil::operator==(const Nil&) const {
    return true;
}

bool Undefined::operator==(const Undefined&) const {
    return true;
}

std::ostream& operator<<(std::ostream& os, const Value& value) {
    if (value.isa<Nil>())
        return os << "()";
    if (value.isa<Undefined>())
        return os << "#<void>";
    if (value.isa<std::int64_t>())
        return os << *value.get<std::int64_t>();
    if (value.isa<bool>())
        return os << (*value.get<bool>() ? "#t" : "#f");
    if (value.isa<char>())
        return os << "#\\" << *value.get<char>();
    if (value.isa<Closure>())
        return os << "#<closure>";
    if (value.isa<std::shared_ptr<Cons>>()) {
        os << "(";

        bool first = true;
        Value current = value;
        while (current.isa<std::shared_ptr<Cons>>()) {
            if (!first)
                os << " ";

            const auto* pair = current.get<std::shared_ptr<Cons>>();
            os << (*pair)->car;
            current = (*pair)->cdr;
            first = false;
        }

        if (!current.isa<Nil>())
            os << " . " << current;
        return os << ")";
    }
    if (value.isa<std::shared_ptr<Box>>())
        return os << "#<box " << (*value.get<std::shared_ptr<Box>>())->val
                  << ">";
    if (value.isa<std::shared_ptr<Vector>>()) {
        const auto* vector_ref = value.get<std::shared_ptr<Vector>>();
        os << "#(";
        for (std::size_t index = 0; index < (*vector_ref)->elements.size();
            ++index) {
            if (index > 0)
                os << " ";
            os << (*vector_ref)->elements[index];
        }
        return os << ")";
    }
    if (value.isa<LispIdent>())
        return os << value.get<LispIdent>()->name;
    if (value.isa<std::string>())
        return os << '"' << *value.get<std::string>() << '"';
    if (value.valueless())
        return os << "#<invalid>";
    return os << "#<unknown>";
}

Value* Environment::lookup(const VarId& id) {
    if (auto it = values.find(id); it != values.end())
        return &it->second;
    if (parent)
        return parent->lookup(id);
    return nullptr;
}

void Environment::bind(const VarId& id, Value value) {
    values[id] = std::move(value);
}

} // namespace lpc::backend
