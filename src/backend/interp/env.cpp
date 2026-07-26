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
    return lambda_ref == other.lambda_ref && env == other.env;
}

bool Nil::operator==(const Nil&) const {
    return true;
}

bool Undefined::operator==(const Undefined&) const {
    return true;
}

namespace {

    void print_char(std::ostream& os, char character, PrintMode mode) {
        if (mode == PrintMode::Display) {
            os << character;
            return;
        }
        switch (character) {
        case ' ':
            os << "#\\space";
            return;
        case '\n':
            os << "#\\newline";
            return;
        default:
            os << "#\\" << character;
            return;
        }
    }

    void print_string(
        std::ostream& os, const std::string& text, PrintMode mode) {
        if (mode == PrintMode::Display) {
            os << text;
            return;
        }
        os << '"';
        for (char character : text) {
            switch (character) {
            case '"':
                os << "\\\"";
                break;
            case '\\':
                os << "\\\\";
                break;
            case '\n':
                os << "\\n";
                break;
            case '\t':
                os << "\\t";
                break;
            default:
                os << character;
                break;
            }
        }
        os << '"';
    }

    struct ValuePrinter {
        std::ostream& os;
        PrintMode mode;

        void operator()(const Nil&) const {
            os << "()";
        }

        void operator()(const Undefined&) const {
            os << "#<void>";
        }

        void operator()(std::int64_t number) const {
            os << number;
        }

        void operator()(bool flag) const {
            os << (flag ? "#t" : "#f");
        }

        void operator()(char character) const {
            print_char(os, character, mode);
        }

        void operator()(const Closure&) const {
            os << "#<closure>";
        }

        void operator()(const std::shared_ptr<Cons>& pair) const {
            os << "(";

            bool first = true;
            Value current(pair);
            while (const auto* cons = current.get<std::shared_ptr<Cons>>()) {
                if (!first)
                    os << " ";

                print_value(os, (*cons)->car, mode);
                current = (*cons)->cdr;
                first = false;
            }

            if (!current.isa<Nil>()) {
                os << " . ";
                print_value(os, current, mode);
            }
            os << ")";
        }

        void operator()(const std::shared_ptr<Box>& box) const {
            os << "#<box ";
            print_value(os, box->val, mode);
            os << ">";
        }

        void operator()(const std::shared_ptr<Vector>& vector) const {
            os << "#(";
            for (std::size_t index = 0; index < vector->elements.size();
                ++index) {
                if (index > 0)
                    os << " ";
                print_value(os, vector->elements[index], mode);
            }
            os << ")";
        }

        void operator()(const LispIdent& ident) const {
            os << ident.name;
        }

        void operator()(const std::shared_ptr<std::string>& text) const {
            print_string(os, *text, mode);
        }
    };

} // namespace

void print_value(std::ostream& os, const Value& value, PrintMode mode) {
    value.visit(ValuePrinter { os, mode });
}

std::ostream& operator<<(std::ostream& os, const Value& value) {
    print_value(os, value, PrintMode::Write);
    return os;
}

Value* Env::lookup(const VarId& id) {
    for (Env* env = this; env != nullptr; env = env->parent)
        for (auto& [bound_id, value] : env->values)
            if (bound_id == id.id)
                return &value;
    return nullptr;
}

void Env::bind(const VarId& id, Value value) {
    for (auto& [bound_id, bound_value] : values) {
        if (bound_id == id.id) {
            bound_value = std::move(value);
            return;
        }
    }
    values.emplace_back(id.id, std::move(value));
}

} // namespace lpc::backend
