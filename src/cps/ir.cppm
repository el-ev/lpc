export module lpc.cps.ir;

import std;

import lpc.sema.core_form;
import lpc.syntax.span;
import lpc.syntax.arenas;
import lpc.syntax.refs;
import lpc.utils.arena;
import lpc.utils.tagged_union;

namespace lpc::cps {

using namespace lpc::sema;
using namespace lpc::syntax;

using lpc::utils::TaggedUnion;

export struct CpsExprTag { };
export using CpsExprRef
    = lpc::utils::ElementReference<CpsExprTag, std::uint32_t>;

export using CpsUnit = std::monostate;

export struct CpsVar {
    VarId var;

    [[nodiscard]] auto operator<=>(const CpsVar& other) const noexcept {
        return var <=> other.var;
    }

    [[nodiscard]] auto operator==(const CpsVar& other) const noexcept {
        return var == other.var;
    }
};

export struct CpsConstant {
    SpanRef value;

    [[nodiscard]] auto operator==(const CpsConstant& other) const noexcept {
        return value == other.value;
    }
};

export class CpsAtom : public TaggedUnion<CpsVar, CpsConstant, CpsUnit> {
public:
    CpsAtom() = default;

    template <typename T>
        requires(!std::is_same_v<std::decay_t<T>, CpsAtom>)
    explicit CpsAtom(T&& value) noexcept
        : TaggedUnion(std::forward<T>(value)) {
    }
};

export struct CpsApp {
    CpsAtom func;
    std::vector<CpsAtom> args;
};

export enum class PrimOp : std::uint8_t {
#define X(name, str) name,
#include "primops.def"
#undef X
};

export struct CpsLet {
    CpsVar target;
    PrimOp op;
    std::vector<CpsAtom> args;
    CpsExprRef body;
};

export struct CpsIf {
    CpsAtom condition;
    CpsExprRef then_branch;
    CpsExprRef else_branch;
};

export struct CpsLambda {
    CpsVar name;
    std::vector<CpsVar> params;
    CpsExprRef body;
    bool is_variadic = false;
};

export struct CpsFix {
    std::vector<CpsExprRef> functions;
    CpsExprRef body;
};

export struct CpsHalt {
    CpsAtom value;
};

export class CpsExpr
    : public TaggedUnion<CpsApp, CpsLet, CpsIf, CpsFix, CpsHalt, CpsLambda> {
public:
    using TaggedUnion::TaggedUnion;
};

export class CpsArena
    : public lpc::utils::Arena<CpsExprTag, CpsExpr, std::uint32_t> {
public:
    explicit CpsArena() noexcept = default;

    template <typename... Args>
    [[nodiscard]] CpsExprRef emplace(Args&&... args) {
        return Arena::emplace(std::forward<Args>(args)...);
    }

    [[nodiscard]] const CpsExpr& get(CpsExprRef ref) const {
        return at(ref);
    }

    [[nodiscard]] CpsExpr& get(CpsExprRef ref) {
        return at(ref);
    }
};

template <typename... Ts>
struct overloaded : Ts... {
    using Ts::operator()...;
};

std::string primop_to_string(PrimOp op) {
    switch (op) {
#define X(op, str)                                                             \
    case PrimOp::op:                                                           \
        return str;
#include "primops.def"
#undef X
    }
    return "unknown";
}

export struct CpsDumpVisitor {
    const CpsArena& arena;
    const syntax::SpanArena& span_arena;
    std::string indent;

    [[nodiscard]] std::string atom_to_string(const CpsAtom& atom) const {
        return atom.visit(overloaded {
            [](const CpsVar& v) { return v.var.debug_name; },
            [&](const CpsConstant& c) {
                return std::format("{}", span_arena.dump(c.value));
            },
            [](const CpsUnit&) { return std::string("()"); },
        });
    }

    std::string operator()(const CpsApp& app) const {
        std::string out = atom_to_string(app.func) + "(";
        for (std::size_t i = 0; i < app.args.size(); ++i) {
            if (i > 0)
                out += ", ";
            out += atom_to_string(app.args[i]);
        }
        out += ")";
        return out;
    }

    std::string operator()(const CpsLet& l) const {
        std::string out = dump(l.body, indent);
        if (!out.empty() && out.back() != '\n')
            out += '\n';
        out += indent + "where " + l.target.var.debug_name + " = ";
        out += std::format("{}(", primop_to_string(l.op));
        for (std::size_t i = 0; i < l.args.size(); ++i) {
            if (i > 0)
                out += ", ";
            out += atom_to_string(l.args[i]);
        }
        out += ")";
        return out;
    }

    std::string operator()(const CpsIf& i) const {
        std::string out = "if " + atom_to_string(i.condition) + " then\n";
        out += indent + "  " + dump(i.then_branch, indent + "  ") + "\n";
        out += indent + "else\n";
        out += indent + "  " + dump(i.else_branch, indent + "  ");
        return out;
    }

    std::string operator()(const CpsLambda& l) const {
        std::string out = "lambda " + l.name.var.debug_name + "(";
        for (std::size_t i = 0; i < l.params.size(); ++i) {
            if (i > 0)
                out += ", ";
            out += l.params[i].var.debug_name;
        }
        out += ") =\n" + indent + "  " + dump(l.body, indent + "  ");
        return out;
    }

    std::string operator()(const CpsFix& f) const {
        std::string out = dump(f.body, indent);
        if (f.functions.empty())
            return out;

        if (!out.empty() && out.back() != '\n')
            out += '\n';
        out += indent + "where fix\n";
        for (const auto& func : f.functions)
            out += indent + "  " + dump(func, indent + "  ") + "\n";
        if (!out.empty() && out.back() == '\n')
            out.pop_back();
        return out;
    }

    std::string operator()(const CpsHalt& h) const {
        return "halt " + atom_to_string(h.value);
    }

    [[nodiscard]] std::string dump(
        CpsExprRef ref, std::string next_indent) const {
        CpsDumpVisitor visitor { .arena = arena,
            .span_arena = span_arena,
            .indent = std::move(next_indent) };
        return arena.get(ref).visit(visitor);
    }
};

} // namespace lpc::cps

export template <>
struct std::hash<lpc::cps::CpsVar> {
    std::size_t operator()(const lpc::cps::CpsVar& v) const noexcept {
        return std::hash<lpc::sema::VarId> {}(v.var);
    }
};
