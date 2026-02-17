module lpc.cps.ir;

import std;

import lpc.utils.tagged_union;

namespace lpc::cps {

using lpc::utils::overloaded;

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

std::string CpsDumpVisitor::atom_to_string(const CpsAtom& atom) const {
    return atom.visit(overloaded {
        [](const CpsVar& v) { return v.var.debug_name; },
        [&](const CpsConstant& c) {
            return std::format("{}", span_arena.dump(c.value));
        },
        [](const CpsUnit&) { return std::string("()"); },
    });
}

std::string CpsDumpVisitor::operator()(const CpsApp& app) const {
    std::string out = atom_to_string(app.func) + "(";
    for (std::size_t i = 0; i < app.args.size(); ++i) {
        if (i > 0)
            out += ", ";
        out += atom_to_string(app.args[i]);
    }
    out += ")";
    return out;
}

std::string CpsDumpVisitor::operator()(const CpsLet& l) const {
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

std::string CpsDumpVisitor::operator()(const CpsIf& i) const {
    std::string out = "if " + atom_to_string(i.condition) + " then\n";
    out += indent + "  " + dump(i.then_branch, indent + "  ") + "\n";
    out += indent + "else\n";
    out += indent + "  " + dump(i.else_branch, indent + "  ") + "\n";
    return out;
}

std::string CpsDumpVisitor::operator()(const CpsLambda& l) const {
    std::string out = "lambda " + l.name.var.debug_name + "(";
    for (std::size_t i = 0; i < l.params.size(); ++i) {
        if (i > 0)
            out += ", ";
        out += l.params[i].var.debug_name;
    }
    out += ") =\n" + indent + "  " + dump(l.body, indent + "  ");
    return out;
}

std::string CpsDumpVisitor::operator()(const CpsFix& f) const {
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

std::string CpsDumpVisitor::operator()(const CpsHalt& h) const {
    return "halt " + atom_to_string(h.value);
}

std::string CpsDumpVisitor::dump(
    CpsExprRef ref, std::string next_indent) const {
    CpsDumpVisitor visitor { .arena = arena,
        .span_arena = span_arena,
        .indent = std::move(next_indent) };
    return arena.get(ref).visit(visitor);
}

} // namespace lpc::cps
