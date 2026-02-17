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
    std::string args_str;
    for (std::size_t i = 0; i < app.args.size(); ++i) {
        if (i > 0)
            args_str += ", ";
        args_str += atom_to_string(app.args[i]);
    }
    return std::format("{}({})", atom_to_string(app.func), args_str);
}

std::string CpsDumpVisitor::operator()(const CpsLet& l) const {
    std::string out = dump(l.body, indent);
    if (!out.empty() && out.back() != '\n')
        out += '\n';
    
    std::string args_str;
    for (std::size_t i = 0; i < l.args.size(); ++i) {
        if (i > 0)
            args_str += ", ";
        args_str += atom_to_string(l.args[i]);
    }

    return std::format("{}{}where {} = {}({})", out, indent, l.target.var.debug_name, primop_to_string(l.op), args_str);
}

std::string CpsDumpVisitor::operator()(const CpsIf& i) const {
    return std::format("if {} then\n{}  {}\n{}else\n{}  {}\n", 
        atom_to_string(i.condition),
        indent, dump(i.then_branch, indent + "  "),
        indent,
        indent, dump(i.else_branch, indent + "  "));
}

std::string CpsDumpVisitor::operator()(const CpsLambda& l) const {
    std::string params_str;
    for (std::size_t i = 0; i < l.params.size(); ++i) {
        if (i > 0)
            params_str += ", ";
        params_str += l.params[i].var.debug_name;
    }
    return std::format("lambda {}({}) =\n{}  {}", 
        l.name.var.debug_name, params_str, 
        indent, dump(l.body, indent + "  "));
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
