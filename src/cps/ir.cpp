module lpc.cps.ir;

import std;

import lpc.utils.error_handler;
import lpc.utils.tagged_union;

namespace lpc::cps {

using lpc::utils::overloaded;
using lpc::utils::Assert;

namespace {

    std::string join_atoms(
        const std::vector<CpsAtom>& atoms, const CpsDumpVisitor& visitor) {
        std::string out;
        for (std::size_t i = 0; i < atoms.size(); ++i) {
            if (i > 0)
                out += " ";
            out += visitor.atom_to_string(atoms[i]);
        }
        return out;
    }

    std::string join_var_names(std::span<const CpsVar> vars) {
        std::string out;
        for (std::size_t i = 0; i < vars.size(); ++i) {
            if (i > 0)
                out += " ";
            out += vars[i].var.debug_name;
        }
        return out;
    }

} // namespace

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
            auto value = span_arena.dump(c.value);
            if (value == "()")
                return std::string("'()");
            return value;
        },
        [](const CpsUnit&) { return std::string("(void)"); },
        [](const CpsLabel& l) {
            return std::format("<label:{}>", l.lambda_ref._index);
        },
    });
}

std::string CpsDumpVisitor::operator()(const CpsApp& app) const {
    std::string out = "(";
    out += atom_to_string(app.func);
    if (!app.args.empty()) {
        out += " ";
        out += join_atoms(app.args, *this);
    }
    out += ")";
    return out;
}

std::string CpsDumpVisitor::operator()(const CpsLet& l) const {
    auto next_indent = indent + "  ";

    std::string binding = std::format(
        "({} ({}", l.target.var.debug_name, primop_to_string(l.op));
    if (!l.args.empty()) {
        binding += " ";
        binding += join_atoms(l.args, *this);
    }
    binding += "))";

    return std::format("(let ({})\n{}{}\n{})", binding, next_indent,
        dump(l.body, next_indent), indent);
}

std::string CpsDumpVisitor::operator()(const CpsIf& i) const {
    auto next_indent = indent + "  ";
    return std::format("(if {}\n{}{}\n{}{}\n{})", atom_to_string(i.condition),
        next_indent, dump(i.then_branch, next_indent), next_indent,
        dump(i.else_branch, next_indent), indent);
}

std::string CpsDumpVisitor::operator()(const CpsLambda& l) const {
    auto next_indent = indent + "  ";

    if (l.is_variadic) {
        Assert(l.params.size() >= 2);
        const auto fixed_count = l.params.size() - 2;
        const auto& rest_name = l.params[fixed_count].var.debug_name;
        const auto& cont_name = l.params.back().var.debug_name;
        const auto tail_name = std::format("__cps_tail_{}", l.name.var.id);

        std::string out;
        if (fixed_count == 0) {
            out += std::format("(lambda {}", tail_name);
        } else {
            out += std::format("(lambda ({} . {})",
                join_var_names(std::span(l.params).first(fixed_count)),
                tail_name);
        }

        out += "\n";
        out += std::format("{}(let (({} (let loop ((xs {}))\n", next_indent,
            rest_name, tail_name);
        out += std::format("{}                  (if (null? xs)\n", next_indent);
        out += std::format("{}                      '()\n", next_indent);
        out += std::format(
            "{}                      (if (null? (cdr xs))\n", next_indent);
        out += std::format("{}                          '()\n", next_indent);
        out += std::format(
            "{}                          (cons (car xs) (loop (cdr xs))))))\n",
            next_indent);
        out += std::format("{}      ({} (let loop ((xs {}))\n", next_indent,
            cont_name, tail_name);
        out += std::format("{}           (if (null? xs)\n", next_indent);
        out += std::format("{}               (void)\n", next_indent);
        out += std::format(
            "{}               (if (null? (cdr xs))\n", next_indent);
        out += std::format("{}                   (car xs)\n", next_indent);
        out += std::format(
            "{}                   (loop (cdr xs))))))\n", next_indent);
        out += std::format(
            "{}{}\n", next_indent + "  ", dump(l.body, next_indent + "  "));
        out += std::format("{})\n", next_indent);
        out += std::format("{})", indent);
        return out;
    }

    return std::format("(lambda ({})\n{}{}\n{})", join_var_names(l.params),
        next_indent, dump(l.body, next_indent), indent);
}

std::string CpsDumpVisitor::operator()(const CpsFix& f) const {
    if (f.functions.empty())
        return dump(f.body, indent);

    auto next_indent = indent + "  ";
    std::string out = "(letrec (\n";
    for (std::size_t i = 0; i < f.functions.size(); ++i) {
        auto function_ref = f.functions[i];
        const auto* lambda = arena.get(function_ref).get<CpsLambda>();
        Assert(lambda != nullptr);
        const std::string& function_name = lambda->name.var.debug_name;

        out += std::format("{}({} {})", next_indent, function_name,
            dump(function_ref, next_indent + "  "));
        if (i + 1 < f.functions.size())
            out += "\n";
    }
    out += ")\n";
    out += std::format("{}{}\n", next_indent, dump(f.body, next_indent));
    out += std::format("{})", indent);
    return out;
}

std::string CpsDumpVisitor::operator()(const CpsHalt& h) const {
    return atom_to_string(h.value);
}

std::string CpsDumpVisitor::dump(
    CpsExprRef ref, std::string next_indent) const {
    CpsDumpVisitor visitor { .arena = arena,
        .span_arena = span_arena,
        .indent = std::move(next_indent) };
    return arena.get(ref).visit(visitor);
}

} // namespace lpc::cps
