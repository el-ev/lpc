module lpc.session;

import std;

import lpc.sema.expand;
import lpc.sema.sema;
import lpc.sema.mutability;
import lpc.cps.lower;
import lpc.context;
import lpc.passes;
import lpc.syntax.arenas;
import lpc.syntax.ast;
import lpc.syntax.lexer;
import lpc.syntax.refs;
import lpc.syntax.syntax;
import lpc.utils.logging;
import lpc.cps.simplify;

namespace lpc {

using namespace lpc::syntax;
using namespace lpc::sema;
using namespace lpc::cps;

using lpc::utils::Error;

int Session::run() noexcept {
    if (_input_file_paths.empty()) {
        Error("No input file. Require exactly one input file.");
        return 1;
    }
    if (_input_file_paths.size() > 1) {
        Error("Require exactly one input file.");
        return 1;
    }
    std::string_view path = _input_file_paths[0];

    std::ifstream input_file { std::string(path) };
    if (!input_file.is_open()) {
        Error("Failed to open input file: {}", path);
        return 1;
    }

    input_file.seekg(0, std::ios::end);
    std::string source;
    source.reserve(input_file.tellg());
    input_file.seekg(0, std::ios::beg);

    source.assign((std::istreambuf_iterator<char>(input_file)),
        std::istreambuf_iterator<char>());
    input_file.close();

    LocationArena loc_arena;
    SExprArena node_arena;
    SpanArena span_arena(std::move(node_arena), std::move(loc_arena));

    CompilerContext ctx(std::move(_options), std::string(path),
        std::move(source), std::move(span_arena));

    auto result = builder<std::monostate>()
                      .add<LexPass>()
                      .add<ParsePass>()
                      .add<ExpandPass>()
                      .add<SemaPass>()
                      .add<MutabilityPass>()
                      .add<LowerPass>()
                      .add<SimplifyPass>()
                      .build()
                      .run({}, ctx);

    if (!result.is_valid())
        return 1;

    return 0;
}

} // namespace lpc
