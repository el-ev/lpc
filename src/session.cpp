module lpc.session;

import std;

import lpc.backend.interp;
import lpc.context;
import lpc.cps.lower;
import lpc.cps.simplify;
import lpc.passes;
import lpc.sema.expand;
import lpc.sema.mutability;
import lpc.sema.sema;
import lpc.syntax.arenas;
import lpc.syntax.ast;
import lpc.syntax.lexer;
import lpc.syntax.refs;
import lpc.syntax.syntax;
import lpc.utils.logging;

namespace lpc {

using namespace lpc::syntax;
using namespace lpc::sema;
using namespace lpc::cps;

using lpc::utils::Error;

int Session::run() noexcept {
    try {
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

        std::string source((std::istreambuf_iterator<char>(input_file)),
            std::istreambuf_iterator<char>());
        if (input_file.bad()) {
            Error("Failed to read input file: {}", path);
            return 1;
        }

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
                          .run({ }, ctx);

        if (!result.is_valid()) {
            if (ctx.stopped_after())
                return 0;
            return 1;
        }

        backend::Interpreter interpreter(ctx.cps_arena(), ctx.span_arena());
        static_cast<void>(interpreter.run(result));

        return 0;
    } catch (const std::exception& e) {
        Error("Unhandled error: {}", e.what());
        return 1;
    } catch (...) {
        Error("Unhandled unknown error");
        return 1;
    }
}

} // namespace lpc
