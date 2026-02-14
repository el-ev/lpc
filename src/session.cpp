module lpc.session;

import std;

import lpc.analysis.expand;
import lpc.context;
import lpc.core.arenas;
import lpc.core.refs;
import lpc.passes;
import lpc.syntax.ast;
import lpc.syntax.lexer;
import lpc.syntax.syntax;
import lpc.utils.logging;

namespace lpc {

using namespace lpc::core;
using namespace lpc::syntax;
using namespace lpc::analysis;

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
    std::string source((std::istreambuf_iterator<char>(input_file)),
        std::istreambuf_iterator<char>());
    input_file.close();

    LocationArena loc_arena;
    SExprArena node_arena;
    SpanArena span_arena(std::move(node_arena), std::move(loc_arena));

    Lexer lexer(span_arena.location_arena(), path, source);
    if (lexer.is_failed())
        return 1;

    auto tokens = lexer.tokens();

    if (_options.should_print("token")) {
        for (const auto& token : tokens)
            std::print("{} ", span_arena.loc(token.loc()).lexeme());
        std::println("");
    }

    Parser parser(std::move(tokens), span_arena);

    if (parser.is_failed())
        return 1;

    auto root = parser.root();

    if (_options.should_print("raw"))
        std::print("{}", span_arena.dump_root(root));

    CompilerContext ctx(std::move(_options), std::move(span_arena));

    auto result = PassManager().add<ExpandPass>().run_all(root, ctx);

    if (!result.is_valid())
        return 1;

    return 0;
}

} // namespace lpc
