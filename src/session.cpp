module lpc.session;

import std;

import lpc.backend.interp;
import lpc.cps.lower;
import lpc.frontend.arenas;
import lpc.frontend.ast;
import lpc.frontend.expand;
import lpc.frontend.lexer;
import lpc.frontend.span;
import lpc.frontend.refs;
import lpc.frontend.syntax;
import lpc.passes;
import lpc.utils.logging;

namespace lpc {

using namespace lpc::frontend;
using lpc::utils::Error;
using lpc::utils::Warn;

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

    if (std::ranges::find(_print_passes, "token") != _print_passes.end()) {
        for (const auto& token : tokens)
            std::print("{} ", span_arena.loc(token.loc()).lexeme());
        std::println("");
    }

    frontend::Parser parser(std::move(tokens), span_arena);

    if (parser.is_failed())
        return 1;

    auto root = parser.root();

    if (std::ranges::find(_print_passes, "raw") != _print_passes.end())
        std::print("{}", span_arena.dump_root(root));

    PassManager pass_manager;
    pass_manager.add_pass<frontend::ExpandPass>(
        _show_core_expansion, _max_expansion_depth);
    root = pass_manager.run_all(root, span_arena, _print_passes);
    if (!root.is_valid())
        return 1;

    if (_backend == "interp") {
        // backend::Interp interp;
        return 0;
    }

    return 0;
}

} // namespace lpc
