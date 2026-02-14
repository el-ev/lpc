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

    frontend::LocationArena loc_arena;
    frontend::SExprArena node_arena(loc_arena);

    frontend::Lexer lexer(loc_arena, path, source);
    if (lexer.is_failed())
        return 1;

    auto tokens = lexer.tokens();

    if (std::ranges::find(_print_passes, "token") != _print_passes.end()) {
        for (const auto& token : tokens)
            std::print("{} ", loc_arena[token.loc()].lexeme());
        std::println("");
    }

    frontend::Parser parser(std::move(tokens), node_arena);

    if (parser.is_failed())
        return 1;

    auto root = parser.root();

    if (std::ranges::find(_print_passes, "raw") != _print_passes.end())
        std::print("{}", node_arena.dump_root(root.expr_ref()));

    PassManager pass_manager;
    pass_manager.add_pass<frontend::ExpandPass>(
        _show_core_expansion, _max_expansion_depth);
    root = pass_manager.run_all(root, node_arena, _print_passes);
    if (!root.is_valid())
        return 1;

    if (_backend == "interp") {
        // backend::Interp interp;
        return 0;
    }

    return 0;
}

} // namespace lpc
