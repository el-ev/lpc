module lpc.session;

import lpc.utils.logging;
import lpc.frontend.expand;
import lpc.frontend.annonate;
import lpc.frontend.lexer;
import lpc.frontend.canonicalize;
import lpc.frontend.syntax;
import lpc.passes;
import lpc.cps.lower;

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

    std::ifstream input_file(path.data());
    if (!input_file.is_open()) {
        Error("Failed to open input file: {}", path);
        return 1;
    }
    std::string source((std::istreambuf_iterator<char>(input_file)),
        std::istreambuf_iterator<char>());
    input_file.close();

    frontend::Lexer lexer(path, source);
    if (lexer.is_failed())
        return 1;

    auto tokens = lexer.tokens();
    auto loc_arena = lexer.loc_arena();

    if (std::ranges::find(_print_passes, "token") != _print_passes.end()) {
        for (const auto& token : tokens)
            std::print("{} ", loc_arena[token.loc()].lexeme());
        std::println("");
    }

    frontend::Parser parser(std::move(tokens), std::move(loc_arena));

    if (parser.is_failed())
        return 1;

    auto root = parser.root();
    auto node_arena = std::move(parser.arena());

    if (std::ranges::find(_print_passes, "raw") != _print_passes.end()) {
        if (_print_json)
            std::print("{}", node_arena.dump_json(root, 2));
        else
            std::print("{}", node_arena.dump(root));
    }

    PassManager pass_manager;
    pass_manager.add_passes<
        // frontend::ExpandPass,
        frontend::AnnonatePass,
        frontend::CanonicalizePass,
        cps::LowerPass
    >();
    root = pass_manager.run_all(root, node_arena, _print_passes, _print_json);
    if (!root.is_valid())
        return 1;
    return 0;
}

} // namespace lpc
