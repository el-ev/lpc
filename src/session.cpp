module lpc.session;

import lpc.logging;
import lpc.frontend.lexer;
import lpc.frontend.parser;

namespace lpc {

using lpc::utils::Error;
using lpc::utils::Warn;

int Session::run() noexcept {
    if (_input_file_paths.empty()) {
        Error("No input files");
        return 1;
    }
    if (_input_file_paths.size() > 1)
        Warn("Warning: Multiple input files provided, "
             "only the first one will be used");

    std::ifstream input_file(_input_file_paths[0].data());

    if (!input_file.is_open()) {
        Error("Failed to open input file: ", _input_file_paths[0]);
        return 1;
    }

    std::string source((std::istreambuf_iterator<char>(input_file)),
        std::istreambuf_iterator<char>());

    input_file.close();

    frontend::Lexer lexer(_input_file_paths[0], source);

    if (lexer.is_failed()) {
        Error("Failed to lex input file: ", _input_file_paths[0]);
        return 1;
    }

    auto tokens = lexer.tokens();
    auto loc_arena = lexer.loc_arena();

    if (_print_tokens) {
        for (const auto& token : tokens)
            std::print("{} ", token.lexeme());
        std::println("\n");
    }

    frontend::Parser parser(std::move(tokens));

    if (parser.is_failed()) {
        Error("Failed to parse input file: ", _input_file_paths[0]);
        return 1;
    }

    auto root = parser.root();
    auto ast_arena = std::move(parser.arena());

    if (_print_ast)
        std::println("{}\n", ast_arena[root].dump_json(ast_arena, loc_arena));

    return 0;
}

} // namespace lpc
