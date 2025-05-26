module lpc.session;

import std;
import lpc.logging;
import lpc.frontend.lexer;
import lpc.frontend.parser;

namespace lpc {

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

    if (_print_tokens) {
        for (const auto& token : tokens)
            std::print("{} ", token.literal());
        std::println("\n");
    }

    frontend::Parser parser(std::move(tokens));

    if (parser.is_failed()) {
        Error("Failed to parse input file: ", _input_file_paths[0]);
        return 1;
    }

    auto root = parser.root();

    if (_print_ast)
        std::println("{}\n", root->dump_json());

    return 0;
}

} // namespace lpc
