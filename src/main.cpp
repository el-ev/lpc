#include <fstream>
#include <iostream>
#include <print>
#include <string>

import lpc.logging;

using namespace lpc;

auto main(int argc, char* argv[]) -> int {
    try {
        if (argc != 2) {
            std::cerr << "Usage: " << argv[0] << " <file>\n";
            return 1;
        }
        Logger::builder().output(std::cerr).build().make_active();
        // TODO: multiple files
        if (argc > 2)
            Warn("Multiple files (arguments) provided, only the first will be used");
        // TODO: arg parsing
        std::string file_path = argv[1];
        std::ifstream input(file_path);
        if (!input) {
            Error("Failed to open file: ", file_path);
            return 1;
        }
        std::string content((std::istreambuf_iterator<char>(input)),
            std::istreambuf_iterator<char>());

        std::print("{}", content);

    } catch (const std::exception& e) {
        std::cerr << "Exception: " << e.what() << '\n';
    } catch (...) {
        std::cerr << "Unknown exception\n";
    }
    return 0;
}
