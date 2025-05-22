import lpc.logging;
#include <iostream>

using namespace lpc;

auto main(int argc, char* argv[]) -> int {
    try {
        if (argc == 1)
            std::cout << "No arguments provided.\n";

        Logger::builder().output(std::cerr).build().make_active();
        Debug();
        Info("Error message: ", 0.12f);
        Warn("Info message: ", reinterpret_cast<void*>(main));
    } catch (const std::exception& e) {
        std::cerr << "Exception: " << e.what() << '\n';
    } catch (...) {
        std::cerr << "Unknown exception\n";
    }
    return 0;
}
