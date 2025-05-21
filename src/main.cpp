#include "logging.h"

#include <iostream>

using namespace lpc;

auto main(int argc, char* argv[]) -> int {
    if (argc == 1)
        std::cout << "No arguments provided.\n";

    Logger::builder()
        .output(std::cerr)
        .max_buffer_size(1024)
        .build()
        .make_active();
    LOG(LogLevel::DEBUG, "Debug message");
    LOG(LogLevel::ERROR, "Error message: ", 0.12f);
    LOG(LogLevel::INFO, "Info message: ", reinterpret_cast<void*>(main));
    return 0;
}
