#include <iostream>

#include "logging.h"

using namespace lpc;

auto main(int argc, char* argv[]) -> int {
    if (argc == 1)
        std::cout << "No arguments provided." << std::endl;
    Logger::setLogger(LogLevel::DEBUG, std::cout);
    LOG(LogLevel::DEBUG, "Debug message");
    LOG(LogLevel::ERROR, "Error message: ", 0.12f);
    LOG(LogLevel::INFO, "Info message: ", (void*)main);
    return 0;
}
