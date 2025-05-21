#include <iostream>
#include <string_view>

#include "logging.h"

using namespace lpc;

auto main(int argc, char* argv[]) -> int {
    if (argc == 1)
        std::cout << "No arguments provided." << std::endl;
    Logger::builder()
        .filter(LogLevel::DEBUG)
        .output(std::cout)
        .max_buffer_size(1024)
        .build()
        .make_active();
    LOG(LogLevel::DEBUG, "Debug message");
    LOG(LogLevel::ERROR, "Error message: ", 0.12f);
    LOG(LogLevel::INFO, "Info message: ", (void*)main);
    LOG_ERROR(std::string_view("123"));
    return 0;
}
