module;
#include <execinfo.h>
#include <unistd.h>
#include <csignal>
#include <cstdio>
#include <print>

export module lpc.utils.crash_handler;

import std;

namespace lpc::utils {

void signal_handler(int sig) {
    std::array<void*, 64> array{};
    int size = 0;

    size = backtrace(array.data(), array.size());

    std::println(stderr, "Error: signal {}:", sig);
    backtrace_symbols_fd(array.data(), size, STDERR_FILENO);

    (void)signal(sig, SIG_DFL);
    (void)raise(sig);
}

export void install_crash_handler() {
    (void)signal(SIGSEGV, signal_handler);
    (void)signal(SIGBUS, signal_handler);
    (void)signal(SIGABRT, signal_handler);
    (void)signal(SIGILL, signal_handler);
    (void)signal(SIGFPE, signal_handler);
}

} // namespace lpc::utils
