module;
#include <execinfo.h>
#include <unistd.h>
#include <signal.h>
#include <cstdio>

export module lpc.utils.crash_handler;

import std;

namespace lpc::utils {

void signal_handler(int sig) {
    std::array<void*, 64> array;
    size_t size;

    size = backtrace(array.data(), array.size());

    std::fprintf(stderr, "Error: signal %d:\n", sig);
    backtrace_symbols_fd(array.data(), size, STDERR_FILENO);

    signal(sig, SIG_DFL);
    raise(sig);
}

export void install_crash_handler() {
    signal(SIGSEGV, signal_handler);
    signal(SIGBUS, signal_handler);
    signal(SIGABRT, signal_handler);
    signal(SIGILL, signal_handler);
    signal(SIGFPE, signal_handler);
}

} // namespace lpc::utils
