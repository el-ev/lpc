#ifndef _LPC_LOGGING_H
#define _LPC_LOGGING_H

#include <concepts>
#include <iostream>
#include <optional>
#include <sstream>
#include <string>

namespace lpc {

enum class LogLevel {
    DEBUG,
    INFO,
    WARN,
    ERROR,
};

#define LOG_TRACE(...) LOG(::lpc::LogLevel::TRACE, __VA_ARGS__)
#define LOG_DEBUG(...) LOG(::lpc::LogLevel::DEBUG, __VA_ARGS__)
#define LOG_INFO(...) LOG(::lpc::LogLevel::INFO, __VA_ARGS__)
#define LOG_ERROR(...) LOG(::lpc::LogLevel::ERROR, __VA_ARGS__)

#define LOG(level, ...)                                                      \
    ::lpc::Logger::_vlog(level,                                              \
                         "[" __FILE__ ":" + std::to_string(__LINE__) + "] ", \
                         __VA_ARGS__)

template <typename T>
concept Streamable = requires(std::ostream &os, T value) {
    { os << value } -> std::convertible_to<std::ostream &>;
};

inline constexpr std::string_view logLevelToString(LogLevel level) {
    switch (level) {
        case LogLevel::DEBUG: return "[DEBUG]";
        case LogLevel::INFO : return "[INFO]";
        case LogLevel::WARN : return "[WARN]";
        case LogLevel::ERROR: return "[ERROR]";
    }
    return "[UNKNOWN]";
}

class Logger {
private:
    LogLevel filter;
    std::ostream &out;
    std::stringstream buf;
    size_t maxBufferSize = 4096;

public:
    Logger(LogLevel level = LogLevel::INFO, std::ostream &output = std::cout)
        : filter(level), out(output), buf() {}
    static void setLogger(LogLevel filter = LogLevel::INFO,
                          std::ostream &out = std::cout);

    template <Streamable T, Streamable... Args>
    static void _vlog(LogLevel level, const std::string &_place, T &&_msg,
                      Args &&..._args);

    void flush() {
        if (buf.str().size() > 0) {
            out << buf.str();
            buf.str("");
        }
    }

    ~Logger() {
        flush();
        if (&out != &std::cout)
            out.flush();
    }

private:
    template <typename... Args>
    void _vlog_impl(Args &&...args);
};

extern std::optional<Logger> logger;

template <typename... Args>
void Logger::_vlog_impl(Args &&...args) {
    ((buf << std::forward<Args>(args)), ...);
    buf << std::endl;
    if (buf.str().size() >= maxBufferSize)
        flush();
}

template <Streamable T, Streamable... Args>
void Logger::_vlog(LogLevel level, const std::string &_place, T &&_msg,
                   Args &&..._args) {
    if (!logger)
        return;
    if (level < logger->filter)
        return;
    logger->_vlog_impl(logLevelToString(level), _place, std::forward<T>(_msg),
                       std::forward<Args>(_args)...);
}

}  // namespace lpc

#endif  // _LPC_LOGGING_H
