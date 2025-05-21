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

#define LOG(level, ...)                                                        \
    ::lpc::Logger::_vlog(level,                                                \
        "[" __FILE__ ":" + std::to_string(__LINE__) + "] ", __VA_ARGS__)

template <typename T>
concept Streamable = requires(std::ostream& os, T value) {
    { os << value } -> std::convertible_to<std::ostream&>;
};

inline constexpr std::string_view logLevelToString(LogLevel level) noexcept {
    switch (level) {
    case LogLevel::DEBUG: return "[DEBUG]";
    case LogLevel::INFO : return "[INFO]";
    case LogLevel::WARN : return "[WARN]";
    case LogLevel::ERROR: return "[ERROR]";
    }
    return "[UNKNOWN]";
}

class Logger;
class LoggerConfig {
private:
    LogLevel filter;
    std::reference_wrapper<std::ostream> output;
    size_t max_buffer_size;
    bool always_flush;

    friend class Logger;

    explicit LoggerConfig(LogLevel _filter, std::ostream& _output,
        size_t _max_buffer_size, bool _always_flush)
        : filter(_filter)
        , output(_output)
        , max_buffer_size(_max_buffer_size)
        , always_flush(_always_flush) {
    }

    class LoggerConfigBuilder {
    private:
#ifdef NDEBUG
        LogLevel _filter = LogLevel::INFO;
#else // !NDEBUG
        LogLevel _filter = LogLevel::DEBUG;
#endif //
        std::reference_wrapper<std::ostream> _output { std::cout };
        size_t _max_buffer_size = 1024;
        bool _always_flush = false;

        explicit LoggerConfigBuilder() = default;
        explicit LoggerConfigBuilder(const LoggerConfigBuilder&) = delete;
        LoggerConfigBuilder& operator=(const LoggerConfigBuilder&) = delete;

        friend class LoggerConfig;

    public:
        [[nodiscard]] LoggerConfigBuilder& filter(LogLevel f) noexcept {
            _filter = f;
            return *this;
        }

        [[nodiscard]] LoggerConfigBuilder& output(std::ostream& o) noexcept {
            _output = std::reference_wrapper<std::ostream>(o);
            return *this;
        }

        [[nodiscard]] LoggerConfigBuilder& max_buffer_size(size_t m) noexcept {
            _max_buffer_size = m;
            return *this;
        }

        [[nodiscard]] LoggerConfigBuilder& always_flush(bool f) noexcept {
            _always_flush = f;
            return *this;
        }

        [[nodiscard]] Logger build() noexcept;
    };

public:
    [[nodiscard]] static LoggerConfigBuilder builder() noexcept {
        return LoggerConfigBuilder();
    }
};

class Logger {
private:
    using LoggerBuilder = LoggerConfig::LoggerConfigBuilder;

    LoggerConfig config;
    std::ostream& out;
    std::stringstream buf;

    explicit Logger(const Logger& Logger) = delete;
    Logger& operator=(const Logger& Logger) = delete;

public:
    explicit Logger(Logger&& other) = default;
    explicit Logger(LoggerConfig config)
        : config(config)
        , out(config.output.get())
        , buf() {
    }

    static void set_logger(Logger&& _logger) noexcept;
    void make_active() noexcept;

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

    template <Streamable T, Streamable... Args>
    static void _vlog(
        LogLevel level, const std::string& _place, T&& _msg, Args&&... _args);

    [[nodiscard]] static LoggerBuilder builder() noexcept {
        return LoggerConfig::builder();
    }

private:
    template <typename... Args> void _vlog_impl(Args&&... args);
};

extern std::optional<Logger> logger;

template <typename... Args> void Logger::_vlog_impl(Args&&... args) {
    ((buf << std::forward<Args>(args)), ...);
    buf << std::endl;
    if (config.always_flush || buf.str().size() >= config.max_buffer_size)
        flush();
}

template <Streamable T, Streamable... Args>
void Logger::_vlog(
    LogLevel level, const std::string& _place, T&& _msg, Args&&... _args) {
    if (!logger)
        return;
    if (level < logger->config.filter)
        return;
    logger->_vlog_impl(logLevelToString(level), _place, std::forward<T>(_msg),
        std::forward<Args>(_args)...);
}
} // namespace lpc

#endif // _LPC_LOGGING_H
