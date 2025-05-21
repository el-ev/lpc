#ifndef _LPC_LOGGING_H
#define _LPC_LOGGING_H

#include <concepts>
#include <cstddef>
#include <cstdint>
#include <format>
#include <functional>
#include <iostream>
#include <optional>
#include <source_location>
#include <sstream>

namespace lpc {

enum class LogLevel : uint8_t {
    DEBUG,
    INFO,
    WARN,
    ERROR,
};

#define LOG(level, ...)                                                        \
    ::lpc::Logger::vlog(level, std::source_location::current(), __VA_ARGS__)

template <typename T>
concept Streamable = requires(std::ostream& os, T value) {
    { os << value } -> std::convertible_to<std::ostream&>;
};

constexpr std::ostream& operator<<(std::ostream& os, LogLevel level) {
    switch (level) {
    case LogLevel::DEBUG: os << "[DEBUG]"; break;
    case LogLevel::INFO : os << "[INFO ]"; break;
    case LogLevel::WARN : os << "[WARN ]"; break;
    case LogLevel::ERROR: os << "[ERROR]"; break;
    }
    return os;
}

const size_t DEFAULT_MAX_BUFFER_SIZE = 1024;

class Logger;
class LoggerConfig {
private:
    LogLevel _filter;
    std::reference_wrapper<std::ostream> _output;
    size_t _max_buffer_size;
    bool _always_flush;

    friend class Logger;

    explicit LoggerConfig(LogLevel filter, std::ostream& output,
        size_t max_buffer_size, bool always_flush)
        : _filter(filter)
        , _output(output)
        , _max_buffer_size(max_buffer_size)
        , _always_flush(always_flush) {
    }

    class LoggerConfigBuilder {
    private:
#ifdef NDEBUG
        LogLevel _filter = LogLevel::INFO;
#else // !NDEBUG
        LogLevel _filter = LogLevel::DEBUG;
#endif //
        std::reference_wrapper<std::ostream> _output { std::cout };
        size_t _max_buffer_size = DEFAULT_MAX_BUFFER_SIZE;
        bool _always_flush = false;

        explicit LoggerConfigBuilder() = default;

        friend class LoggerConfig;

    public:
        explicit LoggerConfigBuilder(const LoggerConfigBuilder&) = delete;
        LoggerConfigBuilder& operator=(const LoggerConfigBuilder&) = delete;

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

    LoggerConfig _config;
    std::ostream& _out;
    std::stringstream _buf;

public:
    explicit Logger(const Logger& logger) = delete;
    Logger& operator=(const Logger& logger) = delete;

    Logger(Logger&& other) = default;
    Logger& operator=(Logger&& other) = delete;

    explicit Logger(LoggerConfig config)
        : _config(config)
        , _out(_config._output.get()) {
    }

    static void set_logger(Logger&& logger) noexcept;
    void make_active() noexcept;

    void flush() {
        if (!_buf.str().empty()) {
            _out << _buf.str();
            _buf.str("");
        }
    }

    ~Logger() {
        flush();
        if (&_out != &std::cout)
            _out.flush();
    }

    template <typename... Args>
    static void vlog(LogLevel level, std::source_location loc, Args&&... args);

    [[nodiscard]] static LoggerBuilder builder() noexcept {
        return LoggerConfig::builder();
    }

private:
    template <typename... Args>
    void vlog_impl(Args&&... args);
};

extern std::optional<Logger> logger;

template <typename... Args>
void Logger::vlog_impl(Args&&... args) {
    ((_buf << std::forward<Args>(args)), ...);
    _buf << "\n";
    if (_config._always_flush || _buf.str().size() >= _config._max_buffer_size)
        flush();
}

template <typename... Args>
void Logger::vlog(LogLevel level, std::source_location loc, Args&&... args) {
    auto format_source_location = [&]() {
        return std::format("[{}:{}] ", loc.file_name(), loc.line());
    };
    if (!logger || level < logger->_config._filter)
        return;
    logger->vlog_impl(
        level, format_source_location(), std::forward<Args>(args)...);
}
} // namespace lpc

#endif // _LPC_LOGGING_H
