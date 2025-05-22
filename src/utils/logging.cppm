module;

#include <concepts>
#include <cstdint>
#include <format>
#include <functional>
#include <iostream>
#include <optional>
#include <source_location>

export module lpc.logging;

namespace lpc {

enum class LogLevel : uint8_t {
    DEBUG,
    INFO,
    WARN,
    ERROR,
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

template <typename T>
concept Streamable = requires(std::ostream& os, T value) {
    { os << value } -> std::convertible_to<std::ostream&>;
};

export class Logger;
class LoggerConfig {
private:
    LogLevel _filter;
    std::reference_wrapper<std::ostream> _output;
    bool _always_flush;

    friend class Logger;

    explicit LoggerConfig(
        LogLevel filter, std::ostream& output, bool always_flush)
        : _filter(filter)
        , _output(output)
        , _always_flush(always_flush) {
    }

    class LoggerConfigBuilder {
    private:
#ifdef NDEBUG
        LogLevel _filter = LogLevel::INFO;
#else // !NDEBUG
        LogLevel _filter = LogLevel::DEBUG;
#endif // NDEBUG
        std::reference_wrapper<std::ostream> _output { std::cout };
        bool _always_flush = false;

    public:
        explicit LoggerConfigBuilder() = default;

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

    ~Logger() {
        if (&_out != &std::cout)
            _out.flush();
    }

    template <Streamable... Args>
    static void vlog(
        LogLevel level, const std::source_location& loc, Args&&... args);

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
    (_out << ... << std::forward<Args>(args)) << '\n';
    if (_config._always_flush)
        _out.flush();
}

template <Streamable... Args>
void Logger::vlog(
    LogLevel level, const std::source_location& loc, Args&&... args) {
    if (!logger || level < logger->_config._filter)
        return;

    auto loc_string = std::format("[{}:{}] ", loc.file_name(), loc.line());
    logger->vlog_impl(level, loc_string, std::forward<Args>(args)...);
}

namespace log_wrapper {
    template <typename... Args>
    struct LogWrapper {
        explicit LogWrapper(
            Args&&... args, LogLevel level, const std::source_location& loc) {
            Logger::vlog(level, loc, std::forward<Args>(args)...);
        }
    };

    template <typename... Args>
    struct Debug : LogWrapper<Args...> {
        explicit Debug(Args&&... args,
            const std::source_location& loc = std::source_location::current())
            : LogWrapper<Args...>(
                  std::forward<Args>(args)..., LogLevel::DEBUG, loc) {
        }
    };

    template <typename... Args>
    struct Info : LogWrapper<Args...> {
        explicit Info(Args&&... args,
            const std::source_location& loc = std::source_location::current())
            : LogWrapper<Args...>(
                  std::forward<Args>(args)..., LogLevel::INFO, loc) {
        }
    };

    template <typename... Args>
    struct Warn : LogWrapper<Args...> {
        explicit Warn(Args&&... args,
            const std::source_location& loc = std::source_location::current())
            : LogWrapper<Args...>(
                  std::forward<Args>(args)..., LogLevel::WARN, loc) {
        }
    };

    template <typename... Args>
    struct Error : LogWrapper<Args...> {
        explicit Error(Args&&... args,
            const std::source_location& loc = std::source_location::current())
            : LogWrapper<Args...>(
                  std::forward<Args>(args)..., LogLevel::ERROR, loc) {
        }
    };

    template <typename... Args>
    Debug(Args&&...) -> Debug<Args...>;

    template <typename... Args>
    Info(Args&&...) -> Info<Args...>;

    template <typename... Args>
    Warn(Args&&...) -> Warn<Args...>;

    template <typename... Args>
    Error(Args&&...) -> Error<Args...>;
}

export template <typename... Args>
using Debug = log_wrapper::Debug<Args...>;
export template <typename... Args>
using Info = log_wrapper::Info<Args...>;
export template <typename... Args>
using Warn = log_wrapper::Warn<Args...>;
export template <typename... Args>
using Error = log_wrapper::Error<Args...>;
} // namespace lpc
