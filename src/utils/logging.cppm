export module lpc.utils.logging;

import std;

namespace lpc::utils {

enum class LogLevel : std::uint8_t {
    DEBUG,
    INFO,
    WARN,
    ERROR,
};

inline std::string to_string(const LogLevel& level) {
    switch (level) {
    case LogLevel::DEBUG:
        return "debug";
    case LogLevel::INFO:
        return "info";
    case LogLevel::WARN:
        return "warn";
    case LogLevel::ERROR:
        return "error";
    }
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
    friend class LoggerBuilder;

    explicit constexpr LoggerConfig(
        LogLevel filter, std::ostream& output, bool always_flush)
        : _filter(filter)
        , _output(output)
        , _always_flush(always_flush) {
    }
};

class LoggerBuilder {
private:
    LoggerConfig _config = LoggerConfig {
#ifdef NDEBUG
        LogLevel::INFO,
#else // !NDEBUG
        LogLevel::DEBUG,
#endif // NDEBUG
        std::cout,
        false,
    };

public:
    explicit constexpr LoggerBuilder() = default;

    explicit LoggerBuilder(const LoggerBuilder&) = delete;
    LoggerBuilder& operator=(const LoggerBuilder&) = delete;

    [[nodiscard]] constexpr LoggerBuilder& filter(LogLevel f) noexcept {
        _config._filter = f;
        return *this;
    }

    [[nodiscard]] constexpr LoggerBuilder& output(std::ostream& o) noexcept {
        _config._output = std::reference_wrapper<std::ostream>(o);
        return *this;
    }

    [[nodiscard]] constexpr LoggerBuilder& always_flush(bool f) noexcept {
        _config._always_flush = f;
        return *this;
    }

    [[nodiscard]] inline constexpr Logger build() noexcept;
};

class Logger {
private:
    LoggerConfig _config;
    std::ostream& _out;

public:
    explicit Logger(const Logger& logger) = delete;
    Logger& operator=(const Logger& logger) = delete;

    constexpr Logger(Logger&& other) = default;
    Logger& operator=(Logger&& other) = delete;

    explicit constexpr Logger(LoggerConfig config)
        : _config(config)
        , _out(_config._output.get()) {
    }

    static void set_logger(Logger&& logger) noexcept;
    void make_active() noexcept;

    constexpr ~Logger() noexcept {
        _out.flush();
    }

    template <Streamable... Args>
    static void vlog(LogLevel level, const std::source_location& loc,
        std::format_string<Args...> fmt, Args&&... args);

    [[nodiscard]] static constexpr LoggerBuilder builder() noexcept {
        return LoggerBuilder();
    }

private:
    template <typename... Args>
    void vlog_impl(LogLevel level, const std::source_location& loc,
        std::format_string<Args...> fmt, Args&&... args);
};

extern std::optional<Logger> logger;

template <typename... Args>
void Logger::vlog_impl(LogLevel level, const std::source_location& /*loc*/,
    std::format_string<Args...> fmt, Args&&... args) {
    std::print(
        // _out, "{}[{}:{}] ", to_string(level), loc.file_name(), loc.line());
        _out, "{}: ", to_string(level));
    std::println(_out, fmt, std::forward<Args>(args)...);
    if (_config._always_flush)
        _out.flush();
}

template <Streamable... Args>
void Logger::vlog(LogLevel level, const std::source_location& loc,
    std::format_string<Args...> fmt, Args&&... args) {
    if (!logger || level < logger->_config._filter)
        return;
    logger->vlog_impl(level, loc, fmt, std::forward<Args>(args)...);
}

inline constexpr Logger LoggerBuilder::build() noexcept {
    return Logger(_config);
}

namespace log_wrapper {
    template <typename... Args>
    struct LogWrapper {
        explicit LogWrapper(LogLevel level, const std::source_location& loc,
            std::format_string<Args...> fmt, Args&&... args) {
            Logger::vlog(level, loc, fmt, std::forward<Args>(args)...);
        }
    };

    template <typename... Args>
    struct Debug : LogWrapper<Args...> {
        explicit Debug(std::format_string<Args...> fmt, Args&&... args,
            const std::source_location& loc = std::source_location::current())
            : LogWrapper<Args...>(
                  LogLevel::DEBUG, loc, fmt, std::forward<Args>(args)...) {
        }
    };

    template <typename... Args>
    struct Info : LogWrapper<Args...> {
        explicit Info(std::format_string<Args...> fmt, Args&&... args,
            const std::source_location& loc = std::source_location::current())
            : LogWrapper<Args...>(
                  LogLevel::INFO, loc, fmt, std::forward<Args>(args)...) {
        }
    };

    template <typename... Args>
    struct Warn : LogWrapper<Args...> {
        explicit Warn(std::format_string<Args...> fmt, Args&&... args,
            const std::source_location& loc = std::source_location::current())
            : LogWrapper<Args...>(
                  LogLevel::WARN, loc, fmt, std::forward<Args>(args)...) {
        }
    };

    template <typename... Args>
    struct Error : LogWrapper<Args...> {
        explicit Error(std::format_string<Args...> fmt, Args&&... args,
            const std::source_location& loc = std::source_location::current())
            : LogWrapper<Args...>(
                  LogLevel::ERROR, loc, fmt, std::forward<Args>(args)...) {
        }
    };

    template <typename... Args>
    Debug(std::format_string<Args...> fmt, Args&&...) -> Debug<Args...>;

    template <typename... Args>
    Info(std::format_string<Args...> fmt, Args&&...) -> Info<Args...>;

    template <typename... Args>
    Warn(std::format_string<Args...> fmt, Args&&...) -> Warn<Args...>;

    template <typename... Args>
    Error(std::format_string<Args...> fmt, Args&&...) -> Error<Args...>;
}

export template <typename... Args>
using Debug = log_wrapper::Debug<Args...>;
export template <typename... Args>
using Info = log_wrapper::Info<Args...>;
export template <typename... Args>
using Warn = log_wrapper::Warn<Args...>;
export template <typename... Args>
using Error = log_wrapper::Error<Args...>;

} // namespace lpc::utils
