#include "logging.h"

#include <optional>

namespace lpc {

std::optional<Logger> logger;

Logger LoggerConfig::LoggerConfigBuilder::build() noexcept {
    // There are at least 17 characters in the log level string
    if (_max_buffer_size <= 20)
        _always_flush = true;
    return Logger(
        LoggerConfig(_filter, _output.get(), _max_buffer_size, _always_flush));
}

void Logger::set_logger(Logger&& new_logger) noexcept {
    logger.emplace(std::move(new_logger));
}

void Logger::make_active() noexcept {
    set_logger(std::move(*this));
}
} // namespace lpc
