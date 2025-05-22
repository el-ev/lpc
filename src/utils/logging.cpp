module;

#include <optional>

module lpc.logging;

namespace lpc {

std::optional<Logger> logger;

Logger LoggerConfig::LoggerConfigBuilder::build() noexcept {
    return Logger(LoggerConfig(_filter, _output.get(), _always_flush));
}

void Logger::set_logger(Logger&& new_logger) noexcept {
    logger.emplace(std::move(new_logger));
}

void Logger::make_active() noexcept {
    set_logger(std::move(*this));
}

} // namespace lpc
