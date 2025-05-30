module lpc.utils.logging;

namespace lpc::utils {

std::optional<Logger> logger;

void Logger::set_logger(Logger&& new_logger) noexcept {
    logger.emplace(std::move(new_logger));
}

void Logger::make_active() noexcept {
    set_logger(std::move(*this));
}

} // namespace lpc
