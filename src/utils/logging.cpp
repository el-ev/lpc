#include "logging.h"

#include <optional>

namespace lpc {

std::optional<Logger> logger;

void Logger::setLogger(LogLevel filter, std::ostream &out) {
    logger.emplace(filter, out);
}
}  // namespace lpc
