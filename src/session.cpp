module;

#include <fstream>

module lpc.session;

import lpc.logging;

namespace lpc {

int Session::run() noexcept {
    if (_input_file_paths.empty()) {
        Error("No input files provided");
        return 1;
    }
    if (_input_file_paths.size() > 1) {
        Warn("Warning: Multiple input files provided, "
             "only the first one will be used");
    }
    std::ifstream input_file(_input_file_paths[0].data());

    return 0;
}

} // namespace lpc