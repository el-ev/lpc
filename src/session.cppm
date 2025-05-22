module;

#include <fstream>
#include <optional>

export module lpc.session;

namespace lpc {

export class Session {
private:
    std::optional<std::ofstream> _output_file;
    std::string_view _output_file_path;

public:
    explicit Session() = default;
    explicit Session(const Session&) = delete;
    Session& operator=(const Session&) = delete;

    Session(Session&&) = default;
    Session& operator=(Session&&) = default;

    ~Session() {
        if (_output_file) {
            _output_file->close();
        }
    }

    static void set_output_file(Session& session, std::string_view path) {
        session._output_file_path = path;
        session._output_file.emplace(path.data());
    }
};

} // namespace lpc