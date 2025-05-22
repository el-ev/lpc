#include <fstream>
#include <iostream>
#include <print>
#include <string>

import lpc.logging;
import lpc.option;
import lpc.session;

using namespace lpc;

auto main(int argc, char* argv[]) noexcept -> int {
    // maybe move the logger into the session, don't know though
    try {
        Logger::builder().output(std::cerr).build().make_active();
        Session session;

        App app = App::builder("lpc", "Iris Shi")
                      .enable_help()
                      .add_option("output", true, true, "out.c",
                          "Output file path", Session::set_output_file)
                      .build();

        std::optional<std::vector<std::string_view>> ret;
        try {
            ret = app.parse(session, { argv + 1, argv + argc });
        } catch (const HelpMessageDisplayedException& e) {
            return 0;
        }

        if (!ret.has_value()) {
            Error("No input files.");
            return 1;
        }

        auto files = ret.value();
        if (files.empty()) {
            Error("No input files.");
            return 1;
        }

        std::ifstream input((std::string(files[0])));
        if (!input) {
            Error("Failed to open file: ", files[0]);
            return 1;
        }

        std::string content((std::istreambuf_iterator<char>(input)),
            std::istreambuf_iterator<char>());

        std::print("{}", content);
        return 0;
    } catch (const std::exception& e) {
        Error("Exception: ", e.what());
        return 1;
    } catch (...) {
        Error("Unknown exception");
        return 1;
    }
}
