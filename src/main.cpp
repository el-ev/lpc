#include <iostream>
#include <print>
#include <stdexcept>
#include <string>

import lpc.logging;
import lpc.option;
import lpc.session;

using namespace lpc;

auto main(int argc, char* argv[]) noexcept -> int {
    Session session;
    // maybe move the logger into the session, don't know though
    Logger::builder().output(std::cerr).build().make_active();

    if (argc < 2) {
        Error("No arguments provided");
        return 1;
    }

    App app = App::builder("lpc", "Iris Shi")
                  .enable_help()
                  .add_option("output", true, true, "out.c", "Output file path",
                      Session::set_output_file)
                  .build();
    try {
        app.parse(session, { argv + 1, argv + argc });
    } catch (const HelpMessageDisplayedException&) {
        return 0;
    } catch (const std::invalid_argument&) {
        return 1;
    } catch (const std::exception& e) {
        Error("Exception: ", e.what());
        return 1;
    }

    return session.run();
}
