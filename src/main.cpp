import std;
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

    App app
        = App::builder("lpc", "Iris Shi")
              .enable_help()
              .set_non_option_callback(
                  [&](std::vector<std::string_view>&& args) {
                      session.set_input_files(std::move(args));
                  })
              .add_option("output", 'o', true, "out.c", "Output file path",
                  [&](std::string_view path) { session.set_output_file(path); })
              .add_option("print-tokens", 0, false, "",
                  "Print tokens to stdout",
                  [&](std::string_view) { session.enable_print_tokens(); })
              .add_option("print-ast", 0, false, "", "Print AST to stdout",
                  [&](std::string_view) { session.enable_print_ast(); })
              .build();

    try {
        app.parse({ argv + 1, argv + argc });
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
