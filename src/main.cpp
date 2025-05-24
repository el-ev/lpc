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

    App::builder("lpc", "Iris Shi")
        .enable_help()
        .set_non_option_callback([&](std::vector<std::string_view>&& args) {
            session.set_input_files(std::move(args));
        })
        .add_option("output", 'o', "Output file path", "out.c",
            [&](std::string_view path) { session.set_output_file(path); })
        .add_option("print-tokens", NO_SHORT_NAME, "Print tokens to stdout",
            [&](std::string_view) { session.enable_print_tokens(); })
        .add_option("print-ast", NO_SHORT_NAME, "Print AST to stdout",
            [&](std::string_view) { session.enable_print_ast(); })
        .build()
        .parse({ argv + 1, argv + argc });

    return session.run();
}
