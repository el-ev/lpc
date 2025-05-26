import std;
import lpc.logging;
import lpc.option;
import lpc.session;

using namespace lpc;

auto main(int argc, char* argv[]) noexcept -> int {
    Session session;
    // maybe move the logger into the session, don't know though
    Logger::builder().output(std::cerr).build().make_active();

    App::builder("lpc", "Iris Shi")
        .enable_help()
        .set_non_option_callback([&](auto&& args) {
            session.set_input_files(std::forward<decltype(args)>(args));
        })
        .add_option("output", 'o', "Output file path", "out.c",
            [&](auto path) { session.set_output_file(path); })
        .add_option("print-tokens", NO_SHORT_NAME, "Print tokens to stdout",
            [&](auto) { session.enable_print_tokens(); })
        .add_option("print-ast", NO_SHORT_NAME, "Print AST to stdout",
            [&](auto) { session.enable_print_ast(); })
        .build()
        .parse({ argv + 1, argv + argc });

    return session.run();
}
