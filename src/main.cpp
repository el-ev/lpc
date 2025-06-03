import std;
import lpc.utils.logging;
import lpc.utils.option;
import lpc.session;

using namespace lpc;

using namespace lpc::utils;

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
        .add_option("print", NO_SHORT_NAME,
            "Print intermediate representations, separated by commas. (token, "
            "sexpr, annonate, ...)",
            "", [&](auto passes_str) { session.set_print_passes(passes_str); })
        .build()
        .parse({ argv + 1, argv + argc });

    return session.run();
}
