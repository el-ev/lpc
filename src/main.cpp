import std;

import lpc.session;
import lpc.utils.logging;
import lpc.utils.option;

using namespace lpc;

using namespace lpc::utils;

auto main(int argc, char* argv[]) noexcept -> int {
    Session session;
    // maybe move the logger into the session, don't know though
    Logger::builder().output(std::cerr).build().make_active();

    App::builder("lpc", "Iris Shi")
        .enable_help()
        .set_non_option_callback([&](auto&& args) {
            std::vector<std::string> files(args.begin(), args.end());
            session.set_input_files(std::move(files));
        })
        .add_option("output", 'o', "Output file path", "out.c",
            [&](auto path) { session.set_output_file(std::string(path)); })
        .add_option("print", NO_SHORT_NAME,
            "Print intermediate representations, separated by commas. "
            "(lex, parse, expand, all)",
            "", [&](auto passes_str) { session.set_print_passes(passes_str); })
        .add_option("stop-after", NO_SHORT_NAME,
            "Stop after the specified pass (lex, parse, expand, sema).", "",
            [&](auto pass) { session.set_stop_after(pass); })
        .add_option("backend", 'b',
            "Backend to use. Currently only 'interp' is supported.", "interp",
            [&](auto backend) {
                if (!session.set_backend(backend))
                    std::quick_exit(1);
            })
        .add_option("show-core-expansion", NO_SHORT_NAME,
            "Show core expansion frames in error stack traces.",
            [&](auto) { session.set_show_core_expansion(true); })
        .add_option("max-expansion-depth", NO_SHORT_NAME,
            "Maximum macro expansion depth.", "1000",
            [&](auto depth) {
                try {
                    session.set_max_expansion_depth(
                        std::stoul(std::string(depth)));
                } catch (...) {
                    std::println(std::cerr, "Invalid depth: {}", depth);
                    std::quick_exit(1);
                }
            })
        .build()
        .parse({ argv + 1, argv + argc });

    return session.run();
}
