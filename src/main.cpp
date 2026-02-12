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
            std::vector<std::string> files;
            files.reserve(args.size());
            for (auto sv : args)
                files.emplace_back(sv);
            session.set_input_files(std::move(files));
        })
        .add_option("output", 'o', "Output file path", "out.c",
            [&](auto path) { session.set_output_file(std::string(path)); })
        .add_option("print", NO_SHORT_NAME,
            "Print intermediate representations, separated by commas. (token, "
            "raw, ...)",
            "", [&](auto passes_str) { session.set_print_passes(passes_str); })
        .add_option("backend", 'b',
            "Backend to use. Currently only 'interp' is supported.", "interp",
            [&](auto backend) {
                if (!session.set_backend(backend))
                    std::quick_exit(1);
            })
        .add_option("show-core-expansion", NO_SHORT_NAME,
            "Show core expansion frames in error stack traces.",
            [&](auto) { session.set_show_core_expansion(true); })
        .build()
        .parse({ argv + 1, argv + argc });

    return session.run();
}
