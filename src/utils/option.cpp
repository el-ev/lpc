module lpc.option;

namespace lpc::utils {

void App::display_help() const noexcept {
    std::println("Help for {}", _name);
    std::println("Author: {}", _author);
    if (!_description.empty())
        std::println("Description: {}", _description);
    std::println("\nOptions:");

    std::size_t max_long_width = std::ranges::max(_options
        | std::views::transform(
            [](const Option& opt) { return opt.long_name.length(); }));

    for (const Option& option : _options) {
        if (option.short_name != NO_SHORT_NAME)
            std::print("  -{}  ", option.short_name);
        else
            std::print("      ");

        std::print("--{:<{}}  ", option.long_name, max_long_width);

        if (!option.description.empty())
            std::print("{}", option.description);
        if (!option.default_value.empty())
            std::print(" (default: {})", option.default_value);
        std::println();
    }
}

void App::enable_help() noexcept {
    for (const auto& option : _options)
        if (option.long_name == "help" || option.short_name == 'h') {
            Error("Option already exists: '--help' or '-h'");
            return;
        }

    // Option adding is deferred until parse() is called
    // because App is moved from the builder after construction
    _help_enabled = true;
}

void App::parse(std::vector<std::string_view> args) noexcept {
    // Apply default values
    for (const auto& option : _options)
        if (!option.default_value.empty() && option.callback)
            option.callback(option.default_value);

    if (_help_enabled)
        _options.emplace_back('h', "help", "Display this help message", false,
            "", [this](std::string_view) {
                display_help();
                std::quick_exit(0);
            });

    std::vector<std::string_view> non_option_args;

    for (auto it = args.begin(); it != args.end(); ++it) {
        if (it->empty())
            continue;

        if ((*it)[0] != '-') {
            non_option_args.push_back(*it);
            continue;
        }

        if (*it == "-") {
            non_option_args.emplace_back("/dev/stdin");
            continue;
        }

        if (*it == "--") {
            std::ranges::move(
                ++it, args.end(), std::back_inserter(non_option_args));
            break;
        }

        const Option* found_option = nullptr;

        if (it->starts_with("--")) {
            // Long option
            std::string_view option_name = it->substr(2);

            auto opt_it = std::ranges::find_if(_options.begin(), _options.end(),
                [option_name](const Option& opt) {
                    return opt.long_name == option_name;
                });
            if (opt_it != _options.end())
                found_option = &*opt_it;
        } else if (it->length() == 2) {
            // Short option
            char short_opt = (*it)[1];
            auto opt_it = std::ranges::find_if(_options.begin(), _options.end(),
                [short_opt](
                    const Option& opt) { return opt.short_name == short_opt; });
            if (opt_it != _options.end())
                found_option = &*opt_it;
        }

        if (found_option == nullptr) {
            Error("Unknown option: ", *it);
            std::println("Run with '--help' to see available options.");
            std::quick_exit(1);
        }

        if (found_option->accepts_value) {
            ++it;
            if (it == args.end()) {
                Error("Option", found_option->long_name, "requires a value");
                std::quick_exit(1);
            }
            if (found_option->callback)
                found_option->callback(*it);
        } else if (found_option->callback)
            found_option->callback("");
    }

    if (_non_option_callback)
        _non_option_callback(std::move(non_option_args));
}

void App::add_option(Option&& option) noexcept {
    if (option.long_name.empty()) {
        Error("Option name cannot be empty");
        return;
    }

    if (option.short_name != NO_SHORT_NAME
        && std::isalpha(option.short_name) == 0) {
        Error("Invalid short option name: '-", option.short_name,
            "'. Must be a letter.");
        return;
    }

    if (std::isalpha(option.long_name[0]) == 0
        || !std::ranges::all_of(option.long_name,
            [](char c) { return std::isalnum(c) || c == '-'; })) {
        Error("Invalid option name: '--", option.long_name, "'");
        return;
    }

    if (option.short_name != NO_SHORT_NAME
        && option.short_name != option.long_name[0])
        Warn("Short option \"-", option.short_name,
            "\"does not match long option \" --", option.long_name, "\"");

    if (_help_enabled
        && (option.long_name == "help" || option.short_name == 'h')) {
        Error("Option already exists: '--help' or '-h'");
        return;
    }

    for (const Option& existing : _options) {
        if (existing.long_name == option.long_name
            || (existing.short_name != NO_SHORT_NAME
                && existing.short_name == option.short_name)) {
            Error("Option already exists: '--", existing.long_name, "' or '-",
                existing.short_name, "'");
            return;
        }
    }

    _options.push_back(std::move(option));
}

} // namespace lpc::utils
