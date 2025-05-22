module;

#include <algorithm>
#include <format>
#include <functional>
#include <print>
#include <stdexcept>

module lpc.option;

import lpc.logging;
import lpc.session;

namespace lpc {

constexpr void App::display_help() const {
    std::println("Help for {}: ", name);
    std::println("Author: {}", author);
    if (description) {
        std::println("Description: {}", *description);
    }
    std::println("Options:");
    for (const Option& option : options) {
        std::print("  --{}", option.long_name);
        if (option.short_name)
            std::print(", -{}", option.long_name[0]);
        if (option.description)
            std::print(": {}", *option.description);
        if (option.default_value)
            std::print(" (default: {})", *option.default_value);
        std::println();
    }
}

void App::parse(Session& session, std::vector<std::string_view> args) const {
    // first call all options that have default values, if any
    for (const auto& option : options) {
        if (option.default_value) {
            option.callback(session, *option.default_value);
        }
    }
    std::vector<std::string_view> non_option_args;
    for (auto it = args.begin(); it != args.end(); ++it) {
        size_t arg_len = it->length();
        if ((*it)[0] == '-') {
            // check if it's a short option
            if (arg_len == 2) {
                if ((*it)[1] == '-') {
                    // it is exactly "--", treat anything after it as non-option
                    ++it;
                    std::copy(
                        it, args.end(), std::back_inserter(non_option_args));
                    break;
                }
                // it is a short option
                auto opt = std::ranges::find_if(
                    options.begin(), options.end(), [&](const Option& opt) {
                        return opt.short_name && opt.long_name[0] == (*it)[1];
                    });
                if (opt != options.end()) {
                    // found the option
                    if (opt->accepts_value) {
                        ++it;
                        if (it == args.end()) {
                            Error(std::format(
                                "Option {} requires a value", opt->long_name));
                            throw std::invalid_argument("");
                        }
                        opt->callback(session, *it);
                    } else {
                        // no value required
                        if (!opt->callback && opt->long_name == "help") {
                            this->display_help();
                            throw HelpMessageDisplayedException();
                        }
                        opt->callback(session, "");
                    }
                    continue;
                }
                // unknown option
                Error(std::format("Unknown option: {}", *it));
                throw std::invalid_argument("");
            }
            if ((*it)[1] != '-') {
                Error(std::format("Invalid option: {}", *it));
                throw std::invalid_argument("");
            }
            // it is a long option
            auto opt = std::ranges::find_if(
                options.begin(), options.end(), [&](const Option& opt) {
                    return opt.long_name == (*it).substr(2);
                });
            if (opt != options.end()) {
                // found the option
                if (opt->accepts_value) {
                    ++it;
                    if (it == args.end()) {
                        Error(std::format(
                            "Option {} requires a value", opt->long_name));
                        throw std::invalid_argument("");
                    }
                    opt->callback(session, *it);
                } else {
                    // no value required
                    if (!opt->callback && opt->long_name == "help") {
                        this->display_help();
                        throw HelpMessageDisplayedException();
                    }
                    opt->callback(session, "");
                }
                continue;
            }
            // unknown option
            Error(std::format("Unknown option: {}", *it));
            throw std::invalid_argument("");
        }
        // it's not an option
        non_option_args.push_back(*it);
    }

    // check if there are any non-option arguments
    session.set_input_files(std::move(non_option_args));
}

AppBuilder& AppBuilder::enable_help() {
    if (!_app.options.empty())
        Warn("Help option should be the first option");

    // the help option is specially handled
    _app.options.emplace_back(true, "help", false, std::nullopt,
        "Display this help message", nullptr);
    return *this;
}

AppBuilder& AppBuilder::add_option(std::string&& long_name, bool short_name,
    bool accepts_value, std::optional<std::string>&& default_value,
    std::optional<std::string>&& description,
    std::function<void(Session&, std::string_view)> callback) {
    if (!accepts_value && default_value) {
        Warn("Warning: Option", long_name,
            "does not accept a value, but a "
            "default value is provided. Ignoring default value.");
    }
    if (long_name.empty()) {
        Error("Option name cannot be empty");
        return *this;
    }
    if (long_name.length() == 1) {
        Warn("Warning: Option name", long_name, "is too short.");
        short_name = true;
    }
    if (!std::ranges::all_of(long_name.begin(), long_name.end(),
            [](char c) { return std::isalnum(c) || c == '_'; })) {
        Error("Option name", long_name, "contains invalid characters");
        return *this;
    }
    for (const auto& option : _app.options) {
        if (option.long_name == long_name) {
            Error("Option", long_name, "already exists");
            return *this;
        }
        if (option.short_name && short_name
            && option.long_name[0] == long_name[0]) {
            Error("Short option for ", long_name,
                " conflicts with existing option ", option.long_name);
            return *this;
        }
    }
    if (callback == nullptr) {
        Warn("Warning: Option", long_name, "does not have a callback.");
        callback = [](Session& session, std::string_view value) {
            (void)session;
            (void)value;
        };
    }
    _app.options.emplace_back(short_name, std::move(long_name), accepts_value,
        std::move(default_value), std::move(description), std::move(callback));
    return *this;
}

} // namespace lpc