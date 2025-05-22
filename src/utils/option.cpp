module;

#include <format>
#include <print>
#include <stdexcept>

module lpc.option;

import lpc.logging;
import lpc.session;

namespace lpc {

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

} // namespace lpc