module;

#include <algorithm>
#include <functional>
#include <optional>
#include <print>
#include <string_view>
#include <utility>
#include <vector>

export module lpc.option;

import lpc.logging;
import lpc.session;

namespace lpc {

class AppBuilder;

export struct Option {
    bool short_name;
    std::string long_name;
    bool accepts_value;
    std::optional<std::string> default_value;
    std::optional<std::string> description;

    std::function<void(Session&, std::string_view)> callback;

    explicit Option() = delete;
    explicit Option(const Option&) = delete;
    Option& operator=(const Option&) = delete;

    Option(Option&&) = default;
    Option& operator=(Option&&) = default;

    explicit Option(bool short_name, std::string&& long_name,
        bool accepts_value, std::optional<std::string>&& default_value,
        std::optional<std::string>&& description,
        std::function<void(Session&, std::string_view)> callback) noexcept
        : short_name(short_name)
        , long_name(std::move(long_name))
        , accepts_value(accepts_value)
        , default_value(std::move(default_value))
        , description(std::move(description))
        , callback(std::move(callback)) {
    }
};

export struct App {
    std::string name;
    std::string author;
    std::optional<std::string> description;

    std::vector<Option> options;

    explicit App() = delete;
    explicit App(const App&) = delete;
    App& operator=(const App&) = delete;

    App(App&&) = default;
    App& operator=(App&&) = default;

    explicit App(std::string&& name, std::string&& author,
        std::optional<std::string>&& description = std::nullopt)
        : name(std::move(name))
        , author(std::move(author))
        , description(std::move(description)) {
    }

    [[nodiscard]] static constexpr AppBuilder builder(std::string&& name,
        std::string&& author,
        std::optional<std::string>&& description = std::nullopt) noexcept;

    constexpr void display_help() const {
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

    // return terms that are not options and doesn't attach to any option
    [[nodiscard]] std::vector<std::string_view> parse(
        Session& session, std::vector<std::string_view> args) const;
};

class AppBuilder {
private:
    App _app;

public:
    explicit AppBuilder(const AppBuilder&) = delete;
    AppBuilder& operator=(const AppBuilder&) = delete;

    explicit constexpr AppBuilder(std::string&& name,
        std::string&& author,
        std::optional<std::string>&& description = std::nullopt) noexcept
        : _app(std::move(name), std::move(author), std::move(description)) {
    }

    [[nodiscard]] AppBuilder& enable_help() {
        if (!_app.options.empty())
            Warn("Help option should be the first option");

        // the help option is specially handled
        _app.options.emplace_back(true, "help", false, std::nullopt,
            "Display this help message", nullptr);
        return *this;
    }

    [[nodiscard]] AppBuilder& add_option(std::string&& long_name,
        bool short_name, bool accepts_value = false,
        std::optional<std::string>&& default_value = std::nullopt,
        std::optional<std::string>&& description = std::nullopt,
        std::function<void(Session&, std::string_view)> callback = nullptr) {
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
        _app.options.emplace_back(short_name, std::move(long_name),
            accepts_value, std::move(default_value), std::move(description),
            std::move(callback));
        return *this;
    }

    [[nodiscard]] constexpr App build() noexcept {
        return std::move(_app);
    }
};

constexpr AppBuilder App::builder(std::string&& name, std::string&& author,
    std::optional<std::string>&& description) noexcept {
    return AppBuilder(
        std::move(name), std::move(author), std::move(description));
}

export class HelpMessageDisplayedException : public std::exception { };
} // namespace lpc