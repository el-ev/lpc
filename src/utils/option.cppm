module;

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

    constexpr void display_help() const;

    void parse(Session& session, std::vector<std::string_view> args) const;
};

class AppBuilder {
private:
    App _app;

public:
    explicit AppBuilder(const AppBuilder&) = delete;
    AppBuilder& operator=(const AppBuilder&) = delete;

    explicit constexpr AppBuilder(std::string&& name, std::string&& author,
        std::optional<std::string>&& description = std::nullopt) noexcept
        : _app(std::move(name), std::move(author), std::move(description)) {
    }

    [[nodiscard]] AppBuilder& enable_help();

    [[nodiscard]] AppBuilder& add_option(std::string&& long_name,
        bool short_name, bool accepts_value = false,
        std::optional<std::string>&& default_value = std::nullopt,
        std::optional<std::string>&& description = std::nullopt,
        std::function<void(Session&, std::string_view)> callback = nullptr);

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