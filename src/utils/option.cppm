export module lpc.utils.option;

import std;

import lpc.utils.logging;

namespace lpc::utils {

class AppBuilder;

export struct Option {
    char short_name;
    bool accepts_value;
    std::string long_name;
    std::string description;
    std::string default_value;
    std::function<void(std::string_view)> callback;

    explicit Option(char short_name, std::string long_name,
        std::string description = "", bool accepts_value = false,
        std::string default_value = "",

        std::function<void(std::string_view)> callback = nullptr)
        : short_name(short_name)
        , accepts_value(accepts_value)
        , long_name(std::move(long_name))
        , description(std::move(description))
        , default_value(std::move(default_value))
        , callback(std::move(callback)) {
    }

    explicit Option(const Option&) = delete;
    Option& operator=(const Option&) = delete;

    Option(Option&&) = default;
    Option& operator=(Option&&) = default;
};

export const char NO_SHORT_NAME = 0;
export const std::string NO_DEFAULT_VALUE;

export class App {
private:
    std::string _name;
    std::string _author;
    std::string _description;
    std::vector<Option> _options;
    std::function<void(std::vector<std::string_view>&&)> _non_option_callback;

    bool _help_enabled = false;

public:
    explicit App(std::string name, std::string author,
        std::string description = "") noexcept
        : _name(std::move(name))
        , _author(std::move(author))
        , _description(std::move(description)) {
    }

    explicit App(const App&) = delete;
    App& operator=(const App&) = delete;

    App(App&&) = default;
    App& operator=(App&&) = default;

    [[nodiscard]] static AppBuilder builder(std::string name,
        std::string author, std::string description = "") noexcept;

    void enable_help() noexcept;

    void display_help() const noexcept;

    void parse(std::vector<std::string_view> args) noexcept;

private:
    void add_option(Option&& option) noexcept;

    friend class AppBuilder;
};

class AppBuilder {
private:
    App _app;

public:
    explicit AppBuilder(std::string name, std::string author,
        std::string description = "") noexcept
        : _app(std::move(name), std::move(author), std::move(description)) {
    }

    explicit AppBuilder(const AppBuilder&) = delete;
    AppBuilder& operator=(const AppBuilder&) = delete;

    AppBuilder(AppBuilder&&) = default;
    AppBuilder& operator=(AppBuilder&&) = default;

    [[nodiscard]] AppBuilder&& enable_help() noexcept {
        _app.enable_help();
        return std::move(*this);
    }

    [[nodiscard]] AppBuilder&& add_option(std::string long_name,
        char short_name, std::string description = "",
        std::function<void(std::string_view)> callback = nullptr) noexcept {
        _app.add_option(
            Option { short_name, std::move(long_name), std::move(description),
                false, "", std::move(callback) });
        return std::move(*this);
    }

    [[nodiscard]] AppBuilder&& add_option(std::string long_name,
        char short_name, std::string description = "",
        std::string default_value = "",
        std::function<void(std::string_view)> callback = nullptr) noexcept {
        _app.add_option(
            Option { short_name, std::move(long_name), std::move(description),
                true, std::move(default_value), std::move(callback) });
        return std::move(*this);
    }

    [[nodiscard]] AppBuilder&& set_non_option_callback(
        std::function<void(std::vector<std::string_view>&&)>
            callback) noexcept {
        _app._non_option_callback = std::move(callback);
        return std::move(*this);
    }

    [[nodiscard]] App&& build() {
        return std::move(_app);
    }
};

AppBuilder App::builder(
    std::string name, std::string author, std::string description) noexcept {
    return AppBuilder(
        std::move(name), std::move(author), std::move(description));
}

} // namespace lpc::utils
