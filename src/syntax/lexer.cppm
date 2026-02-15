export module lpc.syntax.lexer;

import std;

import lpc.context;
import lpc.passes;
import lpc.syntax.arenas;
import lpc.syntax.token;

namespace lpc::syntax {


export class Lexer {
private:
    std::string_view _source;
    std::string_view _cursor;
    std::size_t _line = 1;
    std::string_view::iterator _line_start;
    std::vector<Token> _tokens;
    LocationArena& _loc_arena;
    std::uint32_t _file_idx;

    bool _failed = false;

public:
    explicit Lexer(LocationArena& loc_arena, std::string_view file,
        std::string_view source) noexcept
        : _source(source)
        , _cursor(source)
        , _line_start(source.begin())
        , _loc_arena(loc_arena)
        , _file_idx(_loc_arena.add_file(std::string(file))) {
        while (!is_eof() && !_failed)
            if (!advance() && !is_eof())
                _failed = true;
        if (_failed)
            return;
        _tokens.emplace_back(Token::eof(loc("<EOF>")));
    }

    [[nodiscard]] inline std::vector<Token>&& tokens() noexcept {
        return std::move(_tokens);
    }

    [[nodiscard]] inline bool is_eof() const noexcept {
        return _cursor.empty();
    }

    [[nodiscard]] inline bool is_failed() const noexcept {
        return _failed;
    }

private:
    [[nodiscard]] inline constexpr std::uint32_t file_idx() const noexcept {
        return _file_idx;
    }

    [[nodiscard]] inline constexpr LocRef loc(std::string&& lexeme) noexcept {
        return _loc_arena.emplace(_file_idx, _line,
            (_line == 1 ? 1 : 0) + std::distance(_line_start, _cursor.begin()),
            std::move(lexeme));
    }

    [[nodiscard]] inline constexpr std::string loc_string() noexcept {
        return std::format("{}:{}:{}", _loc_arena.file(_file_idx), _line,
            (_line == 1 ? 1 : 0) + std::distance(_line_start, _cursor.begin()));
    }

    [[nodiscard]] inline constexpr std::string loc_string(LocRef ref) noexcept {
        return _loc_arena.at(ref).source_location();
    }

    bool skip_atmosphere() noexcept;
    bool skip_comment() noexcept;
    bool skip_whitespaces() noexcept;

    [[nodiscard]] bool advance() noexcept;

    [[nodiscard]] bool read_ident() noexcept;
    [[nodiscard]] bool read_sharp() noexcept;
    [[nodiscard]] bool read_number(int radix = 0) noexcept;
    [[nodiscard]] bool read_boolean() noexcept;
    [[nodiscard]] bool read_character() noexcept;
    [[nodiscard]] bool read_string() noexcept;
    [[nodiscard]] bool read_operator() noexcept;
};

export class LexPass final : public Pass<std::monostate, std::vector<Token>> {
private:
    bool _failed = false;

public:
    [[nodiscard]] std::string name() const noexcept final {
        return "lex";
    }

    [[nodiscard]] std::vector<Token> run(
        std::monostate, CompilerContext& ctx) noexcept final;

    [[nodiscard]] std::string dump(
        const std::vector<Token>& result, CompilerContext& ctx) const noexcept final;

    [[nodiscard]] bool is_failed() const noexcept final {
        return _failed;
    }

    explicit LexPass() noexcept = default;
};

} // namespace lpc::syntax
