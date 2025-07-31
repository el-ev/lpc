module lpc.frontend.syntax;

import lpc.frontend.combinators;
import lpc.utils.logging;

namespace lpc::frontend {

using lpc::utils::Error;

// clang-format off
namespace rules {

using namespace lpc::frontend::combinators;

struct Datum;

struct List {
static constexpr auto rule() noexcept {
  return 
chain(
    OneToken<TokenType::LPAREN>()
  , any(
        chain(
            Some<Def<Datum>>()
          , any(
                chain(
                    OneToken<TokenType::DOT>()
                  , Def<List>()
                )
              , chain(
                    OneToken<TokenType::DOT>()
                  , Def<Datum>()
                )
              , make_node<NodeType::Nil>(Succeed())
            )
        )
      , make_node<NodeType::Nil>(Succeed())
    )
  , OneToken<TokenType::RPAREN>()
);
}
};

struct Datum {
static constexpr auto rule() noexcept {
return
any(
    GetConstant()
  , GetIdentifier()
  , make_node<NodeType::List>(Def<List>())
  , make_node<NodeType::Vector>(
        chain(
            OneToken<TokenType::SHELL_LPAREN>()
          , Many<Def<Datum>>()
          , OneToken<TokenType::RPAREN>()
        )
    )
  , make_node<NodeType::List>(
        chain(
            any(
                chain(
                    OneToken<TokenType::APOSTROPHE>()
                  , InsertKeyword<Keyword::QUOTE>()
                )
              , chain(
                    OneToken<TokenType::BACKTICK>()
                  , InsertKeyword<Keyword::QUASIQUOTE>()
                )
              , chain(
                    OneToken<TokenType::COMMA>()
                  , InsertKeyword<Keyword::UNQUOTE>()
                )
              , chain(
                    OneToken<TokenType::COMMA_AT>()
                  , InsertKeyword<Keyword::UNQUOTE_SPLICING>()
                )
            )
          , Def<Datum>()
          , make_node<NodeType::Nil>(Succeed())
        )
    )
);
}
};

struct Program {
static constexpr auto rule() noexcept {
    return make_node<NodeType::Program>(
        Many(Def<Datum>())
    );
}
};

}
// clang-format on

void Parser::parse() noexcept {
    auto program = rules::Program::rule()(_cursor);
    if (_cursor.is_failed())
        return;
    if (!_cursor.is_eof()) {
        Error("Unexpected tokens after parsing the root node. Next token: "
              "\"{}\" at {}",
            _arena.location(_cursor.loc()).lexeme(),
            _arena.location(_cursor.loc()).source_location());
        _cursor.fail();
        return;
    }
    _root = program.value()[0];
}
}
