module lpc.frontend.syntax;

import lpc.frontend.combinators;
import lpc.utils.logging;

namespace lpc::frontend {

using lpc::utils::Debug;
using lpc::utils::Error;

// clang-format off
namespace rules {

using namespace lpc::frontend::combinators;

struct Datum {
static constexpr auto rule() noexcept {
return
any(
    GetConstant()
  , GetVariable()
  , GetKeyword()
  , make_node<NodeType::List>(
        chain(
            !OneToken<TokenType::LPAREN>()
          , Maybe(
                chain(
                    Some<Def<Datum>>()
                  , Maybe(
                        chain(
                            !OneToken<TokenType::DOT>()
                          , Def<Datum>()
                        )
                    )
                )
            )
          , !OneToken<TokenType::RPAREN>()
        )
    )
  , make_node<NodeType::Vector>(
        chain(
            !OneToken<TokenType::SHELL_LPAREN>()
          , Many<Def<Datum>>()
          , !OneToken<TokenType::RPAREN>()
        )
    )
  , chain(
        !OneToken<TokenType::APOSTROPHE>()
      , make_node<NodeType::Quotation>(
            Def<Datum>()
        )
    )
  , chain(
        !OneToken<TokenType::BACKTICK>()
      , make_node<NodeType::List>(
            chain(
                InsertKeyword<Keyword::QUASIQUOTE>()
              , Def<Datum>()
            )
        )
    )
  , chain(
        !OneToken<TokenType::COMMA>()
      , make_node<NodeType::List>(
            chain(
                InsertKeyword<Keyword::UNQUOTE>()
              , Def<Datum>()
            )
        )
    )
  , chain(
        !OneToken<TokenType::COMMA_AT>()
      , make_node<NodeType::List>(
            chain(
                InsertKeyword<Keyword::UNQUOTE_SPLICING>()
              , Def<Datum>()
            )
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
        Error("Unexpected tokens after parsing the root node");
        _cursor.fail();
        return;
    }
    _root = program.value()[0];
}
}
