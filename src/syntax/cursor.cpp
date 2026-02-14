module lpc.syntax.cursor;

import std;

import lpc.syntax.ast;
import lpc.utils.logging;

namespace lpc::syntax {

SpanRef Cursor::get_ident() noexcept {
    if (type() != TokenType::IDENT)
        return SpanRef::invalid();
    std::string name = *value().get_unchecked<std::string>();
    return arena().get_ident(loc(), name);
}

SpanRef Cursor::get_constant() noexcept {
    SpanRef ref;
    switch (type()) {
    case TokenType::NUMBER: {
        LispNumber v = *value().get_unchecked<LispNumber>();
        ref = arena().from_loc(loc(), v);
        break;
    }
    case TokenType::BOOLEAN: {
        bool v = *value().get_unchecked<LispBool>();
        ref = arena().get_bool(loc(), v);
        break;
    }
    case TokenType::CHARACTER: {
        char v = *value().get_unchecked<LispChar>();
        ref = arena().from_loc(loc(), v);
        break;
    }
    case TokenType::STRING: {
        auto v = *value().get_unchecked<LispString>();
        ref = arena().from_loc(loc(), std::move(v));
        break;
    }
    default:
        break;
    }
    return ref;
}

} // namespace lpc::syntax
