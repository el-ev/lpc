module lpc.frontend.parser;

import std;
import lpc.logging;
import lpc.frontend.token;
import lpc.frontend.ast;

namespace lpc::frontend {

using namespace lpc::frontend::combinators;

template <>
OptNodePtr ParserImpl::parse_impl<NodeType::Program>() noexcept {
    return std::make_unique<Node>(NodeType::Program,
        Location(_tokens.front().location().file(),
            _tokens.front().location().line(),
            _tokens.front().location().column()));
}

void ParserImpl::run() noexcept {
    _root = parse_impl<NodeType::Program>().value();
}

} // namespace lpc::frontend