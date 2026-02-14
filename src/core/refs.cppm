export module lpc.core.refs;

import std;

import lpc.utils.arena;

namespace lpc::core {

using lpc::utils::ElementReference;

export using ScopeID = std::uint32_t;

export struct LocTag { };
export struct SExprTag { };
export struct SpanTag { };
export struct ScopeSetTag { };

export using LocRef = ElementReference<LocTag, std::uint32_t>;
export using SExprRef = ElementReference<SExprTag, std::uint32_t>;
export using SpanRef = ElementReference<SpanTag, std::uint32_t>;
export using ScopeSetRef = ElementReference<ScopeSetTag, std::uint32_t>;

} // namespace lpc::core
