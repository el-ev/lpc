export module lpc.frontend.builtin;

import std;
import lpc.frontend.ast;

namespace lpc::frontend {

// Built-in functions

constexpr std::string BUILTIN_PREFIX = "_lpc_bi_";

export class BuiltinFunctions;

class BuiltinFunction {
private:
    std::string _name;
    std::size_t _arity;

public:
    [[nodiscard]] explicit constexpr BuiltinFunction(
        std::string_view name, std::size_t arity)
        : _name(BUILTIN_PREFIX + name)
        , _arity(arity) {
    }

    [[nodiscard]] constexpr std::string_view name() const noexcept {
        return _name;
    }

    [[nodiscard]] constexpr std::size_t arity() const noexcept {
        return _arity;
    }

    [[nodiscard]] NodeLocRef get_ident(
        NodeArena& arena, LocRef loc) const noexcept {
        return arena.get_variable(loc, std::string(_name));
    }

    template <typename... Args>
    NodeLocRef get_call(
        NodeArena& arena, LocRef loc, Args... args) const noexcept {
        NodeLocRef ident = get_ident(arena, loc);
        if (!ident.is_valid())
            return NodeLocRef::invalid();
        return arena.emplace(loc, NodeType::ProcedureCall,
            NodeList { get_ident(arena, loc), std::forward<Args...>(args...) });
    }

    // FIXME: Arity not respected
    NodeLocRef get_call(
        NodeArena& arena, LocRef loc, NodeList args) const noexcept {
        NodeLocRef ident = get_ident(arena, loc);
        if (!ident.is_valid())
            return NodeLocRef::invalid();
        NodeList children;
        children.reserve(args.size() + 1);
        children.push_back(ident);
        std::ranges::copy(args, std::back_inserter(children));
        return arena.emplace(loc, NodeType::ProcedureCall, std::move(children));
    }
};

export class BuiltinFunctions {
private:
    std::unordered_map<std::string, BuiltinFunction> _functions;

public:
    BuiltinFunctions() {
        _functions.emplace("+", BuiltinFunction("add", 1));
        _functions.emplace("-", BuiltinFunction("sub", 1));
        _functions.emplace("=", BuiltinFunction("equal", 1));
        _functions.emplace("display", BuiltinFunction("display", 1));
        _functions.emplace("alloc", BuiltinFunction("alloc", 1));
    }

    [[nodiscard]] const BuiltinFunction* get(
        std::string_view name) const noexcept {
        auto it = _functions.find(std::string(name));
        if (it != _functions.end()) {
            return &it->second;
        }
        return nullptr;
    }
};

} // namespace lpc::frontend