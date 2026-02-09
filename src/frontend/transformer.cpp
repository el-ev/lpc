module lpc.frontend.expand;

namespace lpc::frontend {

class Transformer {
private:
    struct SyntaxRule {
        SExprLocRef pattern;
        SExprLocRef template_;
    };

public:
    Transformer() = default;
};

} // namespace lpc::frontend
