module lpc.frontend.expand;

namespace lpc::frontend {

class Transformer {
private:
    struct SyntaxRule {
        NodeLocRef pattern;
        NodeLocRef template_;
    };
public:
    Transformer() {
    }
};
} // namespace lpc::frontend
