; %lpc --stop-after expand --print expand %s
(let-syntax ((g (syntax-rules () ((_ x) (f x))))) (g 1))
