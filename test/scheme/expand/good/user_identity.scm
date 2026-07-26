; %lpc --stop-after expand --print expand %s
(define-syntax one-arg (syntax-rules () ((_ x) x)))
(one-arg 42)
