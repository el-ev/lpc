; %lpc --no-core --print expand %s
(define-syntax dup (syntax-rules () ((_ x x) "ok")))
