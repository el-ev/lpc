; %lpc --stop-after expand --print expand %s
(define-syntax twice (syntax-rules () ((_ x) (begin x x))))
(twice (display "hi"))
