; %lpc --print expand %s
(define-syntax foo (syntax-rules () ((foo) (define foo 1))))
(foo)