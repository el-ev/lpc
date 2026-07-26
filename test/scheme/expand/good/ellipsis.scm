; %lpc --stop-after expand --print expand %s
(define-syntax do-nothing
  (syntax-rules ()
    ((do-nothing x ...) (begin x ...))))
(do-nothing)
(do-nothing 1 2 3)
(define-syntax pair-map
  (syntax-rules ()
    ((pair-map f (a b) ...) (begin (f a b) ...))))
(pair-map + (1 2) (3 4) (5 6))
