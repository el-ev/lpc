; %lpc --stop-after expand --print expand %s
(define-syntax my-if
  (syntax-rules ()
    ((my-if c t f) (cond (c t) (else f)))))
(my-if #t 1 2)
