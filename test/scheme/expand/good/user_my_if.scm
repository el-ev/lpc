; %lpc --stop-after expand --print expand %s
(define-syntax my-if
  (syntax-rules ()
    ((_ t a b) (if t a b))))
(my-if #t 1 2)
