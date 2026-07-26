; %lpc --stop-after expand --print expand %s
(define-syntax apply-twice
  (syntax-rules ()
    ((apply-twice f x) (f (f x)))))
(apply-twice add1 0)
