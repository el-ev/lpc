; %lpc --print expand --stop-after expand %s
(define-syntax m
  (syntax-rules (if)
    ((_ if) "lit")
    ((_ x) "other")))
((lambda (if) (m if)) 1)
(m if)
