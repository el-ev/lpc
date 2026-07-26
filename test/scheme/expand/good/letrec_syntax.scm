; %lpc --stop-after expand --print expand %s
(letrec-syntax
  ((m (syntax-rules () ((m x) (+ x 1)))))
  (m 42))
