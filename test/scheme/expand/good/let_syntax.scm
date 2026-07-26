; %lpc --stop-after expand --print expand %s
(let-syntax ((m (syntax-rules () ((m x) (+ x 1)))))
  (m 42))
