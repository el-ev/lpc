; %lpc --stop-after expand --print expand %s
(define (fact n)
  (if (= n 0)
      1
      (* n (fact (- n 1)))))
