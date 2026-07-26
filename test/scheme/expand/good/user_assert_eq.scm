; %lpc --stop-after expand --print expand %s
(define-syntax assert-eq
  (syntax-rules ()
    ((assert-eq actual expected)
     (let ((a actual) (e expected))
       (if (= a e) #t #f)))))
(assert-eq (+ 1 2) 3)
