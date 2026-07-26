; %lpc --stop-after expand --print expand %s
(define-syntax my-if
  (syntax-rules ()
    ((my-if test then else)
     (cond (test then)
           (else else)))))
(my-if (> x 0) "pos" "neg")
