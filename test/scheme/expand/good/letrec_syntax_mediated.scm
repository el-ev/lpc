; %lpc --stop-after expand --print expand %s
(letrec-syntax
  ((m1 (syntax-rules ()
         ((m1) (m2))))
   (m2 (syntax-rules ()
         ((m2) (quote success)))))
  (m1))
