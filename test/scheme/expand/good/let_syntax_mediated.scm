; %lpc --stop-after expand --print expand %s
(let-syntax
  ((m1 (syntax-rules ()
         ((m1) (m2))))
   (m2 (syntax-rules ()
         ((m2) (m1)))))
  (m1))
