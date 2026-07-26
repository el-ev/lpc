; %lpc --stop-after expand --print expand %s
(define-syntax swap!
  (syntax-rules ()
    ((swap! a b)
     (let ((tmp a))
       (set! a b)
       (set! b tmp)))))
(swap! x y)
