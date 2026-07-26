; %lpc --stop-after expand --print expand %s
(define-syntax my-let
  (syntax-rules ()
    ((my-let ((x v) ...) body)
     ((lambda (x ...) body) v ...))))
(my-let ((a 1) (b 2)) (+ a b))
