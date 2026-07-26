; %lpc --stop-after expand --print expand %s
(define-syntax my-begin
  (syntax-rules ()
    ((my-begin) (void))
    ((my-begin e) e)
    ((my-begin e1 e2 ...)
     (let ((t e1))
       (my-begin e2 ...)))))
(my-begin 1 2 3)
