; %lpc --stop-after expand --print expand %s
(define-syntax my-list
  (syntax-rules ()
    ((my-list) (quote ()))
    ((my-list a rest ...)
     (cons a (my-list rest ...)))))
(my-list 1 2 3)
