; %lpc --stop-after expand --print expand %s
(let-syntax
  ((my-or (syntax-rules ()
            ((my-or) #f)
            ((my-or e) e)
            ((my-or e1 e2 ...)
             (if e1 e1 (my-or e2 ...))))))
  (my-or #f #t 7))
