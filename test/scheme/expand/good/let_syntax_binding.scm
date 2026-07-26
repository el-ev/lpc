; %lpc --stop-after expand --print expand %s
(let-syntax 
((capture-test 
   (syntax-rules ()
     ((_ val expr)
      (let ((temp val))
        expr)))))

(let ((temp 'outer))
  (capture-test 'inner temp)))

(define-syntax my-or
  (syntax-rules ()
    ((my-or) #f)
    ((my-or e) e)
    ((my-or e1 e2 ...)
     (let ((temp e1))
       (if temp
           temp
           (my-or e2 ...))))))
       
(my-or #f 1 2 3)
