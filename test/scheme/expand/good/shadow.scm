; %lpc --stop-after expand --print expand %s
(define if 1)
(define lambda 2)
if
lambda
(let-syntax ((foo (syntax-rules () ((foo x) (define x 1)))))
  (let ((y 2))
    (foo a)
    (define foo 3)
    (foo b)))
