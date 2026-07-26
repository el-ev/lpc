; %lpc --no-core --print sema --stop-after sema %s
(define f (lambda (x) x))
(define g y)
(define y 1)
g
