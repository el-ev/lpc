; %lpc --no-core --print closure-convert --stop-after closure-convert %s
(define (adder x) (lambda (y) (__fx+ x y)))
(__print (adder 1))
