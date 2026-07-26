; %lpc %s
(define acc '())

(__print (map (lambda (x) (* x x)) '(1 2 3)))
(__print (map + '(1 2 3) '(10 20 30)))
(__print (map cons '(a b) '(1 2 3)))
(__print (map list '(1 2) '(a b) '(x y)))
(__print (map + '(1 2 3) '(10 20)))
(for-each (lambda (x) (set! acc (cons x acc))) '(1 2 3))
(__print (reverse acc))
(for-each (lambda (a b) (display a) (display b) (newline)) '(1 2) '(a b))
