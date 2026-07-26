; %lpc %s
(define (classify x)
  (case x
    ((0) 'zero)
    ((1 3 5 7 9) 'odd)
    ((2 4 6 8) => (lambda (d) (list 'even d)))
    (else 'big)))

(__print (classify 0))
(__print (classify 5))
(__print (classify 4))
(__print (classify 100))
(__print (case 1 ((1) 'one)))
(__print (case 2 ((1) 'one)))
(__print (case 'b ((a) 1) ((b c) 2) (else 3)))
(__print (case 'x ((a) 1) (else => (lambda (k) (list 'fallback k)))))
