; %lpc %s
(define vf (make-vector 3 0))

(__print (vector 1 2 3))
(__print (vector))
(__print (make-vector 2))
(__print (vector-length '#(a b c)))
(__print (vector-ref '#(a b c) 1))
(vector-fill! vf 7)
(__print vf)
(__print (vector->list '#(1 2 3)))
(__print (list->vector '(1 2 3)))
(__print (vector? '#(1)))
(__print (vector? '(1)))
