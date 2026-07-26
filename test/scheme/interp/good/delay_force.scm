; %lpc %s
(define dp (delay (+ 1 2)))
(define dcount 0)
(define dp2 (delay (begin (set! dcount (+ dcount 1)) dcount)))

(__print (force dp))
(force dp2)
(force dp2)
(__print dcount)
(__print (force (delay (delay 5))))
