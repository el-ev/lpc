; %lpc %s
(define pr (cons 1 2))
(define pr2 (list 1 2 3))
(define pr3 pr2)

(set-car! pr 10)
(set-cdr! pr 20)
(__print pr)
(set-car! (cdr pr2) 99)
(__print pr3)
(set-cdr! (cdr pr2) '())
(__print pr2)
(set-car! pr (list 'nested))
(__print pr)
