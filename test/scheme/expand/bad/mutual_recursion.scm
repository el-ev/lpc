; %lpc --print expand %s
(define-syntax a (syntax-rules () ((_) (b))))
(define-syntax b (syntax-rules () ((_) (a))))
(a)
