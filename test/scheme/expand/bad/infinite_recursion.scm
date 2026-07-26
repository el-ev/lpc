; %lpc --print expand %s
(define-syntax m (syntax-rules () ((_ x) (m x)))) (m 1)
