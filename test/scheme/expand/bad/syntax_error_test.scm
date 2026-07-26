; %lpc --print expand %s
(define-syntax check-num
  (syntax-rules ()
    ((_ x) (if (number? x) x (syntax-error "not a number" x)))))

(check-num 1)
(check-num "a")
