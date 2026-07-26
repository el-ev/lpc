; %lpc %s

;; Negative Test: Nested define-syntax
((lambda () (define-syntax bug (syntax-rules () ((_) #f)))))
