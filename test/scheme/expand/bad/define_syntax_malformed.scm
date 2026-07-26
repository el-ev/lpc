; %lpc %s

;; Negative Test: Malformed define-syntax forms

(define-syntax) ;; failure
(define-syntax foo) ;; failure
(define-syntax foo ()) ;; failure
(define-syntax foo (bar)) ;; failure
(define-syntax foo (syntax-rules)) ;; failure
