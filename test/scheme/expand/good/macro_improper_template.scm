; %lpc %s
(define-syntax mklist
  (syntax-rules ()
    ((_ . args) (list . args))))

(mklist 1 2)
