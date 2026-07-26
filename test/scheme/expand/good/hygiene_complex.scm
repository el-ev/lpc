; %lpc --stop-after expand --print expand %s
(define-syntax my-let
  (syntax-rules ()
    ((_ ((var val) ...) body ...)
     ((lambda (var ...) body ...) val ...))))

(define x 1)
(my-let ((x 2)) x)

(define-syntax swap!
  (syntax-rules ()
    ((_ a b)
     (let ((tmp a))
       (set! a b)
       (set! b tmp)))))

(let ((tmp 1) (other 2))
  (swap! tmp other)
  (list tmp other))
