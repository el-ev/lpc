;static constexpr std::string_view CORE_SOURCE = R"CORE(
; Scheme uses ; for comments, that's good.
(define-syntax and
  (syntax-rules ()
    ((and) #t)
    ((and test) test)
    ((and test1 test2 ...)
     (if test1 (and test2 ...) #f))))

(define-syntax or
  (syntax-rules ()
    ((or) #f)
    ((or test) test)
    ((or test1 test2 ...)
     (let ((x test1))
       (if x x (or test2 ...))))))

(define-syntax when
  (syntax-rules ()
    ((when test body ...)
     (if test (begin body ...)))))

(define-syntax unless
  (syntax-rules ()
    ((unless test body ...)
     (if test (void) (begin body ...)))))

(define-syntax let
  (syntax-rules ()
    ((let ((name val) ...) body1 body2 ...)
     ((lambda (name ...) body1 body2 ...)
      val ...))))

(define-syntax let*
  (syntax-rules ()
    ((let* () body1 body2 ...)
     (let () body1 body2 ...))
    ((let* ((name1 val1) rest ...) body1 body2 ...)
     (let ((name1 val1))
       (let* (rest ...) body1 body2 ...)))))

(define-syntax __undefined
  (syntax-rules ()
    ((__undefined)
     (if #f #f))))

(define-syntax letrec
  (syntax-rules ()
    ((_ ((v1 e1) ...) body ...)
     (letrec "gen"
        (v1 ...)
        ()
        ((v1 e1) ...)
        body ...))
  
    ((_ "gen"
        ()
        (t1 ...)
        ((v1 e1) ...)
        body ...)
      (let ((v1 (__undefined)) ...)
        (let ((t1 e1) ...)
          (set! v1 t1)
          ...
          (let () 
            body ...))))

  ((_ "gen"
      (x y ...)
      (t ...)
      ((v1 e1) ...)
      body ...)
   (letrec "gen"
      (y ...)
      (nt t ...)
      ((v1 e1) ...)
      body ...))))

(define-syntax cond
  (syntax-rules (else)
    ((cond (else result1 result2 ...))
     (begin result1 result2 ...))
    ((cond (test result1 result2 ...))
     (if test (begin result1 result2 ...)))
    ((cond (test result1 result2 ...) clause1 clause2 ...)
     (if test
         (begin result1 result2 ...)
         (cond clause1 clause2 ...)))))

(define-syntax quasiquote
  (syntax-rules 
    (unquote unquote-splicing)
    ((_ x) 
     (__qq-expand () x))))

(define-syntax __qq-expand
  (syntax-rules
    (unquote unquote-splicing quasiquote)
    ((_ () (unquote x))
     x)
    
    ((_ () (unquote-splicing x))
     (syntax-error "unquote-splicing at level 0"))

    ((_ () (quasiquote x))
     (list 'quasiquote (__qq-expand (x) x)))
    
    ((_ (x . d) (unquote e))
     (list 'unquote (__qq-expand d e)))
    
    ((_ (x . d) (quasiquote e))
     (list 'quasiquote (__qq-expand (x x . d) e)))

    ((_ d #(e ...))
     (list->vector (__qq-list d (e ...))))

    ((_ d (e . f))
     (__qq-list d (e . f)))

    ((_ d atom)
     'atom)))

(define-syntax __qq-list
  (syntax-rules (unquote unquote-splicing quasiquote)
    ((_ () ((unquote-splicing x) . rest))
     (append x (__qq-list () rest)))
    
    ((_ (x . d) ((unquote-splicing e) . rest))
     (cons (list 'unquote-splicing (__qq-expand d e))
           (__qq-list (x . d) rest)))
    
    ((_ () (unquote e))
     e)
    
    ((_ (x . d) (unquote e))
     (list 'unquote (__qq-expand d e)))
    
    ((_ d (head . tail))
     (cons (__qq-expand d head) (__qq-list d tail)))
    
    ((_ d atom)
     'atom)))

(define-syntax unquote
  (syntax-rules
    ()
    ((_ x)
     (syntax-error "unquote outside of quasiquote"))))

(define-syntax unquote-splicing
    (syntax-rules
      ()
      ((_ x)
       (syntax-error "unquote-splicing outside of quasiquote"))))

(define-syntax delay
  (syntax-rules ()
    ((delay expr)
     (__memo (lambda () expr)))))

; (define (force promise)
;   (promise))

; (define (__memo proc)
;   (let ((run_once? #f)
;         (result #f))
;     (lambda ()
;       (if (not run_once?)
;           (begin (set! result (proc))
;                  (set! run_once? #t)
;                  result)
;           result))))

;)CORE";