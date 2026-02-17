;static constexpr std::string_view CORE_SOURCE = R"CORE(
; Scheme uses ; for comments, that's good.

;; Builtin macros

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
      (let ((v1 (__void)) ...)
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

;; Builtin functions

;; Wrappers for primitives
(define (cons a b) (__cons a b))
(define (car x) (__car x))
(define (cdr x) (__cdr x))
(define (eq? a b) (__eq? a b))
(define (pair? x) (__pair? x))
(define (symbol? x) (__symbol? x))
(define (vector? x) (__vector? x))
(define (vector-ref v i) (__vector-ref v i))
(define (vector-set! v i obj) (__vector-set! v i obj))
(define (length l) (__length l))

;; vector as macro for now
(define-syntax vector
  (syntax-rules ()
    ((_ . args) (__vector . args))))

(define (void) (__void)) ; undefined value, also available as (if #f #f)
(define (null? x) (eq? x '()))

(define (boolean? x) (or (eq? x #t) (eq? x #f))) 
(define (list . args) args)
(define (not x) (eq? x #f))

(define (equal? a b)
  (cond ((eq? a b) #t)
        ((and (pair? a) (pair? b))
         (and (equal? (car a) (car b))
              (equal? (cdr a) (cdr b))))
        (else #f)))

(define (__fold-left f acc lst)
(if (null? lst)
    acc
    (__fold-left f 
                  (f acc (car lst))
                  (cdr lst))))

(define (__fold-right f acc lst)
(if (null? lst)
    acc
    (f (car lst) (__fold-right f acc (cdr lst)))))

(define (__chain-cmp pred lst)
  (if (null? lst) #t
      (if (null? (cdr lst)) #t
          (if (pred (car lst) (car (cdr lst)))
              (__chain-cmp pred (cdr lst))
              #f))))

(define (fx+ a b) (__fx+ a b))
(define (fx- a b) (__fx- a b))
(define (fx* a b) (__fx* a b))
(define (fx/ a b) (__fx/ a b))
(define (fx< a b) (__fx< a b))
(define (fx<= a b) (__fx<= a b))
(define (fx> a b) (__fx< b a))
(define (fx>= a b) (__fx<= b a))
(define (fx= a b) (__fx= a b))

; TODO When other numeric types are supported...
(define (+ . args) (__fold-left fx+ 0 args))
(define (- . args)
  (if (null? args)
      (void)
      (if (null? (cdr args))
          (fx- 0 (car args))
          (__fold-left fx- (car args) (cdr args)))))
(define (* . args) (__fold-left fx* 1 args))
(define (/ . args)
  (if (null? args)
      (void)
      (if (null? (cdr args))
          (fx/ 1 (car args))
          (__fold-left fx/ (car args) (cdr args)))))
(define (< . args) (__chain-cmp fx< args))
(define (<= . args) (__chain-cmp fx<= args))
(define (= . args) (__chain-cmp fx= args))
(define (> . args) (__chain-cmp fx> args))
(define (>= . args) (__chain-cmp fx>= args))

(define (zero? x) (= x 0))

(define (__append-2 l1 l2)
  (if (null? l1) l2
      (cons (car l1) (__append-2 (cdr l1) l2))))

(define (append . lists)
  (__fold-right __append-2 '() lists))

(define (map proc lst)
  (if (null? lst) '()
      (cons (proc (car lst)) (map proc (cdr lst)))))

(define (for-each proc lst)
  (if (null? lst) (void)
      (begin (proc (car lst))
             (for-each proc (cdr lst)))))

(define (reverse lst)
  (letrec ((loop (lambda (l acc)
                   (if (null? l) acc
                       (loop (cdr l) (cons (car l) acc))))))
    (loop lst '())))

(define (list-tail lst k)
  (if (zero? k) lst
      (list-tail (cdr lst) (- k 1))))

(define (list-ref lst k)
  (car (list-tail lst k)))

(define (memq item lst)
  (cond ((null? lst) #f)
        ((eq? item (car lst)) lst)
        (else (memq item (cdr lst)))))

(define (member item lst)
  (cond ((null? lst) #f)
        ((equal? item (car lst)) lst)
        (else (member item (cdr lst)))))

(define (abs x)
  (if (< x 0) (- x) x))

(define (__max2 a b) (if (> a b) a b))
(define (max . args) (__fold-left __max2 (car args) (cdr args)))

(define (__min2 a b) (if (< a b) a b))
(define (min . args) (__fold-left __min2 (car args) (cdr args)))

;)CORE";