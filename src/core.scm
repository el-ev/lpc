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
      val ...))
    ; named let
    ((let tag ((name val) ...) body1 body2 ...)
     ((letrec ((tag (lambda (name ...) body1 body2 ...)))
              tag)
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
  (syntax-rules (else =>)
    ((cond (else result1 result2 ...))
     (begin result1 result2 ...))
    ((cond (test => result))
     (let ((t test))
       (if t (result t) (if #f #f))))
    ((cond (test => result) clause1 clause2 ...)
     (let ((t test))
       (if t (result t)
           (cond clause1 clause2 ...))))
    ((cond (test))
     test)
    ((cond (test) clause1 clause2 ...)
     (let ((t test))
       (if t t (cond clause1 clause2 ...))))
    ((cond (test result1 result2 ...))
     (if test (begin result1 result2 ...)))
    ((cond (test result1 result2 ...) clause1 clause2 ...)
     (if test
         (begin result1 result2 ...)
         (cond clause1 clause2 ...)))))

(define-syntax case
  (syntax-rules (else =>)
    ((case key)
     (begin key (void)))
    ((case key (else => result))
     (result key))
    ((case key (else result1 result2 ...))
     (begin key result1 result2 ...))
    ((case key ((datum ...) => result) clause ...)
     (let ((k key))
       (if (memv k '(datum ...))
           (result k)
           (case k clause ...))))
    ((case key ((datum ...) result1 result2 ...) clause ...)
     (let ((k key))
       (if (memv k '(datum ...))
           (begin result1 result2 ...)
           (case k clause ...))))))

(define-syntax do
  (syntax-rules ()
    ((do ((var init . step) ...) (test expr ...) command ...)
     (letrec ((loop
                (lambda (var ...)
                  (if test
                      (begin (if #f #f) expr ...)
                      (begin
                        command ...
                        (loop (__do-step var step) ...))))))
       (loop init ...)))))

(define-syntax __do-step
  (syntax-rules ()
    ((__do-step var ())
     var)
    ((__do-step var (step))
     step)))

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

(define (__memo proc)
  (let ((run_once? #f)
        (result #f))
    (lambda ()
      (if (not run_once?)
          (begin (set! result (proc))
                 (set! run_once? #t)
                 result)
          result))))

;; Builtin functions

;; Wrappers for primitives
(define (cons a b) (__cons a b))
(define (car x) (__car x))
(define (cdr x) (__cdr x))
(define (eq? a b) (__eq? a b))
(define (eqv? a b) (eq? a b))
(define (pair? x) (__pair? x))
(define (null? x) (__null? x))
(define (boolean? x) (__boolean? x))
(define (symbol? x) (__symbol? x))
(define (vector? x) (__vector? x))
(define (char? x) (__char? x))
(define (string? x) (__string? x))
(define (fixnum? x) (__fixnum? x))
(define (number? x) (fixnum? x))
(define (integer? x) (fixnum? x))
(define (procedure? x) (__procedure? x))
(define (vector-ref v i) (__vector-ref v i))
(define (vector-set! v i obj) (__vector-set! v i obj))
(define (vector-length v) (__length v))
(define (string-length s) (__length s))
(define (make-vector k . fill)
  (if (null? fill)
      (__make-vector k)
      (__make-vector k (car fill))))

(define (vector . args) (list->vector args))

(define (void) (__void)) ; undefined value, also available as (if #f #f)
(define (list . args) args)
(define (list* . args)
  (if (null? args)
      '()
      (letrec ((loop (lambda (xs)
                       (if (null? (cdr xs))
                           (car xs)
                           (cons (car xs) (loop (cdr xs)))))))
        (loop args))))
(define (not x) (eq? x #f))
(define (force promise) (promise))

(define (caar x) (car (car x)))
(define (cadr x) (car (cdr x)))
(define (cdar x) (cdr (car x)))
(define (cddr x) (cdr (cdr x)))

(define (caaar x) (car (caar x)))
(define (caadr x) (car (cadr x)))
(define (cadar x) (car (cdar x)))
(define (caddr x) (car (cddr x)))
(define (cdaar x) (cdr (caar x)))
(define (cdadr x) (cdr (cadr x)))
(define (cddar x) (cdr (cdar x)))
(define (cdddr x) (cdr (cddr x)))

(define (list? x)
  (cond ((null? x) #t)
        ((pair? x) (list? (cdr x)))
        (else #f)))

(define (length x)
  (cond ((null? x) 0)
        ((pair? x) (__fx+ 1 (length (cdr x))))
        (else (__length x))))

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
(define (fx% a b) (__fx% a b))
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
(define (positive? x) (> x 0))
(define (negative? x) (< x 0))

(define (quotient a b) (fx/ a b))
(define (remainder a b) (fx% a b))
(define (modulo a b)
  (let ((r (remainder a b)))
    (if (or (and (< r 0) (> b 0))
            (and (> r 0) (< b 0)))
        (+ r b)
        r)))

(define (odd? x) (not (zero? (remainder x 2))))
(define (even? x) (zero? (remainder x 2)))

(define (__append-2 l1 l2)
  (if (null? l1) l2
      (cons (car l1) (__append-2 (cdr l1) l2))))

(define (append . lists)
  (__fold-right __append-2 '() lists))

(define (__any-null? lists)
  (cond ((null? lists) #f)
        ((null? (car lists)) #t)
        (else (__any-null? (cdr lists)))))

(define (__cars lists)
  (if (null? lists)
      '()
      (cons (car (car lists)) (__cars (cdr lists)))))

(define (__cdrs lists)
  (if (null? lists)
      '()
      (cons (cdr (car lists)) (__cdrs (cdr lists)))))

(define (map proc . lists)
  (if (null? (cdr lists))
      (let loop ((lst (car lists)))
        (if (null? lst)
            '()
            (cons (proc (car lst)) (loop (cdr lst)))))
      (let loop ((lists lists))
        (if (__any-null? lists)
            '()
            (cons (apply proc (__cars lists))
                  (loop (__cdrs lists)))))))

(define (for-each proc . lists)
  (if (null? (cdr lists))
      (let loop ((lst (car lists)))
        (if (null? lst)
            (void)
            (begin (proc (car lst)) (loop (cdr lst)))))
      (let loop ((lists lists))
        (if (__any-null? lists)
            (void)
            (begin (apply proc (__cars lists))
                   (loop (__cdrs lists)))))))

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

(define (list->vector lst)
  (let ((v (make-vector (length lst))))
    (letrec ((fill (lambda (i xs)
                     (if (null? xs)
                         v
                         (begin
                           (vector-set! v i (car xs))
                           (fill (+ i 1) (cdr xs)))))))
      (fill 0 lst))))

(define (vector->list v)
  (letrec ((loop (lambda (i acc)
                   (if (< i 0)
                       acc
                       (loop (- i 1) (cons (vector-ref v i) acc))))))
    (loop (- (vector-length v) 1) '())))

(define (memq item lst)
  (cond ((null? lst) #f)
        ((eq? item (car lst)) lst)
        (else (memq item (cdr lst)))))

(define (memv item lst)
  (cond ((null? lst) #f)
        ((eqv? item (car lst)) lst)
        (else (memv item (cdr lst)))))

(define (member item lst)
  (cond ((null? lst) #f)
        ((equal? item (car lst)) lst)
        (else (member item (cdr lst)))))

(define (assq item alist)
  (cond ((null? alist) #f)
        ((and (pair? (car alist))
              (eq? item (caar alist)))
         (car alist))
        (else (assq item (cdr alist)))))

(define (assv item alist)
  (cond ((null? alist) #f)
        ((and (pair? (car alist))
              (eqv? item (caar alist)))
         (car alist))
        (else (assv item (cdr alist)))))

(define (assoc item alist)
  (cond ((null? alist) #f)
        ((and (pair? (car alist))
              (equal? item (caar alist)))
         (car alist))
        (else (assoc item (cdr alist)))))

(define (abs x)
  (if (< x 0) (- x) x))

(define (__max2 a b) (if (> a b) a b))
(define (max . args) (__fold-left __max2 (car args) (cdr args)))

(define (__min2 a b) (if (< a b) a b))
(define (min . args) (__fold-left __min2 (car args) (cdr args)))

(define (call-with-current-continuation f) (__call/cc f))
(define (call/cc f) (__call/cc f))

(define (__apply-args args)
  (cond ((null? args) '())
        ((null? (cdr args)) (car args))
        (else (cons (car args) (__apply-args (cdr args))))))

(define (apply f . args) (__apply f (__apply-args args)))

;; Pair mutation

(define (set-car! p obj) (__set-car! p obj))
(define (set-cdr! p obj) (__set-cdr! p obj))

;; Output

(define (display obj) (__display obj))
(define (write obj) (__write obj))
(define (newline) (__newline))

;; Characters

(define (char->integer c) (__char->integer c))
(define (integer->char n) (__integer->char n))

(define (char=? . args) (__chain-cmp __char=? args))
(define (char<? . args) (__chain-cmp __char<? args))
(define (__char>? a b) (__char<? b a))
(define (__char<=? a b) (not (__char<? b a)))
(define (__char>=? a b) (not (__char<? a b)))
(define (char>? . args) (__chain-cmp __char>? args))
(define (char<=? . args) (__chain-cmp __char<=? args))
(define (char>=? . args) (__chain-cmp __char>=? args))

(define (char-upcase c)
  (if (and (__char>=? c #\a) (__char<=? c #\z))
      (integer->char (- (char->integer c) 32))
      c))

(define (char-downcase c)
  (if (and (__char>=? c #\A) (__char<=? c #\Z))
      (integer->char (+ (char->integer c) 32))
      c))

(define (__char-ci=? a b) (char=? (char-upcase a) (char-upcase b)))
(define (__char-ci<? a b) (char<? (char-upcase a) (char-upcase b)))
(define (__char-ci>? a b) (__char-ci<? b a))
(define (__char-ci<=? a b) (not (__char-ci<? b a)))
(define (__char-ci>=? a b) (not (__char-ci<? a b)))
(define (char-ci=? . args) (__chain-cmp __char-ci=? args))
(define (char-ci<? . args) (__chain-cmp __char-ci<? args))
(define (char-ci>? . args) (__chain-cmp __char-ci>? args))
(define (char-ci<=? . args) (__chain-cmp __char-ci<=? args))
(define (char-ci>=? . args) (__chain-cmp __char-ci>=? args))

(define (char-alphabetic? c)
  (or (and (__char>=? c #\a) (__char<=? c #\z))
      (and (__char>=? c #\A) (__char<=? c #\Z))))

(define (char-numeric? c)
  (and (__char>=? c #\0) (__char<=? c #\9)))

(define (char-whitespace? c)
  (if (memv (char->integer c) '(9 10 11 12 13 32)) #t #f))

(define (char-upper-case? c)
  (and (__char>=? c #\A) (__char<=? c #\Z)))

(define (char-lower-case? c)
  (and (__char>=? c #\a) (__char<=? c #\z)))

;; Strings

(define (make-string k . fill)
  (if (null? fill)
      (__make-string k #\space)
      (__make-string k (car fill))))

(define (string . chars) (list->string chars))

(define (string-ref s k) (__string-ref s k))
(define (string-set! s k c) (__string-set! s k c))

(define (string=? . args) (__chain-cmp __string=? args))
(define (string<? . args) (__chain-cmp __string<? args))
(define (__string>? a b) (__string<? b a))
(define (__string<=? a b) (not (__string<? b a)))
(define (__string>=? a b) (not (__string<? a b)))
(define (string>? . args) (__chain-cmp __string>? args))
(define (string<=? . args) (__chain-cmp __string<=? args))
(define (string>=? . args) (__chain-cmp __string>=? args))

(define (__string-ci-compare s1 s2)
  ; returns -1, 0 or 1; a proper prefix is less than its extension
  (let ((n1 (string-length s1))
        (n2 (string-length s2)))
    (let loop ((i 0))
      (cond ((= i n1) (if (= i n2) 0 -1))
            ((= i n2) 1)
            (else
             (let ((c1 (char-downcase (string-ref s1 i)))
                   (c2 (char-downcase (string-ref s2 i))))
               (cond ((__char-ci<? c1 c2) -1)
                     ((__char-ci<? c2 c1) 1)
                     (else (loop (+ i 1))))))))))

(define (__string-ci=? a b) (= (__string-ci-compare a b) 0))
(define (__string-ci<? a b) (< (__string-ci-compare a b) 0))
(define (__string-ci>? a b) (> (__string-ci-compare a b) 0))
(define (__string-ci<=? a b) (<= (__string-ci-compare a b) 0))
(define (__string-ci>=? a b) (>= (__string-ci-compare a b) 0))
(define (string-ci=? . args) (__chain-cmp __string-ci=? args))
(define (string-ci<? . args) (__chain-cmp __string-ci<? args))
(define (string-ci>? . args) (__chain-cmp __string-ci>? args))
(define (string-ci<=? . args) (__chain-cmp __string-ci<=? args))
(define (string-ci>=? . args) (__chain-cmp __string-ci>=? args))

(define (substring s start end) (__substring s start end))

(define (string-append . strs) (__fold-left __string-append "" strs))

(define (string->list s)
  (let loop ((i (- (string-length s) 1))
             (acc '()))
    (if (< i 0)
        acc
        (loop (- i 1) (cons (string-ref s i) acc)))))

(define (list->string chars) (__list->string chars))

(define (string-copy s) (substring s 0 (string-length s)))

(define (string-fill! s c)
  (let loop ((i 0))
    (if (= i (string-length s))
        (void)
        (begin (string-set! s i c)
               (loop (+ i 1))))))

;; Symbols

(define (symbol->string s) (__symbol->string s))
(define (string->symbol s) (__string->symbol s))

;; Vectors

(define (vector-fill! v obj)
  (let loop ((i 0))
    (if (= i (vector-length v))
        (void)
        (begin (vector-set! v i obj)
               (loop (+ i 1))))))

;)CORE";
