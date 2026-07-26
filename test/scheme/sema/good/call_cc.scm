; %lpc %s
(define (test-escape)
  (call/cc (lambda (k)
    (k 42)
    (__print "should not reach here"))))

(__print (test-escape))

(__print (+ 1 (call/cc (lambda (k) (k 10)))))

(__print (call/cc (lambda (k) 99)))
