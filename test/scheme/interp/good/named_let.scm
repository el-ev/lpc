; %lpc %s
(define (fact n)
  (let loop ((i 1) (acc 1))
    (if (> i n)
        acc
        (loop (+ i 1) (* acc i)))))

(__print (fact 10))
(__print (let loop ((xs '(1 2 3)) (acc '()))
           (if (null? xs)
               (reverse acc)
               (loop (cdr xs) (cons (* (car xs) 2) acc)))))
(__print (let ((x 1)) (let loop ((x 2)) x)))
