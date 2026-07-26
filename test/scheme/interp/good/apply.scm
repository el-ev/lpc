; %lpc %s
(define (count-loop n)
  (let loop ((i 0))
    (if (= i n)
        'done
        (apply loop (list (+ i 1))))))

(__print (apply + '(1 2 3)))
(__print (apply + 1 2 '(3 4)))
(__print (apply max '(3 1 4)))
(__print (apply list '()))
(__print (apply cons '(1 2)))
(__print (apply (lambda (a b . rest) (list a b rest)) '(1 2 3 4)))
(__print (apply apply (list list '(1 2))))
(__print (count-loop 100000))
