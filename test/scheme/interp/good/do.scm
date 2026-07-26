; %lpc %s
(__print
 (let ((v (make-vector 5)))
   (do ((i 0 (+ i 1)))
       ((= i 5) v)
     (vector-set! v i i))))

(__print
 (do ((i 0 (+ i 1))
      (acc '() (cons i acc)))
     ((= i 4) (reverse acc))))

(__print
 (do ((i 0 (+ i 1))
      (x 10))
     ((= i 3) x)))

(__print
 (do ((i 0 (+ i 1)))
     ((= i 3))))

(__print
 (do ((i 0 (+ i 1))
      (j 10 (- j 1)))
     ((= i j) (list i j))))
