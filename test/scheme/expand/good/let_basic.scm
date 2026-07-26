; %lpc --stop-after expand --print expand %s
(let ((x 1) (y 2)) (+ x y))
(let* ((x 1) (y (+ x 1))) y)
(let ((a 1) (b 2) (c 3)) (+ a b c))
