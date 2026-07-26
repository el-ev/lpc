; %lpc --stop-after expand --print expand %s
(when #t (display "hello") (newline))
(when (< 1 2) (display "yes") (newline))
(unless #f 1 2 3)
(unless #t (a) (b) (c))
