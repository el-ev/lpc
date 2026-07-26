; %lpc --stop-after expand --print expand %s
(cond (#t (display 1)) (else (display 2)))
(cond (a 1) (b 2))
(cond (#t 1) (#f 2) (else 3))
