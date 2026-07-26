; %lpc %s --print expand --stop-after expand

(define-syntax test-lit
  (syntax-rules (lit)
    ((_ lit) 'yes)
    ((_ other) 'no)))

(test-lit lit)
(test-lit not-lit)
