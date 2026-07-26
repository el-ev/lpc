; %lpc %s
(__print (symbol->string 'abc))
(__print (string->symbol "xyz"))
(__print (eq? (string->symbol "a") 'a))
(__print (symbol? 'a))
(__print (symbol? "a"))
(__print (string->symbol (symbol->string 'round-trip)))
