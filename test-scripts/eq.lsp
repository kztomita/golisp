(print (eq 3 3))
(print (eq 3 (+ 1 2)))
(print (eq 3 2))
(print (eq 'a 'a))
(print (eq 'a 'b))
(print "====")	; list
(print (eq '(a b) '(a b)))
(setq a '(a b))
(print (eq a a))
(print "====")	; sequence
(print (eq "aaa" "aaa"))
(setq a "aaa")
(print (eq a a))
