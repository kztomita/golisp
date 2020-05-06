(setq a '(a b c))

(print (rplaca '(a b) 'x))
(print (rplaca a (+ 1 2)))
(print (rplacd a '(4 5)))


