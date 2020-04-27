(print (lambda (x) (+ x 1)))
(print (apply (lambda (x) (+ x 1)) '(1)))

(defun adder (x) (lambda (y) (+ x y)))
(setq add3 (adder 3))
(print (apply add3 '(5)))
