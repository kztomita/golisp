(setq outer_var 100)
(setq y 10)

(defun foo (a b)
  (let ((x 100) (y 200))
    (print (list outer_var a b x y))
    (+ outer_var a b x y)))

(foo 2 3)

(print (list outer_var y))
