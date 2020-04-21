(defun plus (x y) (+ x y))

(print (plus 2 3))


(defun optional_arg (a &optional b (c 10))
  (print (list a b c))
  )

(optional_arg 1)
(optional_arg 1 2)
(optional_arg 1 2 3)
