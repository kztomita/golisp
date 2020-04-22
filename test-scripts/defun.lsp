(defun plus (x y) (+ x y))

(print (plus 2 3))


(defun optional_arg (a &optional b (c 10) (d))
  (print (list a b c d))
  )

(optional_arg 1)
(optional_arg 1 2)
(optional_arg 1 2 3)
(optional_arg 1 2 3 4)
