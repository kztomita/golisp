(defun plus (x y) (+ x y))

(plus 2 3)

(defun foo (a &optional b (c 10))
  (print (list a b c))
  )

(foo 1)
(foo 1 2)
(foo 1 2 3)

