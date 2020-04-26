
(defun plus (x y) (+ x y))

(print (plus 2 3))

; bodyなし関数
(print "----")
(print (defun foo ()))

(print "----")
(defun optional_arg (a &optional b (c 10) (d))
  (print (list a b c d)))

(optional_arg 1)
(optional_arg 1 2)
(optional_arg 1 2 3)
(optional_arg 1 2 3 4)

(defun rest_arg (a &rest b)
  (print (list a b)))

(print "----")
(rest_arg 1)
(rest_arg 1 2)
(rest_arg 1 2 3)

(defun optional_rest_arg (a &optional b &rest c)
  (print (list a b c)))

(print "----")
(optional_rest_arg 1)
(optional_rest_arg 1 2)
(optional_rest_arg 1 2 3)
(optional_rest_arg 1 2 3 4)
