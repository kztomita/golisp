(print (setq a 1))
(print (setq b 2 c (+ 1 2)))
(print (list a b c))

(print "====")
(print (setq outer_a 1))

(defun foo (a b)
  (let ((x 100) (y 200))
    (setq outer_a 100)	; overwrite
    (setq outer_b 200)	; new
    (setq b 20)		; overwrite parameter
    (setq y 30)		; overwite lexical variable
    (print (list outer_a outer_b a b x y))))


(print outer_a)
(foo 1 2)
(print (list outer_a outer_b))

