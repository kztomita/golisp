(setq outer_var 100)
(setq b 10)
(setq y 20)

(defun foo (a b)
  (let ((x 100) (y 200))
    (print (list outer_var a b x y))
    (+ outer_var a b x y)))

(foo 2 3)

(print (list outer_var b y))

(print "====")
(let ()
  (print "nil binding list"))
(let nil
  (print "nil binding list"))

