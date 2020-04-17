(setq c 100)
(defun foo (a b)
  (let ((x 100) (y 200))
    (+ a b x y c)))

(foo 2 3)


