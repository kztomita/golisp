(defun cond-test (x)
  (cond ((= x 1) (print "x = 1"))
	((= x 2) (print "x = 2"))))

(print (cond-test 1))
(print (cond-test 2))
(print (cond-test 3))
