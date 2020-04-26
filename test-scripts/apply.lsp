(print (apply #'+ '(1 2)))
(print (apply #'+ 1 2 '(3 4)))
(print (apply #'+ '()))
(print (apply #'+ nil))
(print (apply '+ '(1 2)))	; 関数名をシンボルで指定

(print "====")

(defun foo ()
  (print "foo called"))

(defun foo1 (a)
  (print "foo1 called")
  (print a))

(defun foo2 (a b)
  (print "foo2 called")
  (print a)
  (print b))

(apply #'foo '())
(apply #'foo1 '(1))
(apply #'foo2 '(1 2))
(apply 'foo1 '(1))	; 関数名をシンボルで指定

; TODO lambda

(print "====")
; 変数経由
(setq plus '+)
(print (apply plus '(1 2)))

(setq plus #'+)
(print (apply plus '(3 4)))

(setq f 'foo1)
(apply f '(1))

