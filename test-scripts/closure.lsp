(defun adder (x) (lambda (y) (+ x y)))
(setq add3 (adder 3))
(print (apply add3 '(5)))

(print "====")

(setq fn (let ((i 3))
  #'(lambda (x) (+ x i))))

(print (apply fn '(2)))

(print "====")

(let ((counter 0))
  (defun reset ()
    (setq counter 0))
  (defun inc ()
    (setq counter (+ counter 1))))

(print (list (inc) (inc) (reset) (inc)))

(print "====")
; 二つのlambda(closure)が同じフリーレキシカル変数を参照するケース
; Ref to http://www.lispworks.com/documentation/HyperSpec/Body/03_ad.htm

(defun two-funs (x)
   (list (function (lambda () x))
         (function (lambda (y) (setq x y)))))
(setq funs (two-funs 6))
(print (apply (car funs) '()))
(print (apply (car (cdr funs)) '(43)))
(print (apply (car funs) '()))

