(defmacro inc (var)
  (list 'setq var (list '+ var 1)))

(setq a 1)
(print a)
(inc a)
(print a)

(print "====")

; backquote
(defmacro foo (var &body body)
  `(progn
     (print ,var)
     (print ',body)
     (print '(,@body))))

(foo 1 2 3)

