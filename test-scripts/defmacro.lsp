(defmacro inc (var)
  (list 'setq var (list '+ var 1)))

(setq a 1)
(print a)
(inc a)
(print a)
