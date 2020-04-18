(defmacro inc (var)
  (list 'setq var (list '+ var 1)))

(setq a 1)
(inc a)
(print a)
