(defmacro inc (var)
  (list (quote setq) var (list (quote +) var 1)))

(setq a 1)
(inc a)
(print a)
