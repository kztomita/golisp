(defmacro optional_arg (a &optional b (c 10) (d))
  `(print '(,a ,b ,c ,d)))

(optional_arg 1)
(optional_arg 1 2)
(optional_arg 1 2 3)
(optional_arg 1 2 3 4)

(print "====")

(defmacro key_arg (a &key b (c 1) (d))
  `(print '(,a ,b ,c ,d)))

(key_arg 1)
(key_arg 1 :b 2)
(key_arg 1 :c 3 :b 2)
(key_arg 1 :c 3 :b 2 :d 4)

(print "====")

(defmacro key_arg2 (a &key ((:foo f)) ((:bar b) 2))
  `(print '(,a ,f ,b)))

(key_arg2 1)
(key_arg2 1 :foo 2 :bar 3)

(print "====")

(defmacro optional_rest_key_arg (a &optional b &rest c &key (d 1))
  `(print '(,a ,b ,c ,d)))

(optional_rest_key_arg 1)
(optional_rest_key_arg 1 2)
;(optional_rest_key_arg 1 2 3)	; error
;(optional_rest_key_arg 1 :d 3)	; error
(optional_rest_key_arg 1 2 :d 3)
