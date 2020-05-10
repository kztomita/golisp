(defun optional_arg (a &optional b (c 10) (d))
  (print (list a b c d)))

(optional_arg 1)
(optional_arg 1 2)
(optional_arg 1 2 3)
(optional_arg 1 2 3 4)

(print "----")

(defun rest_arg (a &rest b)
  (print (list a b)))

(rest_arg 1)
(rest_arg 1 2)
(rest_arg 1 2 3)

(print "----")

(defun optional_rest_arg (a &optional b &rest c)
  (print (list a b c)))

(optional_rest_arg 1)
(optional_rest_arg 1 2)
(optional_rest_arg 1 2 3)
(optional_rest_arg 1 2 3 4)

(print "----")

(defun key_arg (a &key b (c 1) (d))
  (print (list a b c d)))

(key_arg 1)
(key_arg 1 :b 2)
(key_arg 1 :c 3 :b 2)
(key_arg 1 :c 3 :b 2 :d 4)

(print "----")

(defun key_arg2 (a &key ((:foo f)) ((:bar b) 2))
  (print (list a f b)))

(key_arg2 1)
(key_arg2 1 :foo 2 :bar 3)

(print "----")

(defun optional_rest_key_arg (a &optional b &rest c &key (d 1))
  (print (list a b c d)))

(optional_rest_key_arg 1)
(optional_rest_key_arg 1 2)
;(optional_rest_key_arg 1 2 3)	; error
;(optional_rest_key_arg 1 :d 3)	; error
(optional_rest_key_arg 1 2 :d 3)
