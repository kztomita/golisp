(print (function +))

(defun foo ())

(print (function foo))
(print #'foo)

; TODO lambda

; XXX 現状マクロ名を渡してもオブジェクトを返してしまう
