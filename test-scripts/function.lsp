; system function symbol
(print (function +))

; function symbol
(defun foo ())

(print (function foo))
(print #'foo)

; lambda function
(print (function (lambda (x) (+ x 1))))

; XXX 現状マクロ名を渡してもオブジェクトを返してしまう
