(defun foo ()
  (defun bar ()
    (print "bar called"))
  (bar))

; (bar) ここではまだ存在しない
(foo)
(bar) ; 外からでも呼べる

; 上書き
(defun bar ()
  (print "overwited bar called"))

(bar)

; barはglobalに一つしか存在しない

