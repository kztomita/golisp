(print (eql 'a 'a))	; eq true
(print (eql 'a 'b))	; eq false
(print (eql 1 1))
(print (eql 1 2))
(print (eql 1 1.0))
(print (eql 1.0 1.0))
(print (eql "foo" "foo"))

; charactersは現在未サポート
