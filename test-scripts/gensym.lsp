(print (eq (gensym) (gensym)))
(setq a (gensym))
(setq b a)
(print (eq a b))

; TODO intern実装後名称の衝突テスト追加

