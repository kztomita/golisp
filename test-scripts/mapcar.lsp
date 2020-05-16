
(print (mapcar #'car '((1 a) (2 b) (3 c))))
(print (mapcar 'car '((1 a) (2 b) (3 c))))
(print (mapcar #'cons '(a b c) '(1 2 3)))
(print (mapcar #'+ '(1 2 3) '(1 2)))
(print (mapcar #'(lambda (x) (* x 2 )) '(1 2 3)))
