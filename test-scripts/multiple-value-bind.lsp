(print (multiple-value-bind (a b) (values 1 2)))
(print (multiple-value-bind (a b) (values 1 2) (list a b)))
(print (multiple-value-bind (a b) (values 1) (list a b)))
(print (multiple-value-bind (a b) (values 1 2 3) (list a b)))
(print (multiple-value-bind (a b) 1 (list a b)))

