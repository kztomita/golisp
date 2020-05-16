(defun cadr (x) (car (cdr x)))

(defun cddr (x) (cdr (cdr x)))

(defun funcall (fn &rest args)
  (apply fn args))

(defun null (x) (eq x nil))

(defmacro progn (&rest args)
  `(let nil ,@args))

(defmacro cond (&rest args)
  (if (null args)
    nil
    (let ((clause (car args)))
      `(if ,(car clause)
	(progn ,@(cdr clause))
	(cond ,@(cdr args))))))

; characters type not supported yet
(defun eql (x y)
  (cond
   ((typep x 'number) (and (eq (type-of x) (type-of y))
                           (= x y)))
   (t (eq x y)))
  )

(defun symbolp (x) (typep x 'symbol))
(defun stringp (x) (typep x 'string))
(defun consp (x) (typep x 'cons))
(defun atom (x) (not (consp x)))
(defun listp (x) (or (consp x) (null x)))
