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
