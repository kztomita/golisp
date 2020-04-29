(defun cadr (x) (car (cdr x)))

(defun cddr (x) (cdr (cdr x)))

(defun funcall (fn &rest args)
  (apply fn args))

(defmacro progn (&rest args)
  `(let nil ,@args))
