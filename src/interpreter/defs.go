package interpreter

var systemFuncDefinitions string

func init() {
	systemFuncDefinitions = `
	(defun cadr (x) (car (cdr x)))

	(defun cddr (x) (cdr (cdr x)))

	(defun funcall (fn &rest args)
	  (apply fn args))
	`
}
