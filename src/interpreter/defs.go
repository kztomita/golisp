package interpreter

var systemFuncDefinitions string

func init() {
	systemFuncDefinitions = `
	(defun funcall (fn &rest args)
	  (apply fn args))
	`
}
