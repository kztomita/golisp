package interpreter

import (
	"fmt"
)

func funcProgn(ev *evaluator, arglist node) (node, error) {
	if !isProperList(arglist) {
		return nil, fmt.Errorf("Wrong type argument.")
	}

	var lastResult node
	lastResult = &NilNode{}
	for p := getConsCell(arglist) ; p != nil ; p = p.next() {
		var err error
		lastResult, err = ev.Eval(p.car)
		if err != nil {
			return nil, err
		}
	}

	return lastResult, nil
}
