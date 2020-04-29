package interpreter

import (
	"fmt"
)

func funcQuote(ev *evaluator, arglist node) (node, error) {
	if !isProperList(arglist) {
		return nil, fmt.Errorf("Wrong type argument.")
	}

	if countProperListLength(arglist) != 1 {
		return nil, fmt.Errorf("Wrong number of arguments.")
	}

	return arglist.(*ConsCell).car, nil
}
