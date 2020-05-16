package interpreter

import (
	"fmt"
)

func funcCons(ev *evaluator, arglist node) (node, error) {
	if !isProperList(arglist) {
		return nil, fmt.Errorf("Wrong type argument.")
	}

	args, err := createSliceFromProperList(arglist)
	if err != nil {
		return nil, err
	}
	if len(args) != 2 {
		return nil, fmt.Errorf("Wrong number of arguments.")
	}

	return &ConsCell{
		car: args[0],
		cdr: args[1],
	}, nil
}
