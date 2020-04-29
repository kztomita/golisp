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

	first, err := ev.Eval(args[0])
	if err != nil {
		return nil, err
	}

	second, err := ev.Eval(args[1])
	if err != nil {
		return nil, err
	}

	return &ConsCell{
		car: first,
		cdr: second,
	}, nil
}
