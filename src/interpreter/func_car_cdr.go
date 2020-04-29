package interpreter

import (
	"fmt"
)

// (car list)
// Ex. (car '(1 2 3))
func funcCar(ev *evaluator, arglist node) (node, error) {
	if !isProperList(arglist) {
		return nil, fmt.Errorf("Wrong type argument.")
	}

	args, err := createSliceFromProperList(arglist)
	if err != nil {
		return nil, err
	}
	if len(args) != 1 {
		return nil, fmt.Errorf("Wrong number of arguments.")
	}

	result, err := ev.Eval(args[0])
	if err != nil {
		return nil, err
	}
	switch nd := result.(type) {
	case *ConsCell:
		return nd.car, nil
	case *NilNode:
		// empty list, nil symbol
		return &NilNode{}, nil
	default:
		return nil, fmt.Errorf("Wrong type argument.")
	}
}

// (cdr list)
// Ex. (cdr '(1 2 3))
func funcCdr(ev *evaluator, arglist node) (node, error) {
	if !isProperList(arglist) {
		return nil, fmt.Errorf("Wrong type argument.")
	}

	args, err := createSliceFromProperList(arglist)
	if err != nil {
		return nil, err
	}
	if len(args) != 1 {
		return nil, fmt.Errorf("Wrong number of arguments.")
	}

	result, err := ev.Eval(args[0])
	if err != nil {
		return nil, err
	}
	switch nd := result.(type) {
	case *ConsCell:
		return nd.cdr, nil
	case *NilNode:
		// empty list, nil symbol
		return &NilNode{}, nil
	default:
		return nil, fmt.Errorf("Wrong type argument.")
	}
}
