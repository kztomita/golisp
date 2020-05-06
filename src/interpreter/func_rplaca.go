package interpreter

import (
	"fmt"
)

func funcRplaca(ev *evaluator, arglist node) (node, error) {
	if !isProperList(arglist) {
		return nil, fmt.Errorf("rplaca: Wrong type argument.")
	}

	args, err := createSliceFromProperList(arglist)
	if err != nil {
			return nil, err
	}
	if len(args) != 2 {
			return nil, fmt.Errorf("rplaca: Wrong number of arguments.")
	}

	arg0, err := ev.Eval(args[0])
	if err != nil {
		return nil, err
	}
	cell, ok := arg0.(*ConsCell)
	if !ok {
		return nil, fmt.Errorf("rplaca: A first parameter is not cons.")
	}

	obj, err := ev.Eval(args[1])
	if err != nil {
		return nil, err
	}

	cell.car = obj

	return cell, nil
}

func funcRplacd(ev *evaluator, arglist node) (node, error) {
	if !isProperList(arglist) {
		return nil, fmt.Errorf("rplacd: Wrong type argument.")
	}

	args, err := createSliceFromProperList(arglist)
	if err != nil {
			return nil, err
	}
	if len(args) != 2 {
			return nil, fmt.Errorf("rplacd: Wrong number of arguments.")
	}

	arg0, err := ev.Eval(args[0])
	if err != nil {
		return nil, err
	}
	cell, ok := arg0.(*ConsCell)
	if !ok {
		return nil, fmt.Errorf("rplaca: A first parameter is not cons.")
	}

	obj, err := ev.Eval(args[1])
	if err != nil {
		return nil, err
	}

	cell.cdr = obj

	return cell, nil
}
