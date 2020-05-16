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

	cell, ok := args[0].(*ConsCell)
	if !ok {
		return nil, fmt.Errorf("rplaca: A first parameter is not cons.")
	}

	cell.car = args[1]

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

	cell, ok := args[0].(*ConsCell)
	if !ok {
		return nil, fmt.Errorf("rplaca: A first parameter is not cons.")
	}

	cell.cdr = args[1]

	return cell, nil
}
