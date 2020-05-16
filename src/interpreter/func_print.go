package interpreter

import (
	"fmt"
)

func funcPrin1(ev *evaluator, arglist node) (node, error) {
	if !isProperList(arglist) {
		return nil, fmt.Errorf("Wrong type argument.")
	}

	args, err := createSliceFromProperList(arglist)
	if err != nil {
		return nil, err
	}
	// streamは未対応
	if len(args) != 1 {
		return nil, fmt.Errorf("Wrong number of arguments.")
	}

	ev.writed = true
	fmt.Print(args[0].ToString())

	return args[0], nil
}

func funcPrint(ev *evaluator, arglist node) (node, error) {
	if !isProperList(arglist) {
		return nil, fmt.Errorf("Wrong type argument.")
	}

	args, err := createSliceFromProperList(arglist)
	if err != nil {
		return nil, err
	}
	// streamは未対応
	if len(args) != 1 {
		return nil, fmt.Errorf("Wrong number of arguments.")
	}

	ev.writed = true
	fmt.Printf("\n%v ", args[0].ToString())

	return args[0], nil
}
