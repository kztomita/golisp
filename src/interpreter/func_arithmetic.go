package interpreter

import (
	"fmt"
)

func funcAdd(ev *evaluator, arglist node) (node, error) {
	if !isProperList(arglist) {
		return nil, fmt.Errorf("Wrong type argument.")
	}

	args, err := createSliceFromProperList(arglist)
	if err != nil {
		return nil, err
	}

	var result node
	result = &IntNode{value: 0}

	for _, arg := range args {
		element, err := ev.Eval(arg)
		if err != nil {
			return nil, err
		}
		result, err = arithmeticOp(arithmeticOpAdd, result, element)
		if err != nil {
			return nil, err
		}
	}

	return result, nil
}

func funcSubtract(ev *evaluator, arglist node) (node, error) {
	if !isProperList(arglist) {
		return nil, fmt.Errorf("Wrong type argument.")
	}

	args, err := createSliceFromProperList(arglist)
	if err != nil {
		return nil, err
	}
	if len(args) < 1 {
		return nil, fmt.Errorf("Wrong number of arguments.")
	}

	first, err := ev.Eval(args[0])
	if err != nil {
		return nil, err
	}

	if len(args) == 1 {
		return arithmeticOp(arithmeticOpSubtract, &IntNode{value: 0}, first)
	}

	var result node
	result = first

	args = args[1:len(args)]

	for _, arg := range args {
		element, err := ev.Eval(arg)
		if err != nil {
			return nil, err
		}
		result, err = arithmeticOp(arithmeticOpSubtract, result, element)
		if err != nil {
			return nil, err
		}
	}

	return result, nil
}

func funcMultiply(ev *evaluator, arglist node) (node, error) {
	if !isProperList(arglist) {
		return nil, fmt.Errorf("Wrong type argument.")
	}

	args, err := createSliceFromProperList(arglist)
	if err != nil {
		return nil, err
	}

	var result node
	result = &IntNode{value: 1}

	for _, arg := range args {
		element, err := ev.Eval(arg)
		if err != nil {
			return nil, err
		}
		result, err = arithmeticOp(arithmeticOpMultiply, result, element)
		if err != nil {
			return nil, err
		}
	}

	return result, nil
}

func funcDivide(ev *evaluator, arglist node) (node, error) {
	if !isProperList(arglist) {
		return nil, fmt.Errorf("Wrong type argument.")
	}

	args, err := createSliceFromProperList(arglist)
	if err != nil {
		return nil, err
	}
	if len(args) < 1 {
		return nil, fmt.Errorf("Wrong number of arguments.")
	}

	first, err := ev.Eval(args[0])
	if err != nil {
		return nil, err
	}

	if len(args) == 1 {
		return arithmeticOp(arithmeticOpDivide, &IntNode{value: 1}, first)
	}

	var result node
	result = first

	args = args[1:len(args)]

	for _, arg := range args {
		element, err := ev.Eval(arg)
		if err != nil {
			return nil, err
		}
		result, err = arithmeticOp(arithmeticOpDivide, result, element)
		if err != nil {
			return nil, err
		}
	}

	return result, nil
}
