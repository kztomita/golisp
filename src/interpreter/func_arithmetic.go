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
		result, err = arithmeticOp(arithmeticOpAdd, result, arg)
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

	if len(args) == 1 {
		return arithmeticOp(arithmeticOpSubtract, &IntNode{value: 0}, args[0])
	}

	var result node
	result = args[0]

	args = args[1:len(args)]

	for _, arg := range args {
		result, err = arithmeticOp(arithmeticOpSubtract, result, arg)
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
		result, err = arithmeticOp(arithmeticOpMultiply, result, arg)
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

	if len(args) == 1 {
		return arithmeticOp(arithmeticOpDivide, &IntNode{value: 1}, args[0])
	}

	var result node
	result = args[0]

	args = args[1:len(args)]

	for _, arg := range args {
		result, err = arithmeticOp(arithmeticOpDivide, result, arg)
		if err != nil {
			return nil, err
		}
	}

	return result, nil
}
