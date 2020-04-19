package interpreter

import (
	"fmt"
)

func funcAdd(ev *evaluator, c *ConsCell) (node, error) {
	if c != nil && !c.isList() {
		return nil, fmt.Errorf("Wrong type argument.")
	}

	args := createSliceFromList(c)

	var result node
	result = &IntNode{value: 0}

	for _, arg := range args {
		element, err := ev.Eval(arg)
		if err != nil {
			return nil, err
		}
		result, err = arithmeticOp("+", result, element)
		if err != nil {
			return nil, err
		}
	}

	return result, nil
}

func funcSubtract(ev *evaluator, c *ConsCell) (node, error) {
	if c == nil {
		return nil, fmt.Errorf("Wrong number of arguments.")
	}
	if !c.isList() {
		return nil, fmt.Errorf("Wrong type argument.")
	}
	if c.length() < 1 {
		return nil, fmt.Errorf("Wrong number of arguments.")
	}

	args := createSliceFromList(c)

	first, err := ev.Eval(args[0])
	if err != nil {
		return nil, err
	}

	if len(args) == 1 {
		return arithmeticOp("-", &IntNode{value: 0}, first)
	}

	var result node
	result = first

	args = args[1:len(args)]

	for _, arg := range args {
		element, err := ev.Eval(arg)
		if err != nil {
			return nil, err
		}
		result, err = arithmeticOp("-", result, element)
		if err != nil {
			return nil, err
		}
	}

	return result, nil
}
