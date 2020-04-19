package interpreter

import (
	"fmt"
)

func funcEqual(ev *evaluator, c *ConsCell) (node, error) {
	if c == nil {
		return nil, fmt.Errorf("Wrong number of arguments.")
	}
	if !c.isList() {
		return nil, fmt.Errorf("Wrong type argument.")
	}
	if c.length() < 2 {
		return nil, fmt.Errorf("Wrong number of arguments.")
	}

	args := createSliceFromList(c)

	prev, err := ev.Eval(args[0])
	if err != nil {
		return nil, err
	}

	args = args[1:len(args)]

	for _, arg := range args {
		element, err := ev.Eval(arg)
		if err != nil {
			return nil, err
		}
		result, err := arithmeticComparisonOp("==", prev, element)
		if err != nil {
			return nil, err
		}
		if !result {
			return &NilNode{}, nil
		}
	}

	return &TrueNode{}, nil
}

func funcAnd(ev *evaluator, c *ConsCell) (node, error) {
	if c != nil && !c.isList() {
		return nil, fmt.Errorf("Wrong type argument.")
	}

	args := createSliceFromList(c)
	for _, arg := range args {
		result, err := ev.Eval(arg)
		if err != nil {
			return nil, err
		}
		_, ok := result.(*NilNode)
		if ok {
			return &NilNode{}, nil
		}
	}
	return &TrueNode{}, nil
}

func funcOr(ev *evaluator, c *ConsCell) (node, error) {
	if c != nil && !c.isList() {
		return nil, fmt.Errorf("Wrong type argument.")
	}

	args := createSliceFromList(c)
	for _, arg := range args {
		result, err := ev.Eval(arg)
		if err != nil {
			return nil, err
		}
		_, ok := result.(*NilNode)
		if !ok {
			return &TrueNode{}, nil
		}
	}
	return &NilNode{}, nil
}
