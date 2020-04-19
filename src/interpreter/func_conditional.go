package interpreter

import (
	"fmt"
)

func compareAdjacentArguments(ev *evaluator, args []node, op string) (node, error) {
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
		result, err := arithmeticComparisonOp(op, prev, element)
		if err != nil {
			return nil, err
		}
		if !result {
			return &NilNode{}, nil
		}
	}

	return &TrueNode{}, nil
}

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

	return compareAdjacentArguments(ev, args, "==")
}

func funcNotEqual(ev *evaluator, c *ConsCell) (node, error) {
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

	for pivot := 0 ; pivot < len(args) - 1 ; pivot++ {
		for i := pivot + 1 ; i < len(args) ; i++ {
			result, err := arithmeticComparisonOp("==", args[pivot], args[i])
			if err != nil {
				return nil, err
			}
			if result {
				return &NilNode{}, nil
			}
		}
	}

	return &TrueNode{}, nil
}

func funcGreaterThan(ev *evaluator, c *ConsCell) (node, error) {
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

	return compareAdjacentArguments(ev, args, ">")
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
