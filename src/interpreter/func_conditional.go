package interpreter

import (
	"fmt"
)

func compareAdjacentArguments(ev *evaluator, args []node, op arithmeticComparisonType) (node, error) {
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
		result, err := arithmeticComparison(op, prev, element)
		if err != nil {
			return nil, err
		}
		if !result {
			return &NilNode{}, nil
		}
		prev = element
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

	return compareAdjacentArguments(ev, args, arithmeticComparisonEqual)
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
			result, err := arithmeticComparison(arithmeticComparisonEqual, args[pivot], args[i])
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

	return compareAdjacentArguments(ev, args, arithmeticComparisonGreaterThan)
}

func funcGreaterThanOrEqualTo(ev *evaluator, c *ConsCell) (node, error) {
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

	return compareAdjacentArguments(ev, args, arithmeticComparisonGreaterThanOrEqualTo)
}

func funcLessThan(ev *evaluator, c *ConsCell) (node, error) {
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

	return compareAdjacentArguments(ev, args, arithmeticComparisonLessThan)
}

func funcLessThanOrEqualTo(ev *evaluator, c *ConsCell) (node, error) {
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

	return compareAdjacentArguments(ev, args, arithmeticComparisonLessThanOrEqualTo)
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

func funcNot(ev *evaluator, c *ConsCell) (node, error) {
	if c == nil {
		return nil, fmt.Errorf("Wrong number of arguments.")
	}
	if !c.isList() {
		return nil, fmt.Errorf("Wrong type argument.")
	}
	if c.length() != 1 {
		return nil, fmt.Errorf("Wrong number of arguments.")
	}

	arg0 := c.car

	result, err := ev.Eval(arg0)
	if err != nil {
		return nil, err
	}
	_, ok := result.(*NilNode)
	if ok {
		return &TrueNode{}, nil
	} else {
		return &NilNode{}, nil
	}
}
