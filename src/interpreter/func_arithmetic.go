package interpreter

import (
	"fmt"
)

func funcPlus(ev *evaluator, c *ConsCell) (node, error) {
	result := 0		// XXX 取り敢えずintのみ対応

	if c != nil && !c.isList() {
		return nil, fmt.Errorf("Wrong type argument.")
	}

	args := createSliceFromList(c)

	for _, arg := range args {
		element, err := ev.Eval(arg)
		if err != nil {
			return nil, err
		}
		intResult, ok := element.(*IntNode)
		if ok {
			result += intResult.value
		} else {
			return nil, fmt.Errorf("not integer element")
		}
	}

	return &IntNode{value: result}, nil
}

func funcMinus(ev *evaluator, c *ConsCell) (node, error) {
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

	first, err := ev.Eval(args[0])
	if err != nil {
		return nil, err
	}
	firstInt, ok := first.(*IntNode)
	if !ok {
		return nil, fmt.Errorf("Wrong type argument.")
	}
	result := firstInt.value

	args = args[1:len(args)]

	for _, arg := range args {
		element, err := ev.Eval(arg)
		if err != nil {
			return nil, err
		}
		intResult, ok := element.(*IntNode)
		if ok {
			result -= intResult.value
		} else {
			return nil, fmt.Errorf("not integer element")
		}
	}

	return &IntNode{value: result}, nil
}
