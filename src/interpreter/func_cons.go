package interpreter

import (
	"fmt"
)

func funcCons(ev *evaluator, c *ConsCell) (node, error) {
	if c == nil {
		return &NilNode{}, nil
	}
	if !c.isList() {
		return nil, fmt.Errorf("Wrong type argument.")
	}
	if c.length() != 2 {
		return nil, fmt.Errorf("Wrong number of arguments.")
	}

	args := createSliceFromList(c)

	first, err := ev.Eval(args[0])
	if err != nil {
		return nil, err
	}

	second, err := ev.Eval(args[1])
	if err != nil {
		return nil, err
	}

	return &ConsCell{
		car: first,
		cdr: second,
	}, nil
}
