package interpreter

import (
	"fmt"
)

func funcLength(ev *evaluator, arglist node) (node, error) {
	if !isProperList(arglist) {
		return nil, fmt.Errorf("Wrong type argument.")
	}

	args, err := createSliceFromProperList(arglist)
	if err != nil {
		return nil, err
	}
	if len(args) != 1 {
		return nil, fmt.Errorf("Wrong number of arguments.")
	}

	result, err := ev.Eval(args[0])
	if err != nil {
		return nil, err
	}

	switch nd := result.(type) {
	case *ConsCell, *NilNode:
		if !isProperList(result) {
			return nil, fmt.Errorf("Wrong type argument (is not proper list).")
		}
		return &IntNode{value: countProperListLength(result)}, nil
	case *StringNode:
		return &IntNode{value: len(nd.value)}, nil
	default:
		return nil, fmt.Errorf("An argument is not sequence.")
	}
}
