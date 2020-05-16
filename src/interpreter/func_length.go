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

	switch nd := args[0].(type) {
	case *ConsCell, *NilNode:
		if !isProperList(args[0]) {
			return nil, fmt.Errorf("Wrong type argument (is not proper list).")
		}
		return &IntNode{value: countProperListLength(args[0])}, nil
	case *StringNode:
		return &IntNode{value: len(nd.value)}, nil
	default:
		return nil, fmt.Errorf("An argument is not sequence.")
	}
}
