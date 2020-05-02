package interpreter

import (
	"fmt"
)

func funcEq(ev *evaluator, arglist node) (node, error) {
	if !isProperList(arglist) {
	}

	args, err := createSliceFromProperList(arglist)
	if err != nil {
		return nil, err
	}
	if len(args) != 2 {
		return nil, fmt.Errorf("eq: Wrong number of arguments.")
	}

	arg0, err := ev.Eval(args[0])
	if err != nil {
		return nil, err
	}

	arg1, err := ev.Eval(args[1])
	if err != nil {
		return nil, err
	}

	if arg0.GetNodeType() != arg1.GetNodeType() {
		return &NilNode{}, nil
	}
	_, ok := arg0.(*SymbolNode)
	if ok {
		if arg0.(*SymbolNode).symbolKey() == arg1.(*SymbolNode).symbolKey() {
			return &TrueNode{}, nil
		} else {
			return &NilNode{}, nil
		}
	}
	if isConstantNode(arg0) {
		if arg0.ToString() == arg1.ToString() {
			return &TrueNode{}, nil
		} else {
			return &NilNode{}, nil
		}
	}
	// object(list)
	if arg0 == arg1 {
		return &TrueNode{}, nil
	} else {
		return &NilNode{}, nil
	}
}

func isConstantNode(nd node) bool {
	switch nd.(type) {
	case *ConsCell:
		return false
	case *StringNode:
		return false	// sequence
	case nil:
		return false
	default:
		return true
	}
}