package interpreter

import (
	"fmt"
)

func funcAppend(ev *evaluator, arglist node) (node, error) {
	if !isProperList(arglist) {
		return nil, fmt.Errorf("Wrong type argument.")
	}

	args, err := createSliceFromProperList(arglist)
	if err != nil {
		return nil, err
	}

	if len(args) == 0 {
		return &NilNode{}, nil
	}

	if len(args) == 1 {
		return ev.Eval(args[0])
	}

	// len(args) >= 2
	elements := []node{}

	arg0, err := ev.Eval(args[0])
	if err != nil {
		return nil, err
	}

	switch arg0.(type) {
	case *ConsCell:
		elements = createSliceFromList(arg0.(*ConsCell))
	case *NilNode:
		// empty list
	default:
		return nil, fmt.Errorf("The first argument is not a list.")
	}

	args = args[1:len(args)]
	for i, arg := range args {
		argx, err := ev.Eval(arg)
		if err != nil {
			return nil, err
		}

		switch argx.(type) {
		case *ConsCell:
			elements = append(elements, createSliceFromList(argx.(*ConsCell))...)
		case *NilNode:
			// empty list
			continue
		default:
			// リスト以外を追加
			// cdrにConsCell以外を追加するのでdot listになる
			// これ以降に引数があればエラー
			if i != len(args) - 1 {
				return nil, fmt.Errorf("%v is not a list.", argx.ToString())
			}
			if len(elements) == 0 {
				return argx, nil
			}
			list := createList(elements)
			cell := list.(*ConsCell)
			cell.tail().cdr = argx
			return list, nil
		}
	}

	return createList(elements), nil
}
