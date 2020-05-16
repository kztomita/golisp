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
		return args[0], nil
	}

	// len(args) >= 2

	elements, err := createSliceFromProperList(args[0])
	if err != nil {
		return nil, err
	}

	args = args[1:len(args)]
	for i, arg := range args {
		switch arg.(type) {
		case *ConsCell:
			list, err := createSliceFromProperList(arg)
			if err != nil {
				return nil, err
			}
			elements = append(elements, list...)
		case *NilNode:
			// empty list
			continue
		default:
			// リスト以外を追加
			// cdrにConsCell以外を追加するのでdot listになる
			// これ以降に引数があればエラー
			if i != len(args) - 1 {
				return nil, fmt.Errorf("%v is not a list.", arg.ToString())
			}
			if len(elements) == 0 {
				return arg, nil
			}
			list := createList(elements)
			cell := list.(*ConsCell)
			cell.tail().cdr = arg
			return list, nil
		}
	}

	return createList(elements), nil
}
