package interpreter

import (
	"fmt"
)

func funcValues(ev *evaluator, arglist node) (node, error) {
	if !isProperList(arglist) {
		return nil, fmt.Errorf("values: Wrong type argument.")
	}

	args, err := createSliceFromProperList(arglist)
	if err != nil {
		return nil, err
	}

	mv := &MultipleValuesNode{values: []node{}}

	for _, arg := range args {
		evaled, err := ev.Eval(arg)
		if err != nil {
			return nil, err
		}
		mv.values = append(mv.values, evaled)
	}

	return mv, nil
}
