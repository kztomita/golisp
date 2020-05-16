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
		mv.values = append(mv.values, arg)
	}

	return mv, nil
}
