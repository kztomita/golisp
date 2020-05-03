package interpreter

import (
	"fmt"
)

func funcIf(ev *evaluator, arglist node) (node, error) {
	if !isProperList(arglist) {
		return nil, fmt.Errorf("if: Wrong type argument.")
	}

	args, err := createSliceFromProperList(arglist)
	if err != nil {
		return nil, err
	}
	if len(args) != 3 {
		return nil, fmt.Errorf("if: Wrong number of arguments.")
	}

	condition, err := ev.Eval(args[0])
	if err != nil {
		return nil, err
	}

	var nd node
	if condition.GetNodeType() == NtNil {
		nd = args[2]  // else
	} else {
		nd = args[1]  // then
	}

	result, err := ev.Eval(nd)
	if err != nil {
		return nil, err
	}

	return result, nil
}
