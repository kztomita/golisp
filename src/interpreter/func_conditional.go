package interpreter

import (
	"fmt"
)

func compareAdjacentArguments(args []node, op arithmeticComparisonType) (node, error) {
	prev := args[0]

	args = args[1:len(args)]

	for _, arg := range args {
		result, err := arithmeticComparison(op, prev, arg)
		if err != nil {
			return nil, err
		}
		if !result {
			return &NilNode{}, nil
		}
		prev = arg
	}

	return &TrueNode{}, nil
}

func funcEqual(ev *evaluator, arglist node) (node, error) {
	if !isProperList(arglist) {
		return nil, fmt.Errorf("=: Wrong type argument.")
	}

	args, err := createSliceFromProperList(arglist)
	if err != nil {
		return nil, err
	}
	if len(args) < 2 {
		return nil, fmt.Errorf("=: Wrong number of arguments.")
	}

	return compareAdjacentArguments(args, arithmeticComparisonEqual)
}

func funcNotEqual(ev *evaluator, arglist node) (node, error) {
	if !isProperList(arglist) {
		return nil, fmt.Errorf("/=: Wrong type argument.")
	}

	args, err := createSliceFromProperList(arglist)
	if err != nil {
		return nil, err
	}
	if len(args) < 2 {
		return nil, fmt.Errorf("/=: Wrong number of arguments.")
	}

	for pivot := 0 ; pivot < len(args) - 1 ; pivot++ {
		for i := pivot + 1 ; i < len(args) ; i++ {
			result, err := arithmeticComparison(arithmeticComparisonEqual, args[pivot], args[i])
			if err != nil {
				return nil, err
			}
			if result {
				return &NilNode{}, nil
			}
		}
	}

	return &TrueNode{}, nil
}

func funcGreaterThan(ev *evaluator, arglist node) (node, error) {
	if !isProperList(arglist) {
		return nil, fmt.Errorf(">: Wrong type argument.")
	}

	args, err := createSliceFromProperList(arglist)
	if err != nil {
		return nil, err
	}
	if len(args) < 2 {
		return nil, fmt.Errorf(">: Wrong number of arguments.")
	}

	return compareAdjacentArguments(args, arithmeticComparisonGreaterThan)
}

func funcGreaterThanOrEqualTo(ev *evaluator, arglist node) (node, error) {
	if !isProperList(arglist) {
		return nil, fmt.Errorf(">=: Wrong type argument.")
	}

	args, err := createSliceFromProperList(arglist)
	if err != nil {
		return nil, err
	}
	if len(args) < 2 {
		return nil, fmt.Errorf(">=: Wrong number of arguments.")
	}

	return compareAdjacentArguments(args, arithmeticComparisonGreaterThanOrEqualTo)
}

func funcLessThan(ev *evaluator, arglist node) (node, error) {
	if !isProperList(arglist) {
		return nil, fmt.Errorf("<: Wrong type argument.")
	}

	args, err := createSliceFromProperList(arglist)
	if err != nil {
		return nil, err
	}
	if len(args) < 2 {
		return nil, fmt.Errorf("<: Wrong number of arguments.")
	}

	return compareAdjacentArguments(args, arithmeticComparisonLessThan)
}

func funcLessThanOrEqualTo(ev *evaluator, arglist node) (node, error) {
	if !isProperList(arglist) {
		return nil, fmt.Errorf("<=: Wrong type argument.")
	}

	args, err := createSliceFromProperList(arglist)
	if err != nil {
		return nil, err
	}
	if len(args) < 2 {
		return nil, fmt.Errorf("<=: Wrong number of arguments.")
	}

	return compareAdjacentArguments(args, arithmeticComparisonLessThanOrEqualTo)
}

func funcAnd(ev *evaluator, arglist node) (node, error) {
	if !isProperList(arglist) {
		return nil, fmt.Errorf("and: Wrong type argument.")
	}

	args, err := createSliceFromProperList(arglist)
	if err != nil {
		return nil, err
	}

	for _, arg := range args {
		result, err := ev.Eval(arg)
		if err != nil {
			return nil, err
		}
		_, ok := result.(*NilNode)
		if ok {
			return &NilNode{}, nil
		}
	}
	return &TrueNode{}, nil
}

func funcOr(ev *evaluator, arglist node) (node, error) {
	if !isProperList(arglist) {
		return nil, fmt.Errorf("or: Wrong type argument.")
	}

	args, err := createSliceFromProperList(arglist)
	if err != nil {
		return nil, err
	}

	for _, arg := range args {
		result, err := ev.Eval(arg)
		if err != nil {
			return nil, err
		}
		_, ok := result.(*NilNode)
		if !ok {
			return &TrueNode{}, nil
		}
	}
	return &NilNode{}, nil
}

func funcNot(ev *evaluator, arglist node) (node, error) {
	if !isProperList(arglist) {
		return nil, fmt.Errorf("not: Wrong type argument.")
	}

	args, err := createSliceFromProperList(arglist)
	if err != nil {
		return nil, err
	}
	if len(args) != 1 {
		return nil, fmt.Errorf("not: Wrong number of arguments.")
	}
	_, ok := args[0].(*NilNode)
	if ok {
		return &TrueNode{}, nil
	} else {
		return &NilNode{}, nil
	}
}
