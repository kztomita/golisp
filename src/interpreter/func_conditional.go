package interpreter

import (
	"fmt"
)

func compareAdjacentArguments(ev *evaluator, args []node, op arithmeticComparisonType) (node, error) {
	prev, err := ev.Eval(args[0])
	if err != nil {
		return nil, err
	}

	args = args[1:len(args)]

	for _, arg := range args {
		element, err := ev.Eval(arg)
		if err != nil {
			return nil, err
		}
		result, err := arithmeticComparison(op, prev, element)
		if err != nil {
			return nil, err
		}
		if !result {
			return &NilNode{}, nil
		}
		prev = element
	}

	return &TrueNode{}, nil
}

func funcEqual(ev *evaluator, arglist node) (node, error) {
	if !isProperList(arglist) {
		return nil, fmt.Errorf("Wrong type argument.")
	}

	args, err := createSliceFromProperList(arglist)
	if err != nil {
		return nil, err
	}
	if len(args) < 2 {
		return nil, fmt.Errorf("Wrong number of arguments.")
	}

	return compareAdjacentArguments(ev, args, arithmeticComparisonEqual)
}

func funcNotEqual(ev *evaluator, arglist node) (node, error) {
	if !isProperList(arglist) {
		return nil, fmt.Errorf("Wrong type argument.")
	}

	args, err := createSliceFromProperList(arglist)
	if err != nil {
		return nil, err
	}
	if len(args) < 2 {
		return nil, fmt.Errorf("Wrong number of arguments.")
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
		return nil, fmt.Errorf("Wrong type argument.")
	}

	args, err := createSliceFromProperList(arglist)
	if err != nil {
		return nil, err
	}
	if len(args) < 2 {
		return nil, fmt.Errorf("Wrong number of arguments.")
	}

	return compareAdjacentArguments(ev, args, arithmeticComparisonGreaterThan)
}

func funcGreaterThanOrEqualTo(ev *evaluator, arglist node) (node, error) {
	if !isProperList(arglist) {
		return nil, fmt.Errorf("Wrong type argument.")
	}

	args, err := createSliceFromProperList(arglist)
	if err != nil {
		return nil, err
	}
	if len(args) < 2 {
		return nil, fmt.Errorf("Wrong number of arguments.")
	}

	return compareAdjacentArguments(ev, args, arithmeticComparisonGreaterThanOrEqualTo)
}

func funcLessThan(ev *evaluator, arglist node) (node, error) {
	if !isProperList(arglist) {
		return nil, fmt.Errorf("Wrong type argument.")
	}

	args, err := createSliceFromProperList(arglist)
	if err != nil {
		return nil, err
	}
	if len(args) < 2 {
		return nil, fmt.Errorf("Wrong number of arguments.")
	}

	return compareAdjacentArguments(ev, args, arithmeticComparisonLessThan)
}

func funcLessThanOrEqualTo(ev *evaluator, arglist node) (node, error) {
	if !isProperList(arglist) {
		return nil, fmt.Errorf("Wrong type argument.")
	}

	args, err := createSliceFromProperList(arglist)
	if err != nil {
		return nil, err
	}
	if len(args) < 2 {
		return nil, fmt.Errorf("Wrong number of arguments.")
	}

	return compareAdjacentArguments(ev, args, arithmeticComparisonLessThanOrEqualTo)
}

func funcAnd(ev *evaluator, arglist node) (node, error) {
	if !isProperList(arglist) {
		return nil, fmt.Errorf("Wrong type argument.")
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
		return nil, fmt.Errorf("Wrong type argument.")
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
		return nil, fmt.Errorf("Wrong type argument.")
	}

	args, err := createSliceFromProperList(arglist)
	if err != nil {
		return nil, err
	}
	if len(args) != 1 {
		return nil, fmt.Errorf("Wrong number of arguments.")
	}

	arg0 := args[0]

	result, err := ev.Eval(arg0)
	if err != nil {
		return nil, err
	}
	_, ok := result.(*NilNode)
	if ok {
		return &TrueNode{}, nil
	} else {
		return &NilNode{}, nil
	}
}
