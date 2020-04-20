package interpreter

import (
	"fmt"
)

type arithmeticOperationType int
const (
	arithmeticOpAdd arithmeticOperationType = iota
	arithmeticOpSubtract
	arithmeticOpMultiply
	arithmeticOpDivide
)

func arithmeticOp(op arithmeticOperationType, a node, b node) (node, error) {
	if !isNumberNode(a) {
		return nil, fmt.Errorf("a is not a number.")
	}
	if !isNumberNode(b) {
		return nil, fmt.Errorf("b is not a number.")
	}

	aIntNode, aOk := a.(*IntNode)
	bIntNode, bOk := b.(*IntNode)
	if aOk && bOk {
		switch op {
		case arithmeticOpAdd:
			return &IntNode{value: aIntNode.value + bIntNode.value}, nil
		case arithmeticOpSubtract:
			return &IntNode{value: aIntNode.value - bIntNode.value}, nil
		case arithmeticOpMultiply:
			return &IntNode{value: aIntNode.value * bIntNode.value}, nil
		case arithmeticOpDivide:
			if bIntNode.value == 0 {
				return nil, fmt.Errorf("Division by zero.")
			}
			return &IntNode{value: aIntNode.value / bIntNode.value}, nil
		default:
			return nil, fmt.Errorf("Unknown operation.")
		}
	}

	var aFloat float64
	switch nd := a.(type) {
	case *IntNode:
		aFloat = float64(nd.value)
	case *FloatNode:
		aFloat = nd.value
	default:
		return nil, fmt.Errorf("Logic Error. Unknown type.")
	}

	var bFloat float64
	switch nd := b.(type) {
	case *IntNode:
		bFloat = float64(nd.value)
	case *FloatNode:
		bFloat = nd.value
	default:
		return nil, fmt.Errorf("Logic Error. Unknown type.")
	}

	switch op {
	case arithmeticOpAdd:
		return &FloatNode{value: aFloat + bFloat}, nil
	case arithmeticOpSubtract:
		return &FloatNode{value: aFloat - bFloat}, nil
	case arithmeticOpMultiply:
		return &FloatNode{value: aFloat * bFloat}, nil
	case arithmeticOpDivide:
		if bFloat == 0.0 {
			return nil, fmt.Errorf("Division by zero.")
		}
	return &FloatNode{value: aFloat / bFloat}, nil
	default:
		return nil, fmt.Errorf("Unknown operation.")
	}
}

type arithmeticComparisonType int
const (
	arithmeticComparisonEqual arithmeticComparisonType = iota
	arithmeticComparisonNotEqual
	arithmeticComparisonGreaterThan
	arithmeticComparisonGreaterThanOrEqualTo
	arithmeticComparisonLessThan
	arithmeticComparisonLessThanOrEqualTo
)

func arithmeticComparison(cmp arithmeticComparisonType, a node, b node) (bool, error) {
	if !isNumberNode(a) {
		return false, fmt.Errorf("a is not a number.")
	}
	if !isNumberNode(b) {
		return false, fmt.Errorf("b is not a number.")
	}

	switch cmp {
	case arithmeticComparisonNotEqual:			// ==
		result, err := arithmeticComparison(arithmeticComparisonEqual, a, b)
		if err != nil {
			return false, nil
		}
		return !result, nil
	case arithmeticComparisonLessThanOrEqualTo:	// <=
		result, err := arithmeticComparison(arithmeticComparisonEqual, a, b)
		if err != nil {
			return false, nil
		}
		result2, err2 := arithmeticComparison(arithmeticComparisonLessThan, a, b)
		if err2 != nil {
			return false, nil
		}
		return (result || result2), nil
	case arithmeticComparisonGreaterThan:		// >
		result, err := arithmeticComparison(arithmeticComparisonLessThanOrEqualTo, a, b)
		if err != nil {
			return false, nil
		}
		return !result, nil
	case arithmeticComparisonGreaterThanOrEqualTo:	// >=
		result, err := arithmeticComparison(arithmeticComparisonLessThan, a, b)
		if err != nil {
			return false, nil
		}
		return !result, nil
	}

	aIntNode, aOk := a.(*IntNode)
	bIntNode, bOk := b.(*IntNode)
	if aOk && bOk {
		switch cmp {
		case arithmeticComparisonEqual:		// ==
			if aIntNode.value == bIntNode.value {
				return true, nil
			} else {
				return false, nil
			}
		case arithmeticComparisonLessThan:	// <
			if aIntNode.value < bIntNode.value {
				return true, nil
			} else {
				return false, nil
			}
		default:
			return false, fmt.Errorf("Unknown operation.")
		}
	}

	var aFloat float64
	switch nd := a.(type) {
	case *IntNode:
		aFloat = float64(nd.value)
	case *FloatNode:
		aFloat = nd.value
	default:
		return false, fmt.Errorf("Logic Error. Unknown type.")
	}

	var bFloat float64
	switch nd := b.(type) {
	case *IntNode:
		bFloat = float64(nd.value)
	case *FloatNode:
		bFloat = nd.value
	default:
		return false, fmt.Errorf("Logic Error. Unknown type.")
	}

	switch cmp {
	case arithmeticComparisonEqual:		// ==
		if aFloat == bFloat {
			return true, nil
		} else {
			return false, nil
		}
	case arithmeticComparisonLessThan:	// <
		if aFloat < bFloat {
			return true, nil
		} else {
			return false, nil
		}
	default:
		return false, fmt.Errorf("Unknown operation.")
	}
}
