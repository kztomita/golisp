package interpreter

import (
	"fmt"
)

func arithmeticOp(op string, a node, b node) (node, error) {
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
		case "+":
			return &IntNode{value: aIntNode.value + bIntNode.value}, nil
		case "-":
			return &IntNode{value: aIntNode.value - bIntNode.value}, nil
		case "*":
			return &IntNode{value: aIntNode.value * bIntNode.value}, nil
		case "/":
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
	case "+":
		return &FloatNode{value: aFloat + bFloat}, nil
	case "-":
		return &FloatNode{value: aFloat - bFloat}, nil
	case "*":
		return &FloatNode{value: aFloat * bFloat}, nil
	case "/":
		if bFloat == 0.0 {
			return nil, fmt.Errorf("Division by zero.")
		}
	return &FloatNode{value: aFloat / bFloat}, nil
	default:
		return nil, fmt.Errorf("Unknown operation.")
	}
}

func arithmeticComparisonOp(op string, a node, b node) (bool, error) {
	if !isNumberNode(a) {
		return false, fmt.Errorf("a is not a number.")
	}
	if !isNumberNode(b) {
		return false, fmt.Errorf("b is not a number.")
	}

	switch op {
	case "!=":
		result, err := arithmeticComparisonOp("==", a, b)
		if err != nil {
			return false, nil
		}
		return !result, nil
	case "<=":
		result, err := arithmeticComparisonOp("==", a, b)
		if err != nil {
			return false, nil
		}
		result2, err2 := arithmeticComparisonOp("<", a, b)
		if err2 != nil {
			return false, nil
		}
		return (result || result2), nil
	case ">":
		result, err := arithmeticComparisonOp("<=", a, b)
		if err != nil {
			return false, nil
		}
		return !result, nil
	case ">=":
		result, err := arithmeticComparisonOp("<", a, b)
		if err != nil {
			return false, nil
		}
		return !result, nil
	}

	aIntNode, aOk := a.(*IntNode)
	bIntNode, bOk := b.(*IntNode)
	if aOk && bOk {
		switch op {
		case "==":
			if aIntNode.value == bIntNode.value {
				return true, nil
			} else {
				return false, nil
			}
		case "<":
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

	switch op {
	case "==":
		if aFloat == bFloat {
			return true, nil
		} else {
			return false, nil
		}
	case "<":
		if aFloat < bFloat {
			return true, nil
		} else {
			return false, nil
		}
	default:
		return false, fmt.Errorf("Unknown operation.")
	}
}
