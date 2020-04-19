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
