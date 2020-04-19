package interpreter

import (
	"testing"
)

func TestArithmeticOp(t *testing.T) {
	testCases := []struct{
		op			string
		a			node
		b			node
		expected	node
	}{
		{
			"+",
			&IntNode{value: 1},
			&IntNode{value: 2},
			&IntNode{value: 3},
		},
		{
			"+",
			&IntNode{value: 1},
			&FloatNode{value: 2.5},
			&FloatNode{value: 3.5},
		},
		{
			"+",
			&FloatNode{value: 1.5},
			&IntNode{value: 2},
			&FloatNode{value: 3.5},
		},
		{
			"+",
			&FloatNode{value: 1.5},
			&FloatNode{value: 2.5},
			&FloatNode{value: 4.0},
		},
		{
			"-",
			&IntNode{value: 1},
			&IntNode{value: 2},
			&IntNode{value: -1},
		},
		{
			"-",
			&IntNode{value: 1},
			&FloatNode{value: 2.5},
			&FloatNode{value: -1.5},
		},
		{
			"*",
			&IntNode{value: 2},
			&IntNode{value: 3},
			&IntNode{value: 6},
		},
		{
			"*",
			&IntNode{value: 2},
			&FloatNode{value: 2.5},
			&FloatNode{value: 5.0},
		},
		{
			"/",
			&IntNode{value: 7},
			&IntNode{value: 2},
			&IntNode{value: 3},
		},
		{
			"/",
			&IntNode{value: 7},
			&FloatNode{value: 2.0},
			&FloatNode{value: 3.5},
		},
	}

	for _, c := range testCases {
		result, err := arithmeticOp(c.op, c.a, c.b)
		if err != nil {
			t.Errorf("%v", err)
			continue
		}
		if result.GetNodeType() != c.expected.GetNodeType() ||
				result.ToString() != c.expected.ToString() {
			t.Errorf("Result: %v, Expected: %v", result, c.expected)
			continue
		}
	}
}

func TestArithmeticComparisonOp(t *testing.T) {
	testCases := []struct{
		op			string
		a			node
		b			node
		expected	bool
	}{
		{
			"==",
			&IntNode{value: 1},
			&IntNode{value: 1},
			true,
		},
		{
			"==",
			&IntNode{value: 1},
			&IntNode{value: 2},
			false,
		},
		{
			"==",
			&IntNode{value: 1},
			&FloatNode{value: 1.0},
			true,
		},
		{
			"==",
			&FloatNode{value: 1.0},
			&FloatNode{value: 1.0},
			true,
		},
		{
			"!=",
			&IntNode{value: 1},
			&IntNode{value: 1},
			false,
		},
		{
			"!=",
			&IntNode{value: 1},
			&IntNode{value: 2},
			true,
		},
		{
			"<",
			&IntNode{value: 1},
			&IntNode{value: 2},
			true,
		},
		{
			"<",
			&IntNode{value: 2},
			&IntNode{value: 1},
			false,
		},
		{
			"<",
			&IntNode{value: 2},
			&IntNode{value: 2},
			false,
		},
		{
			"<=",
			&IntNode{value: 1},
			&IntNode{value: 2},
			true,
		},
		{
			"<=",
			&IntNode{value: 2},
			&IntNode{value: 1},
			false,
		},
		{
			"<=",
			&IntNode{value: 2},
			&IntNode{value: 2},
			true,
		},
		{
			">",
			&IntNode{value: 1},
			&IntNode{value: 2},
			false,
		},
		{
			">",
			&IntNode{value: 2},
			&IntNode{value: 1},
			true,
		},
		{
			">",
			&IntNode{value: 2},
			&IntNode{value: 2},
			false,
		},
		{
			">=",
			&IntNode{value: 1},
			&IntNode{value: 2},
			false,
		},
		{
			">=",
			&IntNode{value: 2},
			&IntNode{value: 1},
			true,
		},
		{
			">=",
			&IntNode{value: 2},
			&IntNode{value: 2},
			true,
		},
	}

	for _, c := range testCases {
		result, err := arithmeticComparisonOp(c.op, c.a, c.b)
		if err != nil {
			t.Errorf("%v", err)
			continue
		}
		if result != c.expected {
			t.Errorf("Result: %v, Expected: %v", result, c.expected)
			continue
		}
	}
}
