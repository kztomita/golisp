package interpreter

import (
	"testing"
)

func TestCreateList(t *testing.T) {
	testCases := []struct{
		elements	[]node
		expected	string
	}{
		{
			[]node{},
			"nil",
		},
		{
			[]node{
				&IntNode{value: 1},
				&IntNode{value: 2},
			},
			"(1 2)",
		},
		{
			[]node{
				&IntNode{value: 1},
				&IntNode{value: 2},
				&ConsCell{
					car: &IntNode{value: 3},
					cdr: &ConsCell{
						car: &IntNode{value: 4},
						cdr: &NilNode{},
					},
				},
			},
			"(1 2 (3 4))",
		},
	}

	for _, c := range testCases {
		result := createList(c.elements).ToString()
		if result != c.expected {
			t.Errorf("Result: %v, Expected: %v", result, c.expected)
		}
	}
}
