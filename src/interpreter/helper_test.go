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
				&intNode{value: 1},
				&intNode{value: 2},
			},
			"(1 2)",
		},
		{
			[]node{
				&intNode{value: 1},
				&intNode{value: 2},
				&consCell{
					car: &intNode{value: 3},
					cdr: &consCell{
						car: &intNode{value: 4},
						cdr: &nilNode{},
					},
				},
			},
			"(1 2 (3 4))",
		},
	}

	for _, c := range testCases {
		result := createList(c.elements).toString()
		if result != c.expected {
			t.Errorf("Result: %v, Expected: %v", result, c.expected)
		}
	}
}
