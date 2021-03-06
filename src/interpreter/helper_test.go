package interpreter

import (
	"testing"
)

func TestIsProperList(t *testing.T) {
	{
		c := &ConsCell{
			car: &IntNode{value: 1},
			cdr: &ConsCell{
				car: &IntNode{value: 2},
				cdr: &NilNode{},
			},
		}
		if isProperList(c) == false {
			t.Errorf("Proper list: expected: true, result: false")
		}
	}

	{
		c := &NilNode{}
		if isProperList(c) == false {
			t.Errorf("Proper list(empty list): expected: true, result: false")
		}
	}

	{
		if isProperList(nil) == true {
			t.Errorf("nil: expected: false, result: true")
		}
	}

	{
		c := &IntNode{value: 1}
		if isProperList(c) == true {
			t.Errorf("Not list: expected: false, result: true")
		}
	}

	{
		c := &ConsCell{
			car: &IntNode{value: 1},
			cdr: &ConsCell{
				car: &IntNode{value: 2},
				cdr: &IntNode{value: 3},
			},
		}
		if isProperList(c) == true {
			t.Errorf("Dotted list: expected: false, result: true")
		}
	}

	{
		c := &ConsCell{
			car: &IntNode{value: 1},
			cdr: &ConsCell{
				car: &IntNode{value: 2},
				cdr: nil,
			},
		}
		c.cdr.(*ConsCell).cdr = c
		if isProperList(c) == true {
			t.Errorf("Circular list: expected: false, result: true")
		}
	}
}

func TestCountProperListLength(t *testing.T) {
	testCases := []struct{
		list		node
		expected	int
	}{
		{
			&NilNode{},
			0,
		},
		{
			&ConsCell{
				car: &IntNode{value: 1},
				cdr: &ConsCell{
					car: &IntNode{value: 2},
					cdr: &NilNode{},
				},
			},
			2,
		},
		{
			// dotted list
			&ConsCell{
				car: &IntNode{value: 1},
				cdr: &IntNode{value: 2},
			},
			0,
		},
	}

	for _, c := range testCases {
		result := countProperListLength(c.list)
		if result != c.expected {
			t.Errorf("Result: %v, Expected: %v", result, c.expected)
		}
	}
}

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

func TestCreateDotList(t *testing.T) {
	testCases := []struct{
		elements	[]node
		expected	string
	}{
		{
			[]node{
				&IntNode{value: 1},
				&IntNode{value: 2},
			},
			"(1 . 2)",
		},
		{
			[]node{
				&IntNode{value: 1},
				&IntNode{value: 2},
				&IntNode{value: 3},
			},
			"(1 2 . 3)",
		},
	}

	for _, c := range testCases {
		result := createDotList(c.elements).ToString()
		if result != c.expected {
			t.Errorf("Result: %v, Expected: %v", result, c.expected)
		}
	}
}

func TestCreateSliceFromProperList(t *testing.T) {
	testCases := []struct{
		list		node
		err			bool
		expected	[]node
	}{
		{
			&ConsCell{
				car: &IntNode{value: 3},
				cdr: &ConsCell{
					car: &IntNode{value: 4},
					cdr: &ConsCell{
						car: &IntNode{value: 5},
						cdr: &NilNode{},
					},
				},
			},
			false,
			[]node{
				&IntNode{value: 3},
				&IntNode{value: 4},
				&IntNode{value: 5},
			},
		},
		{
			&ConsCell{
				car: &IntNode{value: 3},
				cdr: &ConsCell{
					car: &IntNode{value: 4},
					cdr: &IntNode{value: 5},
				},
			},
			true,
			[]node{},
		},
		{
			&NilNode{},
			false,
			[]node{},
		},
		{
			nil,
			true,
			[]node{},
		},
	}

	for _, c := range testCases {
		result, err := createSliceFromProperList(c.list)
		if c.err == false && err != nil {
			t.Errorf("Expected err is nil. Result: %v", err)
			continue
		} else if c.err == true && err == nil {
			t.Errorf("Expected err , but err is nil.")
			continue
		}
		if len(result) != len(c.expected) {
			t.Errorf("Result size is incorrect. Result: %v, Expected: %v", len(result), len(c.expected))
		}
		for i := range result {
			if result[i].GetNodeType() != c.expected[i].GetNodeType() ||
			   result[i].ToString() != c.expected[i].ToString() {
				t.Errorf("Index %v: Result: %v, Expected: %v", i, result[i], c.expected[i])

			}
		}
	}
}

func TestCopyProperList(t *testing.T) {
	testCases := []struct{
		list		node
		expected	string
	}{
		{
			&ConsCell{
				car: &IntNode{value: 1},
				cdr: &ConsCell{
					car: &IntNode{value: 2},
					cdr: &ConsCell{
						car: &IntNode{value: 3},
						cdr: &NilNode{},
					},
				},
			},
			"(1 2 3)",
		},
		{
			&ConsCell{
				car: &IntNode{value: 1},
				cdr: &ConsCell{
					car: &ConsCell{
						car: &IntNode{value: 2},
						cdr: &ConsCell{
							car: &IntNode{value: 3},
							cdr: &NilNode{},
							},
						},
					cdr: &ConsCell{
						car: &IntNode{value: 4},
						cdr: &NilNode{},
					},
				},
			},
			"(1 (2 3) 4)",
		},
	}

	for _, c := range testCases {
		result, err := copyProperList(c.list)
		if err != nil {
			t.Errorf("%v", err)
			continue
		}
		if result.ToString() != c.expected {
			t.Errorf("Result: %v, Expected: %v", result.ToString(), c.expected)
		}
	}
}
