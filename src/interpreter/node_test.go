package interpreter

import (
	"testing"
)

func TestConsCell(t *testing.T) {
	{
		// dot list
		c := ConsCell{car: &IntNode{value: 1}, cdr: &IntNode{value: 2}}
		result := c.ToString()
		t.Logf("%v", result)
		if result != "(1 . 2)" {
			t.Errorf("expected: (1 . 2), result: %v", result)
		}
	}

	{
		// list
		c := ConsCell{
			car: &IntNode{value: 1},
			cdr: &ConsCell{
				car: &IntNode{value: 2},
				cdr: &NilNode{},
			},
		}
		result := c.ToString()
		t.Logf("%v", result)
		if result != "(1 2)" {
			t.Errorf("expected: (1 2), result: %v", result)
		}
	}

	{
		// child list
		c := ConsCell{
			car: &ConsCell{
				car: &IntNode{value: 1},
				cdr: &ConsCell{
					car: &IntNode{value: 2},
					cdr: &NilNode{},
				},
			},
			cdr: &ConsCell{
				car: &IntNode{value: 3},
				cdr: &NilNode{},
			},
		}
		result := c.ToString()
		t.Logf("%v", result)
		if result != "((1 2) 3)" {
			t.Errorf("expected: ((1 2) 3), result: %v", result)
		}
	}

	{
		// listの末尾がdot list
		c := ConsCell{
			car: &IntNode{value: 1},
			cdr: &ConsCell{
				car: &IntNode{value: 2},
				cdr: &IntNode{value: 3},
			},
		}
		result := c.ToString()
		t.Logf("%v", result)
		if result != "(1 2 . 3)" {
			t.Errorf("expected: (1 2 . 3), result: %v", result)
		}
	}
}
