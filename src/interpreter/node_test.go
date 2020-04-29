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

func TestIsList(t *testing.T) {
// TODO nilは？
	{
		c := &ConsCell{
			car: &IntNode{value: 1},
			cdr: &ConsCell{
				car: &IntNode{value: 2},
				cdr: &NilNode{},
			},
		}
		if c.isList() == false {
			t.Errorf("Proper list: expected: true, result: false")
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
		if c.isList() == true {
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
		if c.isList() == true {
			t.Errorf("Circular list: expected: false, result: true")
		}
	}
}
