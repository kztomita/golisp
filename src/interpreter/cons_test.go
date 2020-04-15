package golisp

import (
	"testing"
)

func TestFxx(t *testing.T) {
	{
		// dot list
		c := consCell{car: &intNode{value: 1}, cdr: &intNode{value: 2}}
		result := c.toString()
		t.Logf("%v", result)
		if result != "(1 . 2)" {
			t.Errorf("expected: (1 . 2), result: %v", result)
		}
	}

	{
		// list
		c := consCell{
			car: &intNode{value: 1},
			cdr: &consCell{
				car: &intNode{value: 2},
				cdr: &nilNode{},
			},
		}
		result := c.toString()
		t.Logf("%v", result)
		if result != "(1 2)" {
			t.Errorf("expected: (1 2), result: %v", result)
		}
	}

	{
		// child list
		c := consCell{
			car: &consCell{
				car: &intNode{value: 1},
				cdr: &consCell{
					car: &intNode{value: 2},
					cdr: &nilNode{},
				},
			},
			cdr: &consCell{
				car: &intNode{value: 3},
				cdr: &nilNode{},
			},
		}
		result := c.toString()
		t.Logf("%v", result)
		if result != "((1 2) 3)" {
			t.Errorf("expected: ((1 2) 3), result: %v", result)
		}
	}

	{
		// listの末尾がdot list
		c := consCell{
			car: &intNode{value: 1},
			cdr: &consCell{
				car: &intNode{value: 2},
				cdr: &intNode{value: 3},
			},
		}
		result := c.toString()
		t.Logf("%v", result)
		if result != "(1 2 . 3)" {
			t.Errorf("expected: (1 2 . 3), result: %v", result)
		}
	}
}

