package golisp

import (
	"testing"
)

func TestEval(t *testing.T) {
	{
		result := eval(&intNode{value: 1})
		if result != nil {
			t.Logf("%v", result.toString());
		}
	}

	{
		c := &consCell{
			car: &symbolNode{name: "+"},
			cdr: &consCell{
				car: &intNode{value: 1},
				cdr: &consCell{
					car: &intNode{value: 2},
					cdr: &nilNode{},
				},
			},
		}
		expr := c.toString()
		t.Logf("%v", expr)
		result := eval(c)
		if result != nil {
			t.Logf("%v", result.toString());
		}
	}

	{
		c := &consCell{
			car: &symbolNode{name: "+"},
			cdr: &consCell{
				car: &consCell{
					car: &symbolNode{name: "+"},
					cdr: &consCell{
						car: &intNode{value: 1},
						cdr: &consCell{
							car: &intNode{value: 2},
							cdr: &nilNode{},
						},
					},
				},
				cdr: &consCell{
					car: &intNode{value: 3},
					cdr: &nilNode{},
				},
			},
		}
		expr := c.toString()
		t.Logf("%v", expr)
		result := eval(c)
		if result != nil {
			t.Logf("%v", result.toString());
		}
	}

}

func TestCar(t *testing.T) {
	// list
	list := &consCell{
		car: &intNode{value: 1},
		cdr: &consCell{
			car: &intNode{value: 2},
			cdr: &consCell{
				car: &intNode{value: 3},
				cdr: &nilNode{},
			},
		},
	}
	expr := list.toString()
	t.Logf("%v", expr)

	// (car '(1 2 3))
	result := eval(&consCell{
		car: &symbolNode{name: "car"},
		cdr: &consCell{
			car: list,
			cdr: &nilNode{},
		},
	})
	if result != nil {
		t.Logf("%v", result.toString())
	}
}

func TestCdr(t *testing.T) {
	// list
	list := &consCell{
		car: &intNode{value: 1},
		cdr: &consCell{
			car: &intNode{value: 2},
			cdr: &consCell{
				car: &intNode{value: 3},
				cdr: &nilNode{},
			},
		},
	}
	expr := list.toString()
	t.Logf("%v", expr)

	// (cdr '(1 2 3))
	result := eval(&consCell{
		car: &symbolNode{name: "cdr"},
		cdr: &consCell{
			car: list,
			cdr: &nilNode{},
		},
	})
	if result != nil {
		t.Logf("%v", result.toString())
	}
}
