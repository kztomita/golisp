package golisp

import (
	"testing"
)

func TestEvalPlus(t *testing.T) {
	ev := newEvaluator()

	{
		result := ev.eval(&intNode{value: 1})
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
		result := ev.eval(c)
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
		result := ev.eval(c)
		if result != nil {
			t.Logf("%v", result.toString());
		}
	}

}

func TestEvalCar(t *testing.T) {
	ev := newEvaluator()

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
	result := ev.eval(&consCell{
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

func TestEvalCdr(t *testing.T) {
	ev := newEvaluator()

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
	result := ev.eval(&consCell{
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

func TestEvalSetq(t *testing.T) {
	ev := newEvaluator()

	// (setq foo 100)
	result := ev.eval(&consCell{
		car: &symbolNode{name: "setq"},
		cdr: &consCell{
			car: &symbolNode{name: "foo"},
			cdr: &consCell{
				car: &intNode{value: 100},
				cdr: &nilNode{},
			},
		},
	})
	if result != nil {
		t.Logf("%v", result.toString())
	}

	t.Logf("%v", ev.eval(&symbolNode{name: "foo"}))
}

func TestFunc(t *testing.T) {
	ev := newEvaluator()

	// 関数の定義
	// (defun foo () (+ 1 2))
	fdef := createList([]node{
		&symbolNode{name: "defun"},
		&symbolNode{name: "foo"},
		&nilNode{},
		createList([]node{
			&symbolNode{name: "+"},
			&intNode{value: 1},
			&intNode{value: 2},
		}),
	})
	//t.Logf("%v", f.toString())

	result := ev.eval(fdef)
	if result != nil {
		t.Logf("%v", result.toString())
	}

	// 関数の実行
	// (foo)
	result = ev.eval(createList([]node{
		&symbolNode{name: "foo"},
	}))
	t.Logf("%v", result.toString())
}
