package golisp

import (
	"testing"
)

func TestEvalPlus(t *testing.T) {
	ev := newEvaluator()

	{
		result, err := ev.eval(&intNode{value: 1})
		if err != nil {
			t.Errorf("%v", err)
		} else {
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
		result, err := ev.eval(c)
		if err != nil {
			t.Errorf("%v", err)
		} else {
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
		result, err := ev.eval(c)
		if err != nil {
			t.Errorf("%v", err)
		} else {
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
	result, err := ev.eval(&consCell{
		car: &symbolNode{name: "car"},
		cdr: &consCell{
			car: list,
			cdr: &nilNode{},
		},
	})
	if err != nil {
		t.Errorf("%v", err)
	} else {
		t.Logf("%v", result.toString());
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
	result, err := ev.eval(&consCell{
		car: &symbolNode{name: "cdr"},
		cdr: &consCell{
			car: list,
			cdr: &nilNode{},
		},
	})
	if err != nil {
		t.Errorf("%v", err)
	} else {
		t.Logf("%v", result.toString());
	}
}

func TestEvalSetq(t *testing.T) {
	ev := newEvaluator()

	// (setq foo 100)
	result, err := ev.eval(&consCell{
		car: &symbolNode{name: "setq"},
		cdr: &consCell{
			car: &symbolNode{name: "foo"},
			cdr: &consCell{
				car: &intNode{value: 100},
				cdr: &nilNode{},
			},
		},
	})
	if err != nil {
		t.Errorf("%v", err)
	} else {
		t.Logf("%v", result.toString());
	}

	result, err = ev.eval(&symbolNode{name: "foo"})
	if err != nil {
		t.Errorf("%v", err)
	} else {
		t.Logf("%v", result.toString());
	}
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

	result, err := ev.eval(fdef)
	if err != nil {
		t.Errorf("%v", err)
	} else {
		t.Logf("%v", result.toString());
	}

	// 関数の実行
	// (foo)
	result, err = ev.eval(createList([]node{
		&symbolNode{name: "foo"},
	}))
	if err != nil {
		t.Errorf("%v", err)
	} else {
		t.Logf("%v", result.toString());
	}
}
func TestFunc2(t *testing.T) {
	ev := newEvaluator()

	{
		// (setq c 100)
		_, err := ev.eval(&consCell{
			car: &symbolNode{name: "setq"},
			cdr: &consCell{
				car: &symbolNode{name: "c"},
				cdr: &consCell{
					car: &intNode{value: 100},
					cdr: &nilNode{},
				},
			},
		})
		if err != nil {
			t.Errorf("%v", err)
		}
	}

	{
		// 関数の定義
		// (defun foo (a b) (+ a b))
		fdef := createList([]node{
			&symbolNode{name: "defun"},
			&symbolNode{name: "foo"},
			createList([]node{
				&symbolNode{name: "a"},
				&symbolNode{name: "b"},
			}),
			createList([]node{
				&symbolNode{name: "+"},
				&symbolNode{name: "a"},
				&symbolNode{name: "b"},
				&symbolNode{name: "c"},		// 外部の変数
			}),
		})
		//t.Logf("%v", fdef.toString())

		result, err := ev.eval(fdef)
		if err != nil {
			t.Errorf("%v", err)
		} else {
			t.Logf("%v", result.toString());
		}
	}

	{
		// 関数の実行
		// (foo 1 2)
		result, err := ev.eval(createList([]node{
			&symbolNode{name: "foo"},
			&intNode{value: 1},
			&intNode{value: 2},
		}))
		if err != nil {
			t.Errorf("%v", err)
		} else {
			t.Logf("%v", result.toString());
		}
	}
}

func TestEvalLet(t *testing.T) {
	ev := newEvaluator()

	// (let ((x 100) (y 200)) (+ x y))
	let := createList([]node{
		&symbolNode{name: "let"},
		createList([]node{
			createList([]node{
				&symbolNode{name: "x"},
				&intNode{value: 100},
			}),
			createList([]node{
				&symbolNode{name: "y"},
				&intNode{value: 200},
			}),
		}),
		createList([]node{
			&symbolNode{name: "+"},
			&symbolNode{name: "x"},
			&symbolNode{name: "y"},
		}),
	})
	t.Logf("%v", let.toString())

	result, err := ev.eval(let)
	if err != nil {
		t.Errorf("%v", err)
	} else {
		t.Logf("%v", result.toString());
	}
}

func TestEvalLet2(t *testing.T) {
	ev := newEvaluator()

	{
		// (setq c 100)
		_, err := ev.eval(&consCell{
			car: &symbolNode{name: "setq"},
			cdr: &consCell{
				car: &symbolNode{name: "c"},
				cdr: &consCell{
					car: &intNode{value: 100},
					cdr: &nilNode{},
				},
			},
		})
		if err != nil {
			t.Errorf("%v", err)
		}
	}
	{
		// (setq x 200)
		_, err := ev.eval(&consCell{
			car: &symbolNode{name: "setq"},
			cdr: &consCell{
				car: &symbolNode{name: "x"},
				cdr: &consCell{
					car: &intNode{value: 200},
					cdr: &nilNode{},
				},
			},
		})
		if err != nil {
			t.Errorf("%v", err)
		}
	}

	{
		// 関数の定義
		// (defun foo (a b) (let ((x 100) (y 200)) (+ a b x y c)))
		fdef := createList([]node{
			&symbolNode{name: "defun"},
			&symbolNode{name: "foo"},
			createList([]node{
				&symbolNode{name: "a"},
				&symbolNode{name: "b"},
			}),
			createList([]node{
				&symbolNode{name: "let"},
				createList([]node{
					createList([]node{
						&symbolNode{name: "x"},
						&intNode{value: 100},
					}),
					createList([]node{
						&symbolNode{name: "y"},
						&intNode{value: 200},
					}),
				}),
				createList([]node{
					&symbolNode{name: "+"},
					&symbolNode{name: "a"},
					&symbolNode{name: "b"},
					&symbolNode{name: "x"},
					&symbolNode{name: "y"},
					&symbolNode{name: "c"},
				}),
			}),
		})
		t.Logf("%v", fdef.toString())

		result, err := ev.eval(fdef)
		if err != nil {
			t.Errorf("%v", err)
		} else {
			t.Logf("%v", result.toString());
		}
	}

	{
		// 関数の実行
		// (foo 1 2)
		result, err := ev.eval(createList([]node{
			&symbolNode{name: "foo"},
			&intNode{value: 1},
			&intNode{value: 2},
		}))
		if err != nil {
			t.Errorf("%v", err)
		} else {
			t.Logf("%v", result.toString());
		}

		if result.toString() != "403" {
			t.Errorf("Result: %v", result.toString())
		}
	}
}
