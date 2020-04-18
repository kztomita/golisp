package interpreter

import (
	"testing"
)

func TestEvalPlus(t *testing.T) {
	testCases := []struct{
		expr        node
		expected    string
	}{
		{
			&ConsCell{
				car: &SymbolNode{name: "+"},
				cdr: &ConsCell{
					car: &IntNode{value: 1},
					cdr: &ConsCell{
						car: &IntNode{value: 2},
						cdr: &NilNode{},
					},
				},
			},
			"3",
		},
		{
			&ConsCell{
				car: &SymbolNode{name: "+"},
				cdr: &ConsCell{
					car: &ConsCell{
						car: &SymbolNode{name: "+"},
						cdr: &ConsCell{
							car: &IntNode{value: 1},
							cdr: &ConsCell{
								car: &IntNode{value: 2},
								cdr: &NilNode{},
							},
						},
					},
					cdr: &ConsCell{
						car: &IntNode{value: 3},
						cdr: &NilNode{},
					},
				},
			},
			"6",
		},
	}

	ev := NewEvaluator()

	for _, c := range testCases {
		t.Logf("%v", c.expr.ToString());
		result, err := ev.Eval(c.expr)
		if err != nil {
			t.Errorf("%v", err)
			continue
		}
		if result.ToString() != c.expected {
			t.Errorf("Result: %v, Expected: %v", result.ToString(), c.expected)
			continue
		}
	}
}

func TestEvalMinus(t *testing.T) {
	testCases := []struct{
		expr        node
		expected    string
	}{
		{
			createList([]node{
				&SymbolNode{name: "-"},
				&IntNode{value: 3},
				&IntNode{value: 1},
			}),
			"2",
		},
		{
			createList([]node{
				&SymbolNode{name: "-"},
				&IntNode{value: 6},
				&IntNode{value: 3},
				&IntNode{value: 1},
			}),
			"2",
		},
	}

	ev := NewEvaluator()

	for _, c := range testCases {
		result, err := ev.Eval(c.expr)
		if err != nil {
			t.Errorf("%v", err)
			continue
		}
		if result.ToString() != c.expected {
			t.Errorf("Result: %v, Expected: %v", result.ToString(), c.expected)
			continue
		}
	}
}

func TestEvalEqual(t *testing.T) {
	testCases := []struct{
		expr        node
		expected    string
	}{
		{
			createList([]node{
				&SymbolNode{name: "="},
				&IntNode{value: 3},
				&IntNode{value: 3},
			}),
			"t",
		},
		{
			createList([]node{
				&SymbolNode{name: "="},
				&IntNode{value: 3},
				&IntNode{value: 2},
			}),
			"nil",
		},
		{
			createList([]node{
				&SymbolNode{name: "="},
				&IntNode{value: 6},
				&IntNode{value: 6},
				&IntNode{value: 6},
			}),
			"t",
		},
		{
			createList([]node{
				&SymbolNode{name: "="},
				&IntNode{value: 6},
				&IntNode{value: 6},
				&IntNode{value: 5},
			}),
			"nil",
		},
	}

	ev := NewEvaluator()

	for _, c := range testCases {
		result, err := ev.Eval(c.expr)
		if err != nil {
			t.Errorf("%v", err)
			continue
		}
		if result.ToString() != c.expected {
			t.Errorf("Result: %v, Expected: %v", result.ToString(), c.expected)
			continue
		}
	}
}

func TestEvalCar(t *testing.T) {
	ev := NewEvaluator()

	// list
	list := &ConsCell{
		car: &IntNode{value: 1},
		cdr: &ConsCell{
			car: &IntNode{value: 2},
			cdr: &ConsCell{
				car: &IntNode{value: 3},
				cdr: &NilNode{},
			},
		},
	}
	expr := list.ToString()
	t.Logf("%v", expr)

	// (car '(1 2 3))
	result, err := ev.Eval(&ConsCell{
		car: &SymbolNode{name: "car"},
		cdr: &ConsCell{
			car: list,
			cdr: &NilNode{},
		},
	})
	if err != nil {
		t.Errorf("%v", err)
	} else {
		t.Logf("%v", result.ToString());
	}
}

func TestEvalCdr(t *testing.T) {
	ev := NewEvaluator()

	// list
	list := &ConsCell{
		car: &IntNode{value: 1},
		cdr: &ConsCell{
			car: &IntNode{value: 2},
			cdr: &ConsCell{
				car: &IntNode{value: 3},
				cdr: &NilNode{},
			},
		},
	}
	expr := list.ToString()
	t.Logf("%v", expr)

	// (cdr '(1 2 3))
	result, err := ev.Eval(&ConsCell{
		car: &SymbolNode{name: "cdr"},
		cdr: &ConsCell{
			car: list,
			cdr: &NilNode{},
		},
	})
	if err != nil {
		t.Errorf("%v", err)
	} else {
		t.Logf("%v", result.ToString());
	}
}

func TestEvalAnd(t *testing.T) {
	testCases := []struct{
		expr        node
		expected    string
	}{
		{
			createList([]node{
				&SymbolNode{name: "and"},
			}),
			"t",
		},
		{
			createList([]node{
				&SymbolNode{name: "and"},
				&TrueNode{},
				&TrueNode{},
			}),
			"t",
		},
		{
			createList([]node{
				&SymbolNode{name: "and"},
				&NilNode{},
				&TrueNode{},
			}),
			"nil",
		},
	}

	ev := NewEvaluator()

	for _, c := range testCases {
		result, err := ev.Eval(c.expr)
		if err != nil {
			t.Errorf("%v", err)
			continue
		}
		if result.ToString() != c.expected {
			t.Errorf("Result: %v, Expected: %v", result.ToString(), c.expected)
			continue
		}
	}
}

func TestEvalOr(t *testing.T) {
	testCases := []struct{
		expr        node
		expected    string
	}{
		{
			createList([]node{
				&SymbolNode{name: "or"},
			}),
			"nil",
		},
		{
			createList([]node{
				&SymbolNode{name: "or"},
				&NilNode{},
				&TrueNode{},
			}),
			"t",
		},
		{
			createList([]node{
				&SymbolNode{name: "or"},
				&NilNode{},
				&NilNode{},
			}),
			"nil",
		},
	}

	ev := NewEvaluator()

	for _, c := range testCases {
		result, err := ev.Eval(c.expr)
		if err != nil {
			t.Errorf("%v", err)
			continue
		}
		if result.ToString() != c.expected {
			t.Errorf("Result: %v, Expected: %v", result.ToString(), c.expected)
			continue
		}
	}
}

func TestEvalSetq(t *testing.T) {
	ev := NewEvaluator()

	// (setq foo 100)
	result, err := ev.Eval(&ConsCell{
		car: &SymbolNode{name: "setq"},
		cdr: &ConsCell{
			car: &SymbolNode{name: "foo"},
			cdr: &ConsCell{
				car: &IntNode{value: 100},
				cdr: &NilNode{},
			},
		},
	})
	if err != nil {
		t.Errorf("%v", err)
	} else {
		t.Logf("%v", result.ToString());
	}

	result, err = ev.Eval(&SymbolNode{name: "foo"})
	if err != nil {
		t.Errorf("%v", err)
	} else {
		t.Logf("%v", result.ToString());
	}
}

func TestFunc(t *testing.T) {
	ev := NewEvaluator()

	// 関数の定義
	// (defun foo () (+ 1 2))
	fdef := createList([]node{
		&SymbolNode{name: "defun"},
		&SymbolNode{name: "foo"},
		&NilNode{},
		createList([]node{
			&SymbolNode{name: "+"},
			&IntNode{value: 1},
			&IntNode{value: 2},
		}),
	})
	//t.Logf("%v", f.ToString())

	result, err := ev.Eval(fdef)
	if err != nil {
		t.Errorf("%v", err)
	} else {
		t.Logf("%v", result.ToString());
	}

	// 関数の実行
	// (foo)
	result, err = ev.Eval(createList([]node{
		&SymbolNode{name: "foo"},
	}))
	if err != nil {
		t.Errorf("%v", err)
	} else {
		t.Logf("%v", result.ToString());
	}
}
func TestFunc2(t *testing.T) {
	ev := NewEvaluator()

	{
		// (setq c 100)
		_, err := ev.Eval(&ConsCell{
			car: &SymbolNode{name: "setq"},
			cdr: &ConsCell{
				car: &SymbolNode{name: "c"},
				cdr: &ConsCell{
					car: &IntNode{value: 100},
					cdr: &NilNode{},
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
			&SymbolNode{name: "defun"},
			&SymbolNode{name: "foo"},
			createList([]node{
				&SymbolNode{name: "a"},
				&SymbolNode{name: "b"},
			}),
			createList([]node{
				&SymbolNode{name: "+"},
				&SymbolNode{name: "a"},
				&SymbolNode{name: "b"},
				&SymbolNode{name: "c"},		// 外部の変数
			}),
		})
		//t.Logf("%v", fdef.ToString())

		result, err := ev.Eval(fdef)
		if err != nil {
			t.Errorf("%v", err)
		} else {
			t.Logf("%v", result.ToString());
		}
	}

	{
		// 関数の実行
		// (foo 1 2)
		result, err := ev.Eval(createList([]node{
			&SymbolNode{name: "foo"},
			&IntNode{value: 1},
			&IntNode{value: 2},
		}))
		if err != nil {
			t.Errorf("%v", err)
		} else {
			t.Logf("%v", result.ToString());
		}
	}
}

func TestEvalLet(t *testing.T) {
	ev := NewEvaluator()

	// (let ((x 100) (y 200)) (+ x y))
	let := createList([]node{
		&SymbolNode{name: "let"},
		createList([]node{
			createList([]node{
				&SymbolNode{name: "x"},
				&IntNode{value: 100},
			}),
			createList([]node{
				&SymbolNode{name: "y"},
				&IntNode{value: 200},
			}),
		}),
		createList([]node{
			&SymbolNode{name: "+"},
			&SymbolNode{name: "x"},
			&SymbolNode{name: "y"},
		}),
	})
	t.Logf("%v", let.ToString())

	result, err := ev.Eval(let)
	if err != nil {
		t.Errorf("%v", err)
	} else {
		t.Logf("%v", result.ToString());
	}
}

func TestEvalLet2(t *testing.T) {
	ev := NewEvaluator()

	{
		// (setq c 100)
		_, err := ev.Eval(&ConsCell{
			car: &SymbolNode{name: "setq"},
			cdr: &ConsCell{
				car: &SymbolNode{name: "c"},
				cdr: &ConsCell{
					car: &IntNode{value: 100},
					cdr: &NilNode{},
				},
			},
		})
		if err != nil {
			t.Errorf("%v", err)
		}
	}
	{
		// (setq x 200)
		_, err := ev.Eval(&ConsCell{
			car: &SymbolNode{name: "setq"},
			cdr: &ConsCell{
				car: &SymbolNode{name: "x"},
				cdr: &ConsCell{
					car: &IntNode{value: 200},
					cdr: &NilNode{},
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
			&SymbolNode{name: "defun"},
			&SymbolNode{name: "foo"},
			createList([]node{
				&SymbolNode{name: "a"},
				&SymbolNode{name: "b"},
			}),
			createList([]node{
				&SymbolNode{name: "let"},
				createList([]node{
					createList([]node{
						&SymbolNode{name: "x"},
						&IntNode{value: 100},
					}),
					createList([]node{
						&SymbolNode{name: "y"},
						&IntNode{value: 200},
					}),
				}),
				createList([]node{
					&SymbolNode{name: "+"},
					&SymbolNode{name: "a"},
					&SymbolNode{name: "b"},
					&SymbolNode{name: "x"},
					&SymbolNode{name: "y"},
					&SymbolNode{name: "c"},
				}),
			}),
		})
		t.Logf("%v", fdef.ToString())

		result, err := ev.Eval(fdef)
		if err != nil {
			t.Errorf("%v", err)
		} else {
			t.Logf("%v", result.ToString());
		}
	}

	{
		// 関数の実行
		// (foo 1 2)
		result, err := ev.Eval(createList([]node{
			&SymbolNode{name: "foo"},
			&IntNode{value: 1},
			&IntNode{value: 2},
		}))
		if err != nil {
			t.Errorf("%v", err)
		} else {
			t.Logf("%v", result.ToString());
		}

		if result.ToString() != "403" {
			t.Errorf("Result: %v", result.ToString())
		}
	}
}

func TestEvalProgn(t *testing.T) {
	ev := NewEvaluator()

	{
		// (progn (setq c 100) (setq d 200))
		_, err := ev.Eval(createList([]node{
			&SymbolNode{name: "progn"},
			createList([]node{
				&SymbolNode{name: "setq"},
				&SymbolNode{name: "c"},
				&IntNode{value: 100},
			}),
			createList([]node{
				&SymbolNode{name: "setq"},
				&SymbolNode{name: "d"},
				&IntNode{value: 200},
			}),
		}))
		if err != nil {
			t.Errorf("%v", err)
		}
	}
}

func TestEvalPrint(t *testing.T) {
	ev := NewEvaluator()

	{
		// (print (+ 1 2))
		_, err := ev.Eval(createList([]node{
			&SymbolNode{name: "print"},
			createList([]node{
				&SymbolNode{name: "+"},
				&IntNode{value: 1},
				&IntNode{value: 2},
			}),
		}))
		if err != nil {
			t.Errorf("%v", err)
		}
	}
}

func TestEvalQuote(t *testing.T) {
	testCases := []struct{
		expr        node
		expected    string
	}{
		{
			// (quote (+ 1 2))
			createList([]node{
				&SymbolNode{name: "quote"},
				createList([]node{
					&SymbolNode{name: "+"},
					&IntNode{value: 1},
					&IntNode{value: 2},
				}),
			}),
			"(+ 1 2)",
		},
	}

	ev := NewEvaluator()

	for _, c := range testCases {
		result, err := ev.Eval(c.expr)
		if err != nil {
			t.Errorf("%v", err)
			continue
		}
		if result.ToString() != c.expected {
			t.Errorf("Result: %v, Expected: %v", result.ToString(), c.expected)
			continue
		}
	}
}

func TestEvalIf(t *testing.T) {
	ev := NewEvaluator()

	{
		// (if nil (print 1) (print 2))
		_, err := ev.Eval(createList([]node{
			&SymbolNode{name: "if"},
			&NilNode{},
			//&IntNode{value: 1},
			createList([]node{
				&SymbolNode{name: "print"},
				&IntNode{value: 1},
			}),
			createList([]node{
				&SymbolNode{name: "print"},
				&IntNode{value: 2},
			}),
		}))
		if err != nil {
			t.Errorf("%v", err)
		}
	}
}
