package interpreter

import (
	"testing"
)

type evalTestCase struct {
	expr        node
	expected    string
}

func evalTestCases(t *testing.T, testCases []evalTestCase) {
	for _, c := range testCases {
		t.Logf("%v", c.expr.ToString())
		ev := NewEvaluator()
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

func TestEvalAdd(t *testing.T) {
	testCases := []evalTestCase{
		{
			&ConsCell{
				car: &SymbolNode{name: "+"},
				cdr: &NilNode{},
			},
			"0",
		},
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
		{
			&ConsCell{
				car: &SymbolNode{name: "+"},
				cdr: &ConsCell{
					car: &IntNode{value: 1},
					cdr: &ConsCell{
						car: &FloatNode{value: 2.5},
						cdr: &NilNode{},
					},
				},
			},
			"3.5",
		},
	}

	evalTestCases(t, testCases)
}

func TestEvalSubtract(t *testing.T) {
	testCases := []evalTestCase{
		{
			createList([]node{
				&SymbolNode{name: "-"},
				&IntNode{value: 3},
			}),
			"-3",
		},
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
		{
			createList([]node{
				&SymbolNode{name: "-"},
				&IntNode{value: 3},
				&FloatNode{value: 0.5},
			}),
			"2.5",
		},
	}

	evalTestCases(t, testCases)
}

func TestEvalMultiply(t *testing.T) {
	testCases := []evalTestCase{
		{
			&ConsCell{
				car: &SymbolNode{name: "*"},
				cdr: &NilNode{},
			},
			"1",
		},
		{
			&ConsCell{
				car: &SymbolNode{name: "*"},
				cdr: &ConsCell{
					car: &IntNode{value: 2},
					cdr: &NilNode{},
				},
			},
			"2",
		},
		{
			&ConsCell{
				car: &SymbolNode{name: "*"},
				cdr: &ConsCell{
					car: &IntNode{value: 2},
					cdr: &ConsCell{
						car: &IntNode{value: 3},
						cdr: &NilNode{},
					},
				},
			},
			"6",
		},
		{
			&ConsCell{
				car: &SymbolNode{name: "*"},
				cdr: &ConsCell{
					car: &IntNode{value: 3},
					cdr: &ConsCell{
						car: &FloatNode{value: 2.5},
						cdr: &NilNode{},
					},
				},
			},
			"7.5",
		},
	}

	evalTestCases(t, testCases)
}

func TestEvalDivide(t *testing.T) {
	testCases := []evalTestCase{
		{
			createList([]node{
				&SymbolNode{name: "/"},
				&FloatNode{value: 2.0},
			}),
			"0.5",
		},
		{
			createList([]node{
				&SymbolNode{name: "/"},
				&IntNode{value: 6},
				&IntNode{value: 2},
			}),
			"3",
		},
		{
			createList([]node{
				&SymbolNode{name: "/"},
				&IntNode{value: 12},
				&IntNode{value: 2},
				&IntNode{value: 3},
			}),
			"2",
		},
		{
			createList([]node{
				&SymbolNode{name: "/"},
				&IntNode{value: 3},
				&FloatNode{value: 2.0},
			}),
			"1.5",
		},
	}

	evalTestCases(t, testCases)
}

func TestEvalEqual(t *testing.T) {
	testCases := []evalTestCase{
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
		{
			createList([]node{
				&SymbolNode{name: "="},
				&IntNode{value: 6},
				&IntNode{value: 6},
				&FloatNode{value: 6.0},
			}),
			"t",
		},
	}
	evalTestCases(t, testCases)
}

func TestEvalNotEqual(t *testing.T) {
	testCases := []evalTestCase{
		{
			createList([]node{
				&SymbolNode{name: "/="},
				&IntNode{value: 3},
				&IntNode{value: 2},
			}),
			"t",
		},
		{
			createList([]node{
				&SymbolNode{name: "/="},
				&IntNode{value: 3},
				&IntNode{value: 3},
			}),
			"nil",
		},
		{
			createList([]node{
				&SymbolNode{name: "/="},
				&IntNode{value: 1},
				&IntNode{value: 2},
				&IntNode{value: 3},
			}),
			"t",
		},
		{
			createList([]node{
				&SymbolNode{name: "/="},
				&IntNode{value: 1},
				&IntNode{value: 2},
				&IntNode{value: 1},
			}),
			"nil",
		},
	}
	evalTestCases(t, testCases)
}

func TestEvalGreaterThan(t *testing.T) {
	testCases := []evalTestCase{
		{
			createList([]node{
				&SymbolNode{name: ">"},
				&IntNode{value: 3},
				&IntNode{value: 2},
			}),
			"t",
		},
		{
			createList([]node{
				&SymbolNode{name: ">"},
				&IntNode{value: 3},
				&IntNode{value: 3},
			}),
			"nil",
		},
		{
			createList([]node{
				&SymbolNode{name: ">"},
				&IntNode{value: 2},
				&IntNode{value: 3},
			}),
			"nil",
		},
		{
			createList([]node{
				&SymbolNode{name: ">"},
				&IntNode{value: 3},
				&IntNode{value: 2},
				&IntNode{value: 1},
			}),
			"t",
		},
		{
			createList([]node{
				&SymbolNode{name: ">"},
				&IntNode{value: 3},
				&IntNode{value: 2},
				&IntNode{value: 3},
			}),
			"nil",
		},
	}
	evalTestCases(t, testCases)
}

func TestEvalGreaterThanOrEqualTo(t *testing.T) {
	testCases := []evalTestCase{
		{
			createList([]node{
				&SymbolNode{name: ">="},
				&IntNode{value: 3},
				&IntNode{value: 2},
			}),
			"t",
		},
		{
			createList([]node{
				&SymbolNode{name: ">="},
				&IntNode{value: 3},
				&IntNode{value: 3},
			}),
			"t",
		},
		{
			createList([]node{
				&SymbolNode{name: ">="},
				&IntNode{value: 2},
				&IntNode{value: 3},
			}),
			"nil",
		},
		{
			createList([]node{
				&SymbolNode{name: ">="},
				&IntNode{value: 3},
				&IntNode{value: 2},
				&IntNode{value: 2},
			}),
			"t",
		},
		{
			createList([]node{
				&SymbolNode{name: ">="},
				&IntNode{value: 3},
				&IntNode{value: 2},
				&IntNode{value: 3},
			}),
			"nil",
		},
	}
	evalTestCases(t, testCases)
}

func TestEvalLessThan(t *testing.T) {
	testCases := []evalTestCase{
		{
			createList([]node{
				&SymbolNode{name: "<"},
				&IntNode{value: 2},
				&IntNode{value: 3},
			}),
			"t",
		},
		{
			createList([]node{
				&SymbolNode{name: "<"},
				&IntNode{value: 3},
				&IntNode{value: 3},
			}),
			"nil",
		},
		{
			createList([]node{
				&SymbolNode{name: "<"},
				&IntNode{value: 3},
				&IntNode{value: 2},
			}),
			"nil",
		},
		{
			createList([]node{
				&SymbolNode{name: "<"},
				&IntNode{value: 1},
				&IntNode{value: 2},
				&IntNode{value: 3},
			}),
			"t",
		},
		{
			createList([]node{
				&SymbolNode{name: "<"},
				&IntNode{value: 3},
				&IntNode{value: 2},
				&IntNode{value: 3},
			}),
			"nil",
		},
	}
	evalTestCases(t, testCases)
}

func TestEvalLessThanOrEqualTo(t *testing.T) {
	testCases := []evalTestCase{
		{
			createList([]node{
				&SymbolNode{name: "<="},
				&IntNode{value: 2},
				&IntNode{value: 3},
			}),
			"t",
		},
		{
			createList([]node{
				&SymbolNode{name: "<="},
				&IntNode{value: 3},
				&IntNode{value: 3},
			}),
			"t",
		},
		{
			createList([]node{
				&SymbolNode{name: "<="},
				&IntNode{value: 3},
				&IntNode{value: 2},
			}),
			"nil",
		},
		{
			createList([]node{
				&SymbolNode{name: "<="},
				&IntNode{value: 1},
				&IntNode{value: 2},
				&IntNode{value: 2},
			}),
			"t",
		},
		{
			createList([]node{
				&SymbolNode{name: "<="},
				&IntNode{value: 1},
				&IntNode{value: 2},
				&IntNode{value: 1},
			}),
			"nil",
		},
	}
	evalTestCases(t, testCases)
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
			car: &ConsCell{
				car: &SymbolNode{name: "quote"},
				cdr: &ConsCell{
					car: list,
					cdr: &NilNode{},
				},
			},
			cdr: &NilNode{},
		},
	})
	if err != nil {
		t.Errorf("%v", err)
	} else {
		resultString := result.ToString()
		if resultString != "1" {
			t.Errorf("Expected: %v, Result: %v", "1", resultString)
		} else {
			t.Logf("%v", resultString);
		}
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
			car: &ConsCell{
				car: &SymbolNode{name: "quote"},
				cdr: &ConsCell{
					car: list,
					cdr: &NilNode{},
				},
			},
			cdr: &NilNode{},
		},
	})
	if err != nil {
		t.Errorf("%v", err)
	} else {
		resultString := result.ToString()
		if resultString != "(2 3)" {
			t.Errorf("Expected: %v, Result: %v", "(2 3)", resultString)
		} else {
			t.Logf("%v", resultString);
		}
	}
}

func TestEvalAnd(t *testing.T) {
	testCases := []evalTestCase{
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

	evalTestCases(t, testCases)
}

func TestEvalOr(t *testing.T) {
	testCases := []evalTestCase{
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

	evalTestCases(t, testCases)
}

func TestEvalSetq(t *testing.T) {
	testCases := []evalTestCase{
		{
			&ContainerNode{
				nodes: []node{
					// (setq foo 100)
					&ConsCell{
						car: &SymbolNode{name: "setq"},
						cdr: &ConsCell{
							car: &SymbolNode{name: "foo"},
							cdr: &ConsCell{
								car: &IntNode{value: 100},
								cdr: &NilNode{},
							},
						},
					},
					// foo
					&SymbolNode{name: "foo"},
				},
			},
			"100",
		},
	}

	evalTestCases(t, testCases)
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
	testCases := []evalTestCase{
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

	evalTestCases(t, testCases)
}

func TestEvalList(t *testing.T) {
	testCases := []evalTestCase{
		{
			// (list)
			createList([]node{
				&SymbolNode{name: "list"},
			}),
			"nil",
		},
		{
			// (list (+ 1 2) 4 5)
			createList([]node{
				&SymbolNode{name: "list"},
				createList([]node{
					&SymbolNode{name: "+"},
					&IntNode{value: 1},
					&IntNode{value: 2},
				}),
				&IntNode{value: 4},
				&IntNode{value: 5},
			}),
			"(3 4 5)",
		},
	}

	evalTestCases(t, testCases)
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
