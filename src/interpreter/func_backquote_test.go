package interpreter

import (
	"testing"
)

func TestExpandBackQuote(t *testing.T) {
	testCases := []struct{
		expr        node
		expected    string
	}{
		{
			// `(a b)
			// (system::backquote (a b))
			createList([]node{
				&SymbolNode{name: "system::backquote"},
				createList([]node{
					&SymbolNode{name: "a"},
					&SymbolNode{name: "b"},
				}),
			}),
			"(list (quote a) (quote b))",
		},
		{
			// `(a ,b)
			// (system::backquote (a (system::unquote b)))
			createList([]node{
				&SymbolNode{name: "system::backquote"},
				createList([]node{
					&SymbolNode{name: "a"},
					createList([]node{
						&SymbolNode{name: "system::unquote"},
						&SymbolNode{name: "b"},
					}),
				}),
			}),
			"(list (quote a) b)",
		},
		{
			// `(a (b ,c))
			// (system::backquote (a (b (system::unquote c))))
			createList([]node{
				&SymbolNode{name: "system::backquote"},
				createList([]node{
					&SymbolNode{name: "a"},
					createList([]node{
						&SymbolNode{name: "b"},
						createList([]node{
							&SymbolNode{name: "system::unquote"},
							&SymbolNode{name: "c"},
						}),
					}),
				}),
			}),
			"(list (quote a) (list (quote b) c))",
		},
		{
			// ``(a ,b ,,c)
			// (system::backquote (system::backquote (a (system::unquote b) (system::unquote (system::unquote c)))))
			createList([]node{
				&SymbolNode{name: "system::backquote"},
				createList([]node{
					&SymbolNode{name: "system::backquote"},
					createList([]node{
						&SymbolNode{name: "a"},
						createList([]node{
							&SymbolNode{name: "system::unquote"},
							&SymbolNode{name: "b"},
						}),
						createList([]node{
							&SymbolNode{name: "system::unquote"},
							createList([]node{
								&SymbolNode{name: "system::unquote"},
								&SymbolNode{name: "c"},
							}),
						}),
					}),
				}),
			}),	
			"(list (quote list) (list (quote quote) (quote a)) (quote b) c)",
			// (list (quote list) (quote (quote a)) (quote b) c) と等価
		},
		{
			// `(a ,@b)
			// (system::backquote (a (system::unquote (system::splice b))))
			createList([]node{
				&SymbolNode{name: "system::backquote"},
				createList([]node{
					&SymbolNode{name: "a"},
					createList([]node{
						&SymbolNode{name: "system::unquote"},
							createList([]node{
							&SymbolNode{name: "system::splice"},
							&SymbolNode{name: "b"},
						}),
					}),
				}),
			}),
			"(cons (quote a) b)",
		},
	}

	for _, c := range testCases {
		t.Logf("%v", c.expr.ToString())
		result, err := expandBackQuote(c.expr)
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
func TestExpandSpliceNode(t *testing.T) {
	testCases := []struct{
		expr        node
		expected    string
	}{
		{
			// (a b c)
			createList([]node{
				&SymbolNode{name: "a"},
				&SymbolNode{name: "b"},
				&SymbolNode{name: "c"},
			}),
			"(cons a (cons b (list c)))",
		},
		{
			// (a b (system::splice c))
			createList([]node{
				&SymbolNode{name: "a"},
				&SymbolNode{name: "b"},
				createList([]node{
					&SymbolNode{name: "system::splice"},
					&SymbolNode{name: "c"},
				}),
			}),
			"(cons a (cons b c))",
		},
		{
			// (a b (system::splice c) d)
			createList([]node{
				&SymbolNode{name: "a"},
				&SymbolNode{name: "b"},
				createList([]node{
					&SymbolNode{name: "system::splice"},
					&SymbolNode{name: "c"},
				}),
				&SymbolNode{name: "d"},
			}),
			"(cons a (cons b (append c (list d))))",
		},
		{
			// ((system::splice a) b (system::splice c) d)
			createList([]node{
				createList([]node{
					&SymbolNode{name: "system::splice"},
					&SymbolNode{name: "a"},
				}),
				&SymbolNode{name: "b"},
				createList([]node{
					&SymbolNode{name: "system::splice"},
					&SymbolNode{name: "c"},
				}),
				&SymbolNode{name: "d"},
			}),
			"(append a (cons b (append c (list d))))",
		},
	}

	for _, c := range testCases {
		t.Logf("%v", c.expr.ToString())
		result := expandSpliceNode(c.expr.(*ConsCell))
		if result.ToString() != c.expected {
			t.Errorf("Result: %v, Expected: %v", result.ToString(), c.expected)
			continue
		}
	}
}
