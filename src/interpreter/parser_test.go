package interpreter

import (
	"testing"
)

func TestParser(t *testing.T) {
	testCases := []struct{
		expression	string
		expected	string
		err			bool
	}{
		{
			`(1 2 (foo 4 5 "bar") 3 4.1)`,
			`(1 2 (foo 4 5 "bar") 3 4.1)`,
			false,
		},
		{
			`()`,
			`nil`,
			false,
		},
		{
			`(1 . 2)`,
			`(1 . 2)`,
			false,
		},
		{
			`1`,
			`1`,
			false,
		},
		{
			`'foo`,
			`(quote foo)`,
			false,
		},
		{
			`('foo '(a b) c)`,
			`((quote foo) (quote (a b)) c)`,
			false,
		},
		{
			"`foo",
			"(system::backquote foo)",
			false,
		},
		{
			"`(a b)",
			"(system::backquote (a b))",
			false,
		},
		{
			"`(a b (c))",
			"(system::backquote (a b (c)))",
			false,
		},
		{
			"`(a ,b)",
			"(system::backquote (a (system::unquote b)))",
			false,
		},
		{
			"`(a ',b)",
			"(system::backquote (a (quote (system::unquote b))))",
			false,
		},
		{
			`)`,
			"",
			true,
		},
		{
			`(1 2`,
			"",
			true,
		},
		{
			`(1 . 2 3)`,
			"",
			true,
		},
		{
			`(1 . 2 . 3)`,
			"",
			true,
		},
		{
			`(. 2)`,
			"",
			true,
		},
		{
			`(1 . )`,
			"",
			true,
		},
	}

	for _, c := range testCases {
		cell, err := Parse(c.expression)
		if !c.err {
			if err != nil {
				t.Errorf("%v", err)
				continue
			}
			// 構文木を文字列に戻して確認
			result := cell.ToString()
			if result != c.expected + "\n" {
				t.Errorf("Result: %v, Expected: %v", result, c.expected)
				continue
			}
			t.Logf("%v", result)
		} else {
			if err == nil {
				t.Errorf("Error is not returned.")
				continue
			}
		}
	}
}


func TestParseAndEval(t *testing.T) {
	// parse & eval
	cell, err := Parse("(+ 1 2 3)")
	if err != nil {
		t.Fatalf("%v", err)
	}

	ev := NewEvaluator()
	result, err := ev.Eval(cell)
	if err != nil {
		t.Errorf("%v", err)
	} else {
		t.Logf("%v", result.ToString());
	}
}
