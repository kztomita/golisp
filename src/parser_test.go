package golisp

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
			`(1 2 (foo 4 5 "bar") 3)`,
			`(1 2 (foo 4 5 "bar") 3)`,
			false,
		},
		{
			`(1 . 2)`,
			`(1 . 2)`,
			false,
		},
		{
			`(1 . 2 3)`,
			"",
			true,
		},
		{
			`(. 2)`,
			"",
			true,
		},
	}

	for _, c := range testCases {
		cell, err := parse(c.expression)
		if !c.err {
			if err != nil {
				t.Errorf("%v", err)
				continue
			}
			// 構文木を文字列に戻して確認
			result := cell.toString()
			if result != c.expected {
				t.Errorf("Result: %v, Expected: %v", result, c.expected)
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
	cell, err := parse("(+ 1 2 3)")
	if err != nil {
		t.Fatalf("%v", err)
	}
	result := eval(cell)
	if result != nil {
		t.Logf("%v", result.toString());
	}
}
