package golisp

import (
	"testing"
)

func TestParser(t *testing.T) {
	testCases := []struct{
		expression	string
		expected	string
	}{
		{
			`(1 2 (foo 4 5 "bar") 3)`,
			`(1 2 (foo 4 5 "bar") 3)`,
		},
	}

	for _, c := range testCases {
		cell, err := parse(c.expression)
		if err != nil {
			t.Fatalf("%v", err)
		}
		result := cell.toString()
		if result != c.expected {
			t.Errorf("Result: %v, Expected: %v", result, c.expected)
		}
		t.Logf("%v", result)
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
