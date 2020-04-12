package golisp

import (
	"testing"
)

func TestParser(t *testing.T) {
	cell, err := parse(`(1 2 (foo 4 5 "bar") 3)`)
	if err != nil {
		t.Fatalf("%v", err)
	}
	result := cell.toString()
	expected := `(1 2 (foo 4 5 "bar") 3)`
	if result != expected {
		t.Errorf("Result: %v, Expected: %v", result, expected)
	}
	t.Logf("%v", result)
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
