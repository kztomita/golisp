package golisp

import (
	"testing"
)

func TestParser(t *testing.T) {
	testCases := []struct{
		expression	string
		results		[]token
	}{
		{
			"(1 2 3 foo)",
			[]token{
				{tokenId: tokenLeftParentheses, literal: "("},
				{tokenId: tokenInt, literal: "1"},
				{tokenId: tokenInt, literal: "2"},
				{tokenId: tokenInt, literal: "3"},
				{tokenId: tokenSymbol, literal: "foo"},
				{tokenId: tokenRightParentheses, literal: ")"},
			},	
		},
		{
			"(1 2 (3) foo)",
			[]token{
				{tokenId: tokenLeftParentheses, literal: "("},
				{tokenId: tokenInt, literal: "1"},
				{tokenId: tokenInt, literal: "2"},
				{tokenId: tokenLeftParentheses, literal: "("},
				{tokenId: tokenInt, literal: "3"},
				{tokenId: tokenRightParentheses, literal: ")"},
				{tokenId: tokenSymbol, literal: "foo"},
				{tokenId: tokenRightParentheses, literal: ")"},
			},	
		},
		{
			`(1 2 "3 test" foo"bar baz")`,
			[]token{
				{tokenId: tokenLeftParentheses, literal: "("},
				{tokenId: tokenInt, literal: "1"},
				{tokenId: tokenInt, literal: "2"},
				{tokenId: tokenString, literal: "3 test"},
				{tokenId: tokenSymbol, literal: "foo"},
				{tokenId: tokenString, literal: "bar baz"},
				{tokenId: tokenRightParentheses, literal: ")"},
			},
		},
	}

	for _, c := range testCases {
		tk := tokenizer{s: c.expression, pos: 0}
		tokens := []*token{}
		for true {
			token := tk.nextToken()
			if token == nil {
				break
			}
			tokens = append(tokens, token)
			//t.Logf("%v", token)
		}

		if len(tokens) != len(c.results) {
			t.Fatalf("Number of tokens is incorrect.")
		}
		for i := range tokens {
			if tokens[i].tokenId != c.results[i].tokenId ||
				tokens[i].literal != c.results[i].literal {
				t.Fatalf("Expected: %v, Result: %v", c.results[i], tokens[i])
			}
		}
	}
}
