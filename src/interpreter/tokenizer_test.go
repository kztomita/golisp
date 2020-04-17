package interpreter

import (
	"strings"
	"testing"
)

func TestTokenizer(t *testing.T) {
	testCases := []struct{
		expression	string
		results		[]token
	}{
		{
			"(1 2 +3 -4 foo)",
			[]token{
				{tokenId: tokenLeftParentheses, literal: "("},
				{tokenId: tokenInt, literal: "1"},
				{tokenId: tokenInt, literal: "2"},
				{tokenId: tokenInt, literal: "+3"},
				{tokenId: tokenInt, literal: "-4"},
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
		{
			`(1 . 2)`,
			[]token{
				{tokenId: tokenLeftParentheses, literal: "("},
				{tokenId: tokenInt, literal: "1"},
				{tokenId: tokenDot, literal: "."},
				{tokenId: tokenInt, literal: "2"},
				{tokenId: tokenRightParentheses, literal: ")"},
			},
		},
		{
			"(1 2 3 \nfoo)",
			[]token{
				{tokenId: tokenLeftParentheses, literal: "("},
				{tokenId: tokenInt, literal: "1"},
				{tokenId: tokenInt, literal: "2"},
				{tokenId: tokenInt, literal: "3"},
				{tokenId: tokenSymbol, literal: "foo"},
				{tokenId: tokenRightParentheses, literal: ")"},
			},
		},
	}

	for _, c := range testCases {
		tk := newTokenizer(strings.NewReader(c.expression))
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
			t.Fatalf("Number of tokens is incorrect. Expected: %v, Result: %v.", len(tokens), len(c.results))
		}
		for i := range tokens {
			if tokens[i].tokenId != c.results[i].tokenId ||
				tokens[i].literal != c.results[i].literal {
				t.Fatalf("Expected: %v, Result: %v", c.results[i], tokens[i])
			}
		}
	}
}

func TestPeekToken(t *testing.T) {
	tk := newTokenizer(strings.NewReader("(1 2 3 foo)"))

	{
		token := tk.peekToken(1)
		if token == nil {
			t.Fatalf("Expected: %v, Result: nil", "(")
		} else if token.tokenId != tokenLeftParentheses {
			t.Fatalf("Expected: %v, Result: %v", "(", token.literal)
		}
	}
	{
		token := tk.peekToken(2)
		if token == nil {
			t.Fatalf("Expected: %v, Result: nil", "(")
		} else if token.tokenId != tokenInt || token.literal != "1" {
			t.Fatalf("Expected: %v, Result: %v", "1", token.literal)
		}
	}
	{
		token := tk.peekToken(7)
		if token != nil {
			t.Fatalf("Expected: nil")
		}
	}
	{
		token := tk.nextToken()
		if token == nil {
			t.Fatalf("Expected: %v, Result: nil", "(")
		} else if token.tokenId != tokenLeftParentheses {
			t.Fatalf("Expected: %v, Result: %v", "(", token.literal)
		}
	}
}
