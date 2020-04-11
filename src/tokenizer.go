package golisp

import (
	"regexp"
)

const (
	tokenLeftParentheses int = iota
	tokenRightParentheses
	tokenInt
	tokenSymbol
)

type token struct {
	tokenId		int
	literal		string
}

type tokenizer struct {
	s	string
	pos	int
}

func (t *tokenizer) nextToken() *token {
	slen := len(t.s)
	t.skipSpace()

	literal := ""
	found := false
	for ; t.pos < slen ; t.pos++ {
		switch t.s[t.pos] {
		case '(':
			if literal == "" {
				t.pos++
				return &token{tokenId: tokenLeftParentheses, literal: "("}
			}
			found = true
		case ')':
			if literal == "" {
				t.pos++
				return &token{tokenId: tokenRightParentheses, literal: ")"}
			}
			found = true
		case ' ':
			found = true
		default:
			literal += string(t.s[t.pos])
		}

		if (found) {
			break
		}
	}

	if literal == "" {
		return nil
	}

	matched, err := regexp.MatchString(`^\d+$`, literal)
	if err == nil && matched {
		return &token{tokenId: tokenInt, literal: literal}
	}

	return &token{tokenId: tokenSymbol, literal: literal}
}

func (t *tokenizer) skipSpace() {
	slen := len(t.s)
	for t.pos < slen && t.s[t.pos] == ' ' {
		t.pos++
	}
}