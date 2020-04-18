package interpreter

import (
	"bufio"
	"io"
	"regexp"
)

const (
	tokenLeftParentheses int = iota
	tokenRightParentheses
	tokenInt
	tokenSymbol
	tokenString
	tokenDot
	tokenQuote
)

type token struct {
	tokenId		int
	literal		string
}

type tokenizer struct {
	scanner	*bufio.Scanner
	s	string
	pos	int			// next position
}
func newTokenizer(rd io.Reader) *tokenizer {
	return &tokenizer{
		scanner: bufio.NewScanner(rd),
	}
}

func (t *tokenizer) peekChar(offset int) byte {
	for t.pos + offset >= len(t.s) {
		if !t.scanner.Scan() {
			return 0
		}
		t.s += t.scanner.Text() + "\n"
	}
	return t.s[t.pos + offset]
}

func (t *tokenizer) nextChar() byte {
	b := t.peekChar(0)
	if b == 0 {
		return 0
	}

	t.pos++

	return b
}

func (t *tokenizer) skipWhiteSpace() {
	for true {
		c := t.nextChar()
		if c == 0 {
			break
		}
		if !isWhiteSpace(c) {
			t.pos--
			break
		}
	}
}

func (t *tokenizer) nextToken() *token {
	t.skipWhiteSpace()

	literal := ""
	found := false

	for true {
		c := t.peekChar(0)
		if c == 0 {
			break		// eof
		}

		if isWhiteSpace(c) {
			if len(literal) > 0 {
				break
			} else {
				continue	// 来ないはず
			}
		}

		switch c {
		case '(':
			if literal == "" {
				t.nextChar()
				return &token{tokenId: tokenLeftParentheses, literal: "("}
			}
			found = true
		case ')':
			if literal == "" {
				t.nextChar()
				return &token{tokenId: tokenRightParentheses, literal: ")"}
			}
			found = true
		case '.':
			if literal == "" {
				next := t.peekChar(1)
				if next == 0 || isWhiteSpace(next) {
					t.pos++
					return &token{tokenId: tokenDot, literal: "."}
				}
			}
		case '"':
			// TODO escape seq
			if literal == "" {
				t.nextChar()
				for true {
					c := t.nextChar()
					if c == 0 || c == '"' {
						break
					}
					literal += string(c)
				}
				return &token{tokenId: tokenString, literal: literal}
			}
			found = true
		case '\'':
			if literal == "" {
				t.nextChar()
				return &token{tokenId: tokenQuote, literal: "'"}
			}
			found = true
		default:
			literal += string(c)
		}

		if (found) {
			break
		}

		t.nextChar()
	}

	if literal == "" {
		return nil
	}

	matched, err := regexp.MatchString(`^(\+|-|)\d+$`, literal)
	if err == nil && matched {
		return &token{tokenId: tokenInt, literal: literal}
	}

	return &token{tokenId: tokenSymbol, literal: literal}
}

func (t *tokenizer) peekToken(offset int) *token {
	saved := t.pos
	var token *token
	for i := 0 ; i < offset ; i++ {
		token = t.nextToken()
	}
	t.pos = saved
	return token
}


func isWhiteSpace(c byte) bool {
	return c == ' ' || c == '\n'
}