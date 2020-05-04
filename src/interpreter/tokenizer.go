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
	tokenFloat
	tokenSymbol
	tokenString
	tokenDot
	tokenQuote
	tokenBackQuote
	tokenComma
	tokenCommaAt
	tokenSharpQuote
)

type token struct {
	tokenId		int
	literal		string
}

type tokenizer struct {
	scanner	*bufio.Scanner
	s	string
	pos	int			// next position
	lineno	int
}
func newTokenizer(rd io.Reader) *tokenizer {
	return &tokenizer{
		scanner: bufio.NewScanner(rd),
		lineno: 1,
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

	if b == 0x0d && t.peekChar(0) == 0x0a {
		t.lineno++
		t.pos++
		b = 0x0a
	} else if b == 0x0a || b == 0x0d {
		t.lineno++
		b = 0x0a
	}

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

func (t *tokenizer) skipLine() {
	for true {
		c := t.nextChar()
		if c == 0 {
			break
		}

		// \x0d\x0a
		if c == 0x0d && t.peekChar(0) == 0x0a {
			t.pos += 1
			break
		}
		if c == 0x0a || c == 0x0d {
			break
		}
	}
}

func (t *tokenizer) nextToken() *token {
	t.skipWhiteSpace()

	literal := ""
	found := false

	for ; true ; t.nextChar() {
		c := t.peekChar(0)
		if c == 0 {
			break		// eof
		}

		if isWhiteSpace(c) {
			if len(literal) > 0 {
				break
			} else {
				continue
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
			literal += string(c)
		case '"':
			if literal == "" {
				t.nextChar()
				return &token{tokenId: tokenString, literal: readString(t)}
			}
			found = true
		case '\'':
			if literal == "" {
				t.nextChar()
				return &token{tokenId: tokenQuote, literal: "'"}
			}
			found = true
		case '`':
			if literal == "" {
				t.nextChar()
				return &token{tokenId: tokenBackQuote, literal: "`"}
			}
			found = true
		case ',':
			if literal == "" {
				next := t.peekChar(1)
				if next == '@' {
					t.nextChar()
					t.nextChar()
					return &token{tokenId: tokenCommaAt, literal: ",@"}
				} else {
					t.nextChar()
					return &token{tokenId: tokenComma, literal: ","}
				}
			}
			found = true
		case '#':
			if literal == "" {
				next := t.peekChar(1)
				if next == '\'' {
					t.nextChar()
					t.nextChar()
					return &token{tokenId: tokenSharpQuote, literal: "#'"}
				}
			}

		case ';':
			if literal == "" {
				// ignore comment
				t.skipLine()
				t.pos--
				continue
			}
			found = true
		default:
			literal += string(c)
		}

		if (found) {
			break
		}
	}

	if literal == "" {
		return nil
	}

	matched, err := regexp.MatchString(`^(\+|-|)\d+$`, literal)
	if err == nil && matched {
		return &token{tokenId: tokenInt, literal: literal}
	}
	matched, err = regexp.MatchString(`^(\+|-|)(\d+\.\d+|\.\d+|\d+.)$`, literal)
	if err == nil && matched {
		return &token{tokenId: tokenFloat, literal: literal}
	}
	matched, err = regexp.MatchString(`(?i)^(\+|-|)(\d+\.\d+|\.\d+|\d+.|\d+)(e(\+|-)?\d+)?$`, literal)
	if err == nil && matched {
		return &token{tokenId: tokenFloat, literal: literal}
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
	return c == ' ' || c == '\t' || c == 0x0d || c == 0x0a
}

// "まで読み込む
func readString(t *tokenizer) string {
	literal := ""
	for true {
		c := t.nextChar()
		if c == 0 || c == '"' {
			break
		}
		if c == '\\' {
			// escape
			c := t.nextChar()
			if c == 0 {
				break
			} else {
				literal += string(c)
			}
			continue
		}
		literal += string(c)
	}
	return literal
}

