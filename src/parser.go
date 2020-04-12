package golisp

import (
	"fmt"
	"strconv"
)

func parse(s string) (*consCell, error) {
	tk := &tokenizer{s: s, pos: 0}

	for true {
		token := tk.nextToken()
		if token == nil {
			break
		}

		switch (token.tokenId) {
		case tokenLeftParentheses:
			return createList(tk)
		default:
			return nil, fmt.Errorf("xxxxxx")
		}
	}
	return nil, fmt.Errorf("xxxxxx")
}

func createList(tk *tokenizer) (*consCell, error) {
	var head *consCell
	var tail *consCell

	for true {
		token := tk.nextToken()
		if token == nil {
			// TODO message
			break
		}
	
		var cc *consCell
		switch (token.tokenId) {
		case tokenLeftParentheses:
			listcc, err := createList(tk)
			if err != nil {
				return nil, err
			}
			cc = &consCell{car: listcc}
		case tokenRightParentheses:
			tail.cdr = &nilNode{}
			return head, nil
		case tokenInt:
			i, err := strconv.Atoi(token.literal)
			if err != nil {
				return nil, fmt.Errorf("can't parse literal as integer.")
			}
			cc = &consCell{car: &intNode{value: i}}
		case tokenSymbol:
			cc = &consCell{car: &symbolNode{name: token.literal}}
		case tokenString:
			// TODO
		default:
			return nil, fmt.Errorf("Unknown token (%v)", token)	
		}

		if cc != nil {
			if tail == nil {
				head = cc
				tail = head
			} else {
				tail.cdr = cc
				tail = cc
			}
		}
	}

	return nil, fmt.Errorf("list is not terminated.")
}