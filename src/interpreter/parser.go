package interpreter

import (
	"fmt"
	"strconv"
)

func Parse(s string) (node, error) {
	tk := &tokenizer{s: s, pos: 0}

	container := &ContainerNode{}

	for true {
		nd, err := readExpression(tk)
		if err != nil {
			return nil, err
		}
		if nd == nil {
			break
		}
		container.nodes = append(container.nodes, nd)
	}

	return container, nil
}

func readExpression(tk *tokenizer) (node, error) {
	token := tk.nextToken()
	if token == nil {
		return nil, nil
	}

	switch (token.tokenId) {
	case tokenLeftParentheses:
		return readList(tk)
	case tokenInt:
		i, err := strconv.Atoi(token.literal)
		if err != nil {
			return nil, fmt.Errorf("can't parse literal as integer.")
		}
		return  &IntNode{value: i}, nil
	case tokenSymbol:
		return &SymbolNode{name: token.literal}, nil
	case tokenString:
		return &StringNode{value: token.literal}, nil
	default:
		return nil, fmt.Errorf("xxxxxx")
	}
}

func readList(tk *tokenizer) (node, error) {
	var head *ConsCell
	var tail *ConsCell

	foundDot := false

	for true {
		token := tk.nextToken()
		if token == nil {
			return nil, fmt.Errorf("list is not terminated.")
		}
	
		var cc *ConsCell
		switch (token.tokenId) {
		case tokenLeftParentheses:
			listcc, err := readList(tk)
			if err != nil {
				return nil, err
			}
			cc = &ConsCell{car: listcc}
		case tokenRightParentheses:
			// empty list
			if tail == nil {
				return &NilNode{}, nil
			}
			// terminate
			if tail.cdr == nil {
				tail.cdr = &NilNode{}
			}
			return head, nil
		case tokenDot:
			foundDot  = true
		case tokenInt:
			i, err := strconv.Atoi(token.literal)
			if err != nil {
				return nil, fmt.Errorf("can't parse literal as integer.")
			}
			cc = &ConsCell{car: &IntNode{value: i}}
		case tokenSymbol:
			cc = &ConsCell{car: &SymbolNode{name: token.literal}}
		case tokenString:
			cc = &ConsCell{car: &StringNode{value: token.literal}}
		default:
			return nil, fmt.Errorf("Unknown token (%v)", token)
		}

		if cc != nil {
			if !foundDot {
				if tail == nil {
					head = cc
					tail = head
				} else {
					tail.cdr = cc
					tail = cc
				}
			} else {
				if tail == nil {
					return nil, fmt.Errorf("syntax error")
				} else {
					// dot listを作る(conscellを追加するのではなくcdrに設定する)
					if tail.cdr != nil {
						// dot list作成済み。後に要素がまだある。
						return nil, fmt.Errorf("syntax error")
					}
					tail.cdr = cc.car
				}
			}
		}
	}

	// not to reach
	return nil, fmt.Errorf("not to reach")
}