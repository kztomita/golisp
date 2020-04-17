package interpreter

import (
	"fmt"
	"strconv"
	"strings"
)

func Parse(s string) (node, error) {
	tk := newTokenizer(strings.NewReader(s))

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
	case tokenInt, tokenSymbol, tokenString:
		return createLeafNode(token)
	default:
		return nil, fmt.Errorf("xxxxxx")
	}
}

func createLeafNode(token *token) (node, error) {
	switch (token.tokenId) {
	case tokenInt:
		i, err := strconv.Atoi(token.literal)
		if err != nil {
			return nil, fmt.Errorf("can't parse literal as integer.")
		}
		return &IntNode{value: i}, nil
	case tokenSymbol:
		return &SymbolNode{name: token.literal}, nil
	case tokenString:
		return &StringNode{value: token.literal}, nil
	default:
		return nil, fmt.Errorf("Unknown token %v", token)
	}
}

func readList(tk *tokenizer) (node, error) {

	nodes := []node{}

	foundDot := -1

	for true {
		token := tk.nextToken()
		if token == nil {
			return nil, fmt.Errorf("list is not terminated.")
		}
	
		var nd node
		switch (token.tokenId) {
		case tokenLeftParentheses:
			var err error
			nd, err = readList(tk)
			if err != nil {
				return nil, err
			}
		case tokenRightParentheses:
			// empty list
			if foundDot == -1 {
				if len(nodes) == 0 {
					return &NilNode{}, nil
				}
				return createList(nodes), nil
			} else {
				if foundDot != len(nodes) - 1 {
					return nil, fmt.Errorf("syntax error.")
				}
				return createDotList(nodes), nil
			}
		case tokenDot:
			if len(nodes) == 0 {
				return nil, fmt.Errorf("syntax error.")
			}
			if foundDot != -1 {
				return nil, fmt.Errorf("syntax error.")
			}
			foundDot = len(nodes)
			continue
		case tokenInt, tokenSymbol, tokenString:
			var err error
			nd, err = createLeafNode(token)
			if err != nil {
				return nil, err
			}
		default:
			return nil, fmt.Errorf("Unknown token (%v)", token)
		}

		if nd == nil {
			return nil, fmt.Errorf("Logic error(nd is nil).");
		}

		nodes = append(nodes, nd)
	}

	// not to reach
	return nil, fmt.Errorf("not to reach")
}