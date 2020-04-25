package interpreter

import (
	"fmt"
	"io"
	"strconv"
	"strings"
)

func Parse(s string) (node, error) {
	parser := NewReaderParser(strings.NewReader(s))

	container := &ContainerNode{}

	for true {
		nd, err := parser.ParseExpression()
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


type ReaderParser struct {
	tk	*tokenizer
}
func NewReaderParser(rd io.Reader) *ReaderParser {
	return &ReaderParser{tk: newTokenizer(rd)}
}
// io.Readerからexpressionを一つ分解析
// 解析すべきexpressionがないなら(nil, nil)をリターン。
func (p *ReaderParser) ParseExpression() (node, error) {
	return readExpression(&parsingContext{}, p.tk)
}

// 一つのexpressionを解析する間有効なコンテキスト
type parsingContext struct {
	backquote	int
	comma		int
}
func (c *parsingContext) clone() *parsingContext {
	ctx2 := *c
	return &ctx2
}

func readExpression(ctx *parsingContext, tk *tokenizer) (node, error) {
	token := tk.nextToken()
	if token == nil {
		return nil, nil
	}

	return createExpressionNode(ctx, tk, token)
}

func createExpressionNode(ctx *parsingContext, tk *tokenizer, token *token) (node, error) {
	switch (token.tokenId) {
	case tokenLeftParentheses:
		return readList(ctx, tk)
	case tokenInt, tokenFloat, tokenSymbol, tokenString:
		return createLeafNode(token)
	case tokenQuote:
		return quoteNextNode(ctx, tk)
	case tokenBackQuote:
		ctx2 := ctx.clone()
		ctx2.backquote++
		return backquoteNextNode(ctx2, tk)
	case tokenComma:
		ctx2 := ctx.clone()
		ctx2.comma++
		if ctx.backquote < ctx2.comma {
			return nil, fmt.Errorf("Too many commas.")
		}
		return unquoteNextNode(ctx2, tk)
	case tokenCommaAt:
		return unquoteAndSpliceNextNode(ctx, tk)
	default:
		return nil, fmt.Errorf("Syntax error.")
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
	case tokenFloat:
		f, err := strconv.ParseFloat(token.literal, 64)
		if err != nil {
			return nil, fmt.Errorf("can't parse literal as integer.")
		}
		return &FloatNode{value: f}, nil
	case tokenSymbol:
		return &SymbolNode{name: token.literal}, nil
	case tokenString:
		return &StringNode{value: token.literal}, nil
	default:
		return nil, fmt.Errorf("Unknown token %v", token)
	}
}

func quoteNextNode(ctx *parsingContext, tk *tokenizer) (node, error) {
	nd, err := readExpression(ctx, tk)
	if err != nil {
		return nil, err
	}
	if nd == nil {
		return nil, fmt.Errorf("Invalid quote literal.")
	}

	return createList([]node{
		&SymbolNode{name: "quote"},
		nd,
	}), nil
}

func backquoteNextNode(ctx *parsingContext, tk *tokenizer) (node, error) {
	nd, err := readExpression(ctx, tk)
	if err != nil {
		return nil, err
	}
	if nd == nil {
		return nil, fmt.Errorf("Invalid backquote literal.")
	}

	return createList([]node{
		&SymbolNode{name: "system::backquote"},
		nd,
	}), nil
}

func unquoteNextNode(ctx *parsingContext, tk *tokenizer) (node, error) {
	nd, err := readExpression(ctx, tk)
	if err != nil {
		return nil, err
	}
	if nd == nil {
		return nil, fmt.Errorf("Invalid comma literal.")
	}

	return createList([]node{
		&SymbolNode{name: "system::unquote"},
		nd,
	}), nil
}

func unquoteAndSpliceNextNode(ctx *parsingContext, tk *tokenizer) (node, error) {
	nd, err := readExpression(ctx, tk)
	if err != nil {
		return nil, err
	}
	if nd == nil {
		return nil, fmt.Errorf("Invalid ,@ literal.")
	}

	return createList([]node{
		&SymbolNode{name: "system::unquote"},
		createList([]node{
			&SymbolNode{name: "system::splice"},
			nd,
		}),
	}), nil
}

func readList(ctx *parsingContext, tk *tokenizer) (node, error) {

	nodes := []node{}

	foundDot := -1

	for true {
		token := tk.nextToken()
		if token == nil {
			return nil, fmt.Errorf("list is not terminated.")
		}

		var nd node
		switch (token.tokenId) {
		case tokenRightParentheses:
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
		default:
			var err error
			nd, err = createExpressionNode(ctx, tk, token)
			if err != nil {
				return nil, err
			}
		}

		if nd == nil {
			return nil, fmt.Errorf("Logic error(nd is nil).");
		}

		nodes = append(nodes, nd)
	}

	// not to reach
	return nil, fmt.Errorf("not to reach")
}