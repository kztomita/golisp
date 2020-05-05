package interpreter

import (
	"fmt"
	"io"
	"strconv"
	"strings"
)

type ParserError struct {
	LineNo	int
	Err  	error
}
func (e *ParserError) Error() string {
	if e.LineNo > 0 {
		return "Line " + strconv.Itoa(e.LineNo) + ": " + e.Err.Error()
	} else {
		return e.Err.Error()
	}
}
func (e *ParserError) Unwrap() error {
	return e.Err
}
func parserError(lineno int, err error) *ParserError {
	return &ParserError{
		LineNo: lineno,
		Err: err,
	}
}

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
		return createLeafNode(tk, token)
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
			return nil, parserError(tk.lineno, fmt.Errorf("Too many commas."))
		}
		return unquoteNextNode(ctx2, tk)
	case tokenCommaAt:
		return unquoteAndSpliceNextNode(ctx, tk)
	case tokenSharpQuote:
		return functionNextNode(ctx, tk)
	default:
		return nil, parserError(tk.lineno, fmt.Errorf("Syntax error."))
	}
}

func createLeafNode(tk *tokenizer, token *token) (node, error) {
	cmn := NodeCommon{lineno: tk.lineno}

	switch (token.tokenId) {
	case tokenInt:
		i, err := strconv.Atoi(token.literal)
		if err != nil {
			return nil, parserError(tk.lineno, fmt.Errorf("can't parse literal as integer."))
		}
		return &IntNode{value: i, common: cmn}, nil
	case tokenFloat:
		f, err := strconv.ParseFloat(token.literal, 64)
		if err != nil {
			return nil, parserError(tk.lineno, fmt.Errorf("can't parse literal as integer."))
		}
		return &FloatNode{value: f, common: cmn}, nil
	case tokenSymbol:
		return &SymbolNode{name: strings.ToLower(token.literal), common: cmn}, nil
	case tokenString:
		return &StringNode{value: token.literal, common: cmn}, nil
	default:
		return nil, parserError(tk.lineno, fmt.Errorf("Unknown token %v", token))
	}
}

func quoteNextNode(ctx *parsingContext, tk *tokenizer) (node, error) {
	nd, err := readExpression(ctx, tk)
	if err != nil {
		return nil, err
	}
	if nd == nil {
		return nil, parserError(tk.lineno, fmt.Errorf("Invalid quote literal."))
	}

	return createListWithLineno([]node{
		&SymbolNode{name: "quote", common: NodeCommon{lineno: tk.lineno}},
		nd,
	}, tk.lineno), nil
}

func backquoteNextNode(ctx *parsingContext, tk *tokenizer) (node, error) {
	nd, err := readExpression(ctx, tk)
	if err != nil {
		return nil, err
	}
	if nd == nil {
		return nil, parserError(tk.lineno, fmt.Errorf("Invalid backquote literal."))
	}

	return createListWithLineno([]node{
		&SymbolNode{name: "system::backquote", common: NodeCommon{lineno: tk.lineno}},
		nd,
	}, tk.lineno), nil
}

func unquoteNextNode(ctx *parsingContext, tk *tokenizer) (node, error) {
	nd, err := readExpression(ctx, tk)
	if err != nil {
		return nil, err
	}
	if nd == nil {
		return nil, parserError(tk.lineno, fmt.Errorf("Invalid comma literal."))
	}

	return createListWithLineno([]node{
		&SymbolNode{name: "system::unquote", common: NodeCommon{lineno: tk.lineno}},
		nd,
	}, tk.lineno), nil
}

func unquoteAndSpliceNextNode(ctx *parsingContext, tk *tokenizer) (node, error) {
	nd, err := readExpression(ctx, tk)
	if err != nil {
		return nil, err
	}
	if nd == nil {
		return nil, parserError(tk.lineno, fmt.Errorf("Invalid ,@ literal."))
	}

	return createListWithLineno([]node{
		&SymbolNode{name: "system::unquote", common: NodeCommon{lineno: tk.lineno}},
		createList([]node{
			&SymbolNode{name: "system::splice", common: NodeCommon{lineno: tk.lineno}},
			nd,
		}),
	}, tk.lineno), nil
}

func functionNextNode(ctx *parsingContext, tk *tokenizer) (node, error) {
	nd, err := readExpression(ctx, tk)
	if err != nil {
		return nil, err
	}
	if nd == nil {
		return nil, parserError(tk.lineno, fmt.Errorf("Invalid #' literal."))
	}

	return createListWithLineno([]node{
		&SymbolNode{name: "function", common: NodeCommon{lineno: tk.lineno}},
		nd,
	}, tk.lineno), nil
}

func readList(ctx *parsingContext, tk *tokenizer) (node, error) {

	nodes := []node{}

	foundDot := -1

	for true {
		token := tk.nextToken()
		if token == nil {
			return nil, parserError(tk.lineno, fmt.Errorf("list is not terminated."))
		}

		var nd node
		switch (token.tokenId) {
		case tokenRightParentheses:
			if foundDot == -1 {
				if len(nodes) == 0 {
					return &NilNode{common: NodeCommon{lineno: tk.lineno}}, nil
				}
				return createListWithLineno(nodes, tk.lineno), nil
			} else {
				if foundDot != len(nodes) - 1 {
					return nil, parserError(tk.lineno, fmt.Errorf("syntax error."))
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
			return nil, parserError(tk.lineno, fmt.Errorf("Logic error(nd is nil)."))
		}

		nodes = append(nodes, nd)
	}

	// not to reach
	return nil, parserError(tk.lineno, fmt.Errorf("not to reach"))
}