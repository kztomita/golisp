package interpreter

import (
	"fmt"
)

func funcSystemBackQuote(ev *evaluator, c *ConsCell) (node, error) {
	if c == nil {
		return nil, fmt.Errorf("Wrong number of arguments.")
	}
	if !c.isList() {
		return nil, fmt.Errorf("Wrong type argument.")
	}
	if c.length() != 1 {
		return nil, fmt.Errorf("Wrong number of arguments.")
	}

	return processBackQuote(ev, c.car, 1)
}

// backquote,unquote,splice関数のみ評価して返す。それ以外の要素は評価せず返す(quote)。
func processBackQuote(ev *evaluator, nd node, depth int) (node, error) {
	switch nd.(type) {
	case *ConsCell:
		// list
		cell := nd.(*ConsCell)
		if symbol, ok := cell.car.(*SymbolNode); ok {
			if symbol.name == "system::unquote" {
				return processUnquote(ev, cell, depth)
			} else if symbol.name == "system::backquote" {
				if cell.next() == nil {
					return nil, fmt.Errorf("An argument of system::backquote not found.")
				}
				return processBackQuote(ev, cell.next().car, depth + 1)
			}
		}

		if depth >= 2 {
			// listシンボルを挿入
			insertCell := &ConsCell{
				car: cell.car,
				cdr: cell.cdr,
			}
			cell.car = &SymbolNode{name: "list"}
			cell.cdr = insertCell
		}
		for ; cell != nil ; cell = cell.next() {
			n, err := processBackQuote(ev, cell.car, depth)
			if err != nil {
				return nil, err
			}
			cell.car = n
		}
		return nd, nil
	default:
		return nd, nil
	}
}

func processUnquote(ev *evaluator, c *ConsCell, backQuoteDepth int) (node, error) {

	// system::unquote,spliceをたどって、最深のunquote対象ノードと深さを取得
	var nd node = c
	unquoteDepth := 0
	for true {
		switch nd.(type) {
		case *ConsCell:
			cell := nd.(*ConsCell)
			symbol, ok := cell.car.(*SymbolNode)
			if ok {
				if symbol.name == "system::unquote" {
					// ndはsystem::unquote関数呼び出しリスト
					unquoteDepth++
					if cell.next() == nil {
						return nil, fmt.Errorf("An argument of system::unquote not found.")
					}
					nd = cell.next().car
					continue
				}
			}
		default:
		}
		break
	}

	if unquoteDepth == 0 {
		return nil, fmt.Errorf("An argument 'c *ConsCell' is not system::unquote/splice.")
	}

	if unquoteDepth == backQuoteDepth {
		// unquote
		return ev.Eval(nd)
	} else {
		// quote
		return nd, nil
	}
}

