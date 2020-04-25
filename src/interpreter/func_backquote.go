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

	// backquoteマクロの展開
	expanded, err := expandBackQuote(&ConsCell{
		car: &SymbolNode{name: "system::backquote"},
		cdr: c,
	})
	if err != nil {
		return nil, err
	}
	return ev.Eval(expanded)
}

// backquoteマクロの展開
func expandBackQuote(nd node) (node, error) {
	cell, ok := nd.(*ConsCell)
	if !ok {
		return nil, fmt.Errorf("argument is not backquote.")
	}
	symbol, ok := cell.car.(*SymbolNode)
	if !ok {
		return nil, fmt.Errorf("argument is not backquote.")
	}
	if symbol.name != "system::backquote" {
		return nil, fmt.Errorf("argument is not backquote.")
	}
	if cell.next() == nil {
		return nil, fmt.Errorf("An argument of system::backquote not found.")
	}

	return expandBackQuotedNode(cell.next().car)
}

func expandBackQuotedNode(nd node) (node, error) {
	symbol := getListFirstSymbol(nd)
	if symbol != nil && symbol.name == "system::backquote" {
		// ネストしていたら展開して差し替えて処理
		expanded, err := expandBackQuote(nd)
		if err != nil {
			return nil, err
		}
		nd = expanded
	}

	switch nd.(type) {
	case *ConsCell:
		// list
		cell := nd.(*ConsCell)

		symbol := getListFirstSymbol(cell)
		if symbol != nil {
			// unquoteならunwrap
			if symbol.name == "system::unquote" {
				if cell.next() == nil {
					return nil, fmt.Errorf("An argument of system::unquote not found.")
				}
				return cell.next().car, nil
			}
			// spliceならunwrap
			if symbol.name == "system::splice" {
				if cell.next() == nil {
					return nil, fmt.Errorf("An argument of system::splice not found.")
				}
				return cell.next().car, nil
			}
		}

		// (system::unquote ...), (system::splice ...)でないリスト

		head := cell

		for cell := head ; cell != nil ; cell = cell.next() {
			n, err := expandBackQuotedNode(cell.car)
			if err != nil {
				return nil, err
			}
			cell.car = n
		}

		foundSplice := false
		for cell := head ; cell != nil ; cell = cell.next() {
			symbol := getListFirstSymbol(cell.car)
			if symbol != nil && symbol.name == "system::splice" {
				foundSplice = true
				break
			}
		}

		if (!foundSplice) {
			// (system::splice ...)が要素にない
			// listシンボルを挿入
			head = &ConsCell{
				car: &SymbolNode{name: "list"},
				cdr: head,
			}
			return head, nil
		} else {
			// (system::splice ...)を要素に持つ
			return expandSpliceNode(head), nil
		}

	default:
		// leaf node
		return &ConsCell{
			car: &SymbolNode{name: "quote"},
			cdr: &ConsCell{
				car: nd,
				cdr: &NilNode{},
			},
		}, nil
	}
}

// (system::splice ...)の展開
// (system::splice ...)のリストを親のリスト内に展開する式を作成
func expandSpliceNode(cell *ConsCell) node {
	if cell == nil {
		return nil
	}

	elements := createSliceFromList(cell)

	lastElement := elements[len(elements) - 1]
	symbol := getListFirstSymbol(lastElement)
	var prev node
	if symbol != nil && symbol.name == "system::splice" {
		c := lastElement.(*ConsCell)
		prev = c.next().car
	} else {
		prev = createList([]node{
			&SymbolNode{name: "list"},
			lastElement,
		})
	}

	for i := len(elements) - 2 ; i >= 0 ; i-- {
		element := elements[i]
		symbol := getListFirstSymbol(element)
		if symbol != nil && symbol.name == "system::splice" {
			c := element.(*ConsCell)
			prev = &ConsCell{
				car: &SymbolNode{name: "append"},
				cdr: &ConsCell{
					car: c.next().car,
					cdr: &ConsCell{
						car: prev,
						cdr: &NilNode{},
					},
				},
			}
			} else {
			prev = &ConsCell{
				car: &SymbolNode{name: "cons"},
				cdr: &ConsCell{
					car: element,
					cdr: &ConsCell{
						car: prev,
						cdr: &NilNode{},
					},
				},
			}
		}
	}
	head := prev
	return head
}