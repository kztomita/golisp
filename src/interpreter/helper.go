package interpreter

import (
	"regexp"
)

//
// List関連
//

// proper listであることをチェック
// (dotted list/circular listでないことをチェック)
func isProperList(head node) bool {
	nd := head
	for true {
		switch nd.(type) {
		case *ConsCell:
			cell := nd.(*ConsCell)
			nd = cell.cdr
			if nd == head {
				return false	// circular list
			}
		case *NilNode:
			return true
		default:
			// not list or dotted list
			return false
		}
	}
	// not to reach
	return false
}

func createList(elements []node) node {
	if len(elements) == 0 {
		return &NilNode{}
	}

	var p node
	var prev node
	prev = &NilNode{}
	for i := len(elements) - 1 ; i >= 0 ; i-- {
		el := elements[i]
		p = &ConsCell{
			car: el,
			cdr: prev,
		}
		prev = p
	}

	return p
}

// 最後のエントリをcdrに入れる
func createDotList(elements []node) node {
	if len(elements) < 2 {
		return nil
	}

	var p node
	var prev node
	prev = elements[len(elements) - 1]
	for i := len(elements) - 2 ; i >= 0 ; i-- {
		el := elements[i]
		p = &ConsCell{
			car: el,
			cdr: prev,
		}
		prev = p
	}

	return p
}

func createSliceFromList(head *ConsCell) []node {
	result := []node{}

	c := head
	for c != nil {
		result = append(result, c.car)

		// 正しく作成されていればcdrはnilにはならない(終端は&NilNode)。
		if c.cdr == nil {
			break
		}

		switch cdr := c.cdr.(type) {
		case *ConsCell:
			c = cdr		// next cons cell
		case *NilNode:
			c = nil		// terminate
		default:
			// dot list
			result = append(result, cdr)
			c = nil
		}
	}

	return result
}

func getListFirstSymbol(nd node) *SymbolNode {
	cell, ok := nd.(*ConsCell)
	if !ok {
		return nil
	}
	symbol, ok := cell.car.(*SymbolNode)
	if !ok {
		return nil
	}
	return symbol
}


//
// 文字列関連
//

func escapeString(s string) string {
	return regexEscapeString.ReplaceAllString(s, `\"`)
}

var regexEscapeString *regexp.Regexp

func init() {
	regexEscapeString, _ = regexp.Compile(`"`)
}
