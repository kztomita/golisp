package interpreter

import (
	"fmt"
	"reflect"
	"regexp"
)

//
// List関連
//

// proper listであることをチェック
// (dotted list/circular listでないことをチェック)
func isProperList(head node) bool {
	if head == nil || reflect.ValueOf(head).IsNil() {
		return false
	}

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

// Proper Listの長さをカウント
// Proper Listでなければ0を返す
func countProperListLength(head node) int {
	if !isProperList(head) {
		return 0
	}

	length := 0
	nd := head
	for true {
		switch nd.(type) {
		case *ConsCell:
			length++
			nd = nd.(*ConsCell).cdr
		case *NilNode:
			return length
		default:
			// not to reach
			return 0
		}
	}
	// not to reach
	return 0
}

// headリストの先頭の*ConsCellを返す。
// headが空リスト(*NilNode)の場合はnilを返す。
// headがリストでない場合もnilを返す。
func getConsCell(head node) *ConsCell {
	if head == nil || reflect.ValueOf(head).IsNil() {
		return nil
	}
	switch head.(type) {
	case *ConsCell:
		return head.(*ConsCell)
	case *NilNode:
		// empty list
		return nil
	default:
		// not list
		return nil
	}
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

func createSliceFromProperList(head node) ([]node, error) {
	if !isProperList(head) {
		return nil, fmt.Errorf("Argument is not a proper list.")
	}

	result := []node{}

	nd := head
	for true {
		switch nd.(type) {
		case *ConsCell:
			cell := nd.(*ConsCell)
			result = append(result, cell.car)
			nd = cell.cdr
		case *NilNode:
			return result, nil
		default:
			return nil, fmt.Errorf("Logic error. Argument is not a proper list.")
		}
	}
	// not to reach
	return nil, fmt.Errorf("Logic error. Not to reach.")
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
