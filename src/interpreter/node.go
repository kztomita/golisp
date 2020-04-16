package interpreter

import (
	"fmt"
)

type node interface {
	GetNodeType() int
	ToString() string
}

const (
	NtConsCell int = iota
	NtInt
	NtNil
	NtSymbol
	NtString
	NtFunc
)

type IntNode struct {
	value		int
}
func (n *IntNode) GetNodeType() int {
	return NtInt
}
func (n *IntNode) ToString() string {
	return fmt.Sprintf("%v", n.value)
}

type NilNode struct {
}
func (n *NilNode) GetNodeType() int {
	return NtNil
}
func (n *NilNode) ToString() string {
	return "nil"
}

type SymbolNode struct {
	name	string
}
func (n *SymbolNode) GetNodeType() int {
	return NtSymbol
}
func (n *SymbolNode) ToString() string {
	return n.name
}

type StringNode struct {
	value	string
}
func (n *StringNode) GetNodeType() int {
	return NtString
}
func (n *StringNode) ToString() string {
	// TODO escape
	return `"` + n.value + `"`
}

type FuncNode struct {
	parameters	[]*SymbolNode	// 仮引数名のsymbolNode
	body		*ConsCell
	scope		*lexicalScope
}
func (n *FuncNode) GetNodeType() int {
	return NtFunc
}
func (n *FuncNode) ToString() string {
	// TODO escape
	return "function"
}

type ConsCell struct {
	car		node
	cdr		node
}
func (c *ConsCell) GetNodeType() int {
	return NtConsCell
}
func (c *ConsCell) ToString() string {
	s:= "("

	c2 := c

	// listの終わりまでループ
	for true {
		if c2.car == nil {
			s += "car: empty"
		} else {
			s += c2.car.ToString()
		}

		if c2.cdr == nil {
			s += "cdr: empty"
			break
		}

		// end of list
		if c2.cdr.GetNodeType() == NtNil {
			break
		}

		// 次もConsCellだった場合たどる
		next, ok := c2.cdr.(*ConsCell)
		if ok {
			s += " "
			c2 = next
		} else {
			// dot list
			s += " . " + c2.cdr.ToString()
			break
		}
	}
	s += ")"

	return s
}

func (c *ConsCell) next() *ConsCell {
	next, ok := c.cdr.(*ConsCell)
	if !ok {
		return nil
	}
	return next
}

func (c *ConsCell) isDotList() bool {
	if c.cdr.GetNodeType() != NtConsCell &&
	   c.cdr.GetNodeType() != NtNil {
		return true
	}
	return false
}

// 末尾がdot listでないかチェック
func (c *ConsCell) isList() bool {
	c2 := c
	for c2 != nil {
		if c2.isDotList() {
			return false
		}
		c2 = c2.next()
	}
	return true
}

func (c *ConsCell) length() int {
	length := 0

	c2 := c
	for c2 != nil {
		length++
		if c2.isDotList() {
			break
		}
		c2 = c2.next()
	}
	return length
}

