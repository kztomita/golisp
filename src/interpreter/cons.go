package interpreter

import (
	"fmt"
)

type node interface {
	getNodeType() int
	toString() string
}

const (
	ntConsCell int = iota
	ntInt
	ntNil
	ntSymbol
	ntString
	ntFunc
)

type intNode struct {
	value		int
}
func (n *intNode) getNodeType() int {
	return ntInt
}
func (n *intNode) toString() string {
	return fmt.Sprintf("%v", n.value)
}

type nilNode struct {
}
func (n *nilNode) getNodeType() int {
	return ntNil
}
func (n *nilNode) toString() string {
	return "nil"
}

type symbolNode struct {
	name	string
}
func (n *symbolNode) getNodeType() int {
	return ntSymbol
}
func (n *symbolNode) toString() string {
	return n.name
}

type stringNode struct {
	value	string
}
func (n *stringNode) getNodeType() int {
	return ntString
}
func (n *stringNode) toString() string {
	// TODO escape
	return `"` + n.value + `"`
}

type funcNode struct {
	parameters	[]*symbolNode	// 仮引数名のsymbolNode
	body		*consCell
	scope		*lexicalScope
}
func (n *funcNode) getNodeType() int {
	return ntFunc
}
func (n *funcNode) toString() string {
	// TODO escape
	return "function"
}

type consCell struct {
	car		node
	cdr		node
}
func (c *consCell) getNodeType() int {
	return ntConsCell
}
func (c *consCell) toString() string {
	s:= "("

	c2 := c

	// listの終わりまでループ
	for true {
		if c2.car == nil {
			s += "car: empty"
		} else {
			s += c2.car.toString()
		}

		if c2.cdr == nil {
			s += "cdr: empty"
			break
		}

		// end of list
		if c2.cdr.getNodeType() == ntNil {
			break
		}

		// 次もConsCellだった場合たどる
		next, ok := c2.cdr.(*consCell)
		if ok {
			s += " "
			c2 = next
		} else {
			// dot list
			s += " . " + c2.cdr.toString()
			break
		}
	}
	s += ")"

	return s
}

func (c *consCell) next() *consCell {
	next, ok := c.cdr.(*consCell)
	if !ok {
		return nil
	}
	return next
}

func (c *consCell) isDotList() bool {
	if c.cdr.getNodeType() != ntConsCell &&
	   c.cdr.getNodeType() != ntNil {
		return true
	}
	return false
}

// 末尾がdot listでないかチェック
func (c *consCell) isList() bool {
	c2 := c
	for c2 != nil {
		if c2.isDotList() {
			return false
		}
		c2 = c2.next()
	}
	return true
}

func (c *consCell) length() int {
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

