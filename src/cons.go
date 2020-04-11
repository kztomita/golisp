package golisp

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

type consCell struct {
	car		node
	cdr		node
}
func (n *consCell) getNodeType() int {
	return ntConsCell
}
func (n *consCell) toString() string {
	if n.car == nil || n.cdr == nil {
		s := ""
		if n.cdr == nil {
			s += "car: empty"
		}
		if n.car == nil {
			s += "cdr: empty"
		}
		return s
	}

	s:= "("

	n2 := n

	// listの終わりまでループ
	for true {
		if n2.car == nil {
			s += "car: empty"
		} else {
			s += n2.car.toString()
		}

		if n2.cdr == nil {
			s += "cdr: empty"
			break
		}

		// end of list
		if n2.cdr.getNodeType() == ntNil {
			break
		}

		// 次もConsCellだった場合たどる
		next, ok := n2.cdr.(*consCell)
		if ok {
			s += " "
			n2 = next	
		} else {
			// dot list
			s += " . " + n2.cdr.toString() 
			break
		}
	}
	s += ")"

	return s
}
