package interpreter

import (
	"fmt"
)

type node interface {
	GetNodeType() int
	ToString() string
	Common() *NodeCommon
}

const (
	NtContainer int = iota
	NtConsCell
	NtNil
	NtTrue
	NtInt
	NtFloat
	NtSymbol
	NtString
	NtSystemFunc
	NtFunc
	NtMacro
	NtMultipleValues
)

type NodeCommon struct {
	lineno	int
}
func nodeLineno(nd node) int {
	return nd.Common().lineno
}

type ContainerNode struct {
	common	NodeCommon
	nodes	[]node
}
func (n *ContainerNode) GetNodeType() int {
	return NtContainer
}
func (n *ContainerNode) ToString() string {
	s := ""
	for _, nd := range n.nodes {
		s += nd.ToString() + "\n"
	}
	return s
}
func (n *ContainerNode) Common() *NodeCommon {
	return &n.common
}

type NilNode struct {
	common	NodeCommon
}
func (n *NilNode) GetNodeType() int {
	return NtNil
}
func (n *NilNode) ToString() string {
	return "nil"
}
func (n *NilNode) Common() *NodeCommon {
	return &n.common
}

type TrueNode struct {
	common	NodeCommon
}
func (n *TrueNode) GetNodeType() int {
	return NtTrue
}
func (n *TrueNode) ToString() string {
	return "t"
}
func (n *TrueNode) Common() *NodeCommon {
	return &n.common
}

type IntNode struct {
	common		NodeCommon
	value		int
}
func (n *IntNode) GetNodeType() int {
	return NtInt
}
func (n *IntNode) ToString() string {
	return fmt.Sprintf("%v", n.value)
}
func (n *IntNode) Common() *NodeCommon {
	return &n.common
}

type FloatNode struct {
	common		NodeCommon
	value		float64
}
func (n *FloatNode) GetNodeType() int {
	return NtFloat
}
func (n *FloatNode) ToString() string {
	return fmt.Sprintf("%v", n.value)
}
func (n *FloatNode) Common() *NodeCommon {
	return &n.common
}

type SymbolNode struct {
	common	NodeCommon
	unnamed	bool			// gensymで作成したシンボルでtrue
	name	string
}
func (n *SymbolNode) GetNodeType() int {
	return NtSymbol
}
func (n *SymbolNode) ToString() string {
	if n.unnamed == false {
		return n.name
	} else {
		return fmt.Sprintf("unnamed:%p", n)
	}
}
func (n *SymbolNode) Common() *NodeCommon {
	return &n.common
}
func (n *SymbolNode) symbolKey() string {
	if n.unnamed == false {
		return "named:" + n.name
	} else {
		return fmt.Sprintf("unnamed:%p", n)		// cloneでkeyが変わるので注意
	}
}
func (n *SymbolNode) clone() *SymbolNode {
	cloned := *n
	return &cloned
}

type StringNode struct {
	common	NodeCommon
	value	string
}
func (n *StringNode) GetNodeType() int {
	return NtString
}
func (n *StringNode) ToString() string {
	return `"` + escapeString(n.value) + `"`
}
func (n *StringNode) Common() *NodeCommon {
	return &n.common
}

type SystemFuncNode struct {
	common	NodeCommon
	name	string
}
func (n *SystemFuncNode) GetNodeType() int {
	return NtSystemFunc
}
func (n *SystemFuncNode) ToString() string {
	return "<system function " + n.name + ">"
}
func (n *SystemFuncNode) Common() *NodeCommon {
	return &n.common
}

type FuncNode struct {
	common		NodeCommon
	parameters	[]*ordinaryLambdaListParameter
	body		node					// *ConsCell or *NilNode
	env			*lexicalEnvironment		// captured environment
}
func (n *FuncNode) GetNodeType() int {
	return NtFunc
}
func (n *FuncNode) ToString() string {
	return "<function>"
}
func (n *FuncNode) Common() *NodeCommon {
	return &n.common
}

type MacroNode struct {
	common		NodeCommon
	parameters	[]*macroLambdaListParameter
	body		node					// *ConsCell or *NilNode
	env			*lexicalEnvironment		// captured environment
}
func (n *MacroNode) GetNodeType() int {
	return NtMacro
}
func (n *MacroNode) ToString() string {
	return "macro"
}
func (n *MacroNode) Common() *NodeCommon {
	return &n.common
}

type MultipleValuesNode struct {
	common	NodeCommon
	values	[]node
}
func (n *MultipleValuesNode) GetNodeType() int {
	return NtMultipleValues
}
func (n *MultipleValuesNode) ToString() string {
	str := ""
	for i := range n.values {
		str += n.values[i].ToString()
		if i < len(n.values) - 1 {
			str += ", "
		}
	}
	return str
}
func (n *MultipleValuesNode) Common() *NodeCommon {
	return &n.common
}

type ConsCell struct {
	common	NodeCommon
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
func (n *ConsCell) Common() *NodeCommon {
	return &n.common
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

func (c *ConsCell) tail() *ConsCell {
	if !isProperList(c) {
		return nil
	}

	var tail *ConsCell
	for cell := c ; cell != nil ; cell = cell.next() {
		tail = cell
	}
	return tail
}

func isNumberNode(n node) bool {
	switch n.(type) {
	case *IntNode, *FloatNode:
		return true
	default:
		return false
	}
}

