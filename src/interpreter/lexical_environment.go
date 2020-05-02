package interpreter

import (
	"fmt"
)

type symbolTable map[string]node
func (s symbolTable) lookup(symbol *SymbolNode) (node, bool) {
	nd, ok := s[symbol.symbolKey()]
	if ok {
		return nd, true
	} else {
		return nil, false
	}
}
func (s symbolTable) set(symbol *SymbolNode, value node) {
	s[symbol.symbolKey()] = value
}

func dumpSymbolTable(t symbolTable) {
	fmt.Printf("Symbol Table\n")
	for k := range t {
		fmt.Printf("%v\n", k)
	}
}

type lexicalEnvironment struct {
	parent	*lexicalEnvironment
	symbols symbolTable
}
func newLexicalEnvironment(parent *lexicalEnvironment) *lexicalEnvironment {
	return &lexicalEnvironment{
		parent: parent,
		symbols: symbolTable{},
	}
}
func (e *lexicalEnvironment) lookupSymbol(symbol *SymbolNode) (node, bool) {
	for env := e ; env != nil ; env = env.parent {
		nd, ok := env.symbols.lookup(symbol)
		if ok {
			return nd, true
		}
	}

	return nil, false
}