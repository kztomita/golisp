package interpreter

import (
	"fmt"
)

type symbolTable map[string]node

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
func (e *lexicalEnvironment) lookupSymbol(name string) (node, bool) {
	for env := e ; env != nil ; env = env.parent {
		nd, ok := env.symbols[name]
		if ok {
			return nd, true
		}
	}

	return nil, false
}