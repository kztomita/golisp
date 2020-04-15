package golisp

type symbolTable map[string]node

type lexicalScope struct {
	parent	*lexicalScope
	symbolTableStack []symbolTable	// 再帰呼び出しでsymbolTableがスタックされていく
}
func newLexicalScope(parent *lexicalScope) *lexicalScope {
	return &lexicalScope{
		parent: parent,
		symbolTableStack: []symbolTable{symbolTable{}},
	}
}
func (s *lexicalScope) topSymbolTable() symbolTable {
	return s.symbolTableStack[len(s.symbolTableStack) - 1]
}
func (s *lexicalScope) lookupSymbol(name string) (node, bool) {
	scope := s
	for scope != nil {
		symTable := scope.topSymbolTable()
		nd, ok := symTable[name]
		if ok {
			return nd, true
		}
		scope = scope.parent
	}

	return nil, false
}


