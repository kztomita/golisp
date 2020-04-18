package interpreter

import (
	"fmt"
)


func funcDefun(ev *evaluator, c *ConsCell) (node, error) {
	if c == nil {
		return nil, fmt.Errorf("Wrong number of arguments.")
	}
	if !c.isList() {
		return nil, fmt.Errorf("Wrong type argument.")
	}

	args := createSliceFromList(c)
	if len(args) < 3 {
		return nil, fmt.Errorf("Wrong number of arguments.")
	}

	arg0, ok := args[0].(*SymbolNode)	// func name
	if !ok {
		return nil, fmt.Errorf("Wrong type argument.")
	}

	parameters := []*SymbolNode{}	// 仮引数名のsymbolNode
	arg1, ok1 := args[1].(*ConsCell)	// parameters
	if ok1 {
		acell := arg1
		for acell != nil {
			sym, ok := acell.car.(*SymbolNode)
			if !ok {
				return nil, fmt.Errorf("Wrong type argument.")
			}
			parameters = append(parameters, sym)
			acell = acell.next()
		}
	}

	bodyHead := c.next().next()		// body list

	fn := &FuncNode{
		parameters: parameters,
		body: bodyHead,
		scope: newLexicalScope(ev.topScope()),	// 関数定義時にLexicalScope作成
	}

	symTable := ev.topScope().topSymbolTable()
	symTable[arg0.name] = fn

	return &NilNode{}, nil
}

func funcDefmacro(ev *evaluator, c *ConsCell) (node, error) {
	if c == nil {
		return nil, fmt.Errorf("Wrong number of arguments.")
	}
	if !c.isList() {
		return nil, fmt.Errorf("Wrong type argument.")
	}

	args := createSliceFromList(c)
	if len(args) < 3 {
		return nil, fmt.Errorf("Wrong number of arguments.")
	}

	arg0, ok := args[0].(*SymbolNode)	// func name
	if !ok {
		return nil, fmt.Errorf("Wrong type argument.")
	}

	parameters := []*SymbolNode{}	// 仮引数名のsymbolNode
	arg1, ok1 := args[1].(*ConsCell)	// parameters
	if ok1 {
		acell := arg1
		for acell != nil {
			sym, ok := acell.car.(*SymbolNode)
			if !ok {
				return nil, fmt.Errorf("Wrong type argument.")
			}
			parameters = append(parameters, sym)
			acell = acell.next()
		}
	}

	bodyHead := c.next().next()		// body list

	fn := &MacroNode{
		parameters: parameters,
		body: bodyHead,
		scope: newLexicalScope(ev.topScope()),	// 関数定義時にLexicalScope作成
	}

	symTable := ev.topScope().topSymbolTable()
	symTable[arg0.name] = fn

	return &NilNode{}, nil
}