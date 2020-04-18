package interpreter

import (
	"fmt"
)

func funcSetq(ev *evaluator, c *ConsCell) (node, error) {
	if c == nil {
		return nil, fmt.Errorf("Wrong number of arguments.")
	}
	if !c.isList() {
		return nil, fmt.Errorf("Wrong type argument.")
	}

	args := createSliceFromList(c)
	if len(args) != 2 {
		return nil, fmt.Errorf("Wrong number of arguments.")
	}

	arg0, ok := args[0].(*SymbolNode)	// symbolname
	if !ok {
		return nil, fmt.Errorf("Wrong type argument.")
	}

	result, err := ev.Eval(args[1])
	if err != nil {
		return nil, err
	}

	scope := ev.topScope()
	var symTable symbolTable
	for ; scope != nil ; scope = scope.parent {
		symTable = scope.topSymbolTable()
		_, ok := symTable[arg0.name]
		if ok {
			break
		}
	}
	if symTable == nil {
		symTable = ev.scopeStack[0].symbolTableStack[0]
	}
	symTable[arg0.name] = result

	return &NilNode{}, nil
}
