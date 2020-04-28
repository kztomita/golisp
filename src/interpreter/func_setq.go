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
	if (len(args) % 2) == 1 {
		return nil, fmt.Errorf("Wrong number of arguments.")
	}

	var result node
	for i := 0 ; i < len(args) ; i+=2 {
		symbol, ok := args[i].(*SymbolNode)	// symbolname
		if !ok {
			return nil, fmt.Errorf("Wrong type argument.")
		}

		var err error
		result, err = ev.Eval(args[i + 1])
		if err != nil {
			return nil, err
		}

		var symTable symbolTable
		for env := ev.topEnvironment() ; env != nil ; env = env.parent {
			symTable = env.symbols
			_, ok := symTable[symbol.name]
			if ok {
				break
			}
		}
		if symTable == nil {
			symTable = ev.envStack[0].symbols
		}
		symTable[symbol.name] = result
	}

	return result, nil
}
