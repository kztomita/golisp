package interpreter

import (
	"fmt"
)

func funcSetq(ev *evaluator, arglist node) (node, error) {
	if !isProperList(arglist) {
		return nil, fmt.Errorf("Wrong type argument.")
	}

	args, err := createSliceFromProperList(arglist)
	if err != nil {
		return nil, err
	}
	if len(args) == 0 {
		return nil, fmt.Errorf("Wrong number of arguments.")
	}
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
