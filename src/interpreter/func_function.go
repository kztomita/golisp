package interpreter

import (
	"fmt"
)

func funcFunction(ev *evaluator, c *ConsCell) (node, error) {
	if c == nil {
		return nil, fmt.Errorf("Wrong number of arguments.")
	}
	if !c.isList() {
		return nil, fmt.Errorf("Wrong type argument.")
	}
	if c.length() != 1 {
		return nil, fmt.Errorf("Wrong number of arguments.")
	}

	arg0 := c.car

	// TODO lambda
	switch arg0.(type) {
	case *SymbolNode:
		funcName := arg0.(*SymbolNode).name

		if _, ok := embeddedFunctions[funcName]; ok {
			return &SystemFuncNode{name: funcName}, nil
		}
		if fn, ok := functionTable[funcName]; ok {
			return fn, nil
		}
		return nil, fmt.Errorf("%v function not found.", funcName)
	default:
		return nil, fmt.Errorf("Wrong type argument.")
	}
 }
