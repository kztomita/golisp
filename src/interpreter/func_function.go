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

	arg0 := c.car	// 評価は不要

	// TODO lambda
	switch arg0.(type) {
	case *SymbolNode:
		funcName := arg0.(*SymbolNode).name
		fn := lookupFunction(funcName)
		if fn == nil {
			return nil, fmt.Errorf("%v function not found.", funcName)
		}
		return fn, nil
	default:
		return nil, fmt.Errorf("Wrong type argument.")
	}
 }
