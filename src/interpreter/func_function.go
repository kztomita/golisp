package interpreter

import (
	"fmt"
)

func funcFunction(ev *evaluator, arglist node) (node, error) {
	if !isProperList(arglist) {
		return nil, fmt.Errorf("Wrong type argument.")
	}

	args, err := createSliceFromProperList(arglist)
	if err != nil {
		return nil, err
	}
	if len(args) != 1 {
		return nil, fmt.Errorf("Wrong number of arguments.")
	}

	arg0 := args[0]	// 評価は不要

	switch arg0.(type) {
	case *SymbolNode:
		funcName := arg0.(*SymbolNode).name
		fn := lookupFunction(funcName)
		if fn == nil {
			return nil, fmt.Errorf("%v function not found.", funcName)
		}
		return fn, nil
	case *ConsCell:
		// lambda式か確認
		funcNameSym := getListFirstSymbol(arg0.(*ConsCell))
		if funcNameSym == nil || funcNameSym.name != "lambda" {
			return nil, fmt.Errorf("Wrong type argument.")
		}
		// lambda式を評価してFuncNodeへ
		result, err := ev.Eval(arg0)
		if err != nil {
			return nil, err
		}
		// FuncNodeをそのまま返す
		return result, nil
	default:
		return nil, fmt.Errorf("Wrong type argument.")
	}
 }
