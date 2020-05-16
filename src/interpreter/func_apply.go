package interpreter

import (
	"fmt"
)

func funcApply(ev *evaluator, arglist node) (node, error) {
	if !isProperList(arglist) {
		return nil, fmt.Errorf("Wrong type argument.")
	}

	args, err := createSliceFromProperList(arglist)
	if err != nil {
		return nil, err
	}
	if len(args) < 2 {
		return nil, fmt.Errorf("Too few arguments given.")
	}

	function := args[0]

	switch function.(type) {
	case *SystemFuncNode, *FuncNode, *SymbolNode:
	default:
		return nil, fmt.Errorf("The first argument is not function.")
	}

	rest := getConsCell(arglist).next()
	applyArgs, err := createSliceFromProperList(rest)
	if err != nil {
		return nil, err
	}
	var head node
	if len(applyArgs) > 0 {
		// 末尾のリストにconsしていく
		head = applyArgs[len(applyArgs) - 1]
		for i := len(applyArgs) - 2 ; i >= 0 ; i-- {
			// (cons applyArgs[i] prev)
			head = &ConsCell{
				car: applyArgs[i],
				cdr: head,
			}
		}
	} else {
		head = &NilNode{}
	}

	if symbol, ok := function.(*SymbolNode); ok {
		funcNode := lookupFunction(symbol.name)
		if funcNode == nil {
			return nil, fmt.Errorf("Undefined system function %v.", symbol.name)
		}
		function = funcNode
	}

	switch nd := function.(type) {
	case *SystemFuncNode:
		f, ok := embeddedFunctions[nd.name]
		if !ok {
			return nil, fmt.Errorf("Undefined system function %v.", nd.name)
		}

		if f.fntype != lispFuncTypeFunction {
			return nil, fmt.Errorf("Undefined system function %v.", nd.name)
		}

		return f.fn(ev, head)
	case *FuncNode:
		fn := nd
		// 新しいlexical environmentを作成して切り替え
		// 新しいenvironmentは関数作成時にキャプチャしたenvironmentの子とする
		ev.pushEnvironment(newLexicalEnvironment(fn.env))

		a, err := createSliceFromProperList(head)
		if err != nil {
			return nil, err
		}
		result, err := evalFunc(ev, fn, a)

		ev.popEnvironment()

		return result, err
	default:
		return nil, fmt.Errorf("Logic error: invalid node type.")
	}
}

