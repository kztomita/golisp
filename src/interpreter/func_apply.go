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

	evaledArg0, err := ev.Eval(args[0])
	if err != nil {
		return nil, err
	}

	switch evaledArg0.(type) {
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
		var err error
		head, err = ev.Eval(applyArgs[len(applyArgs) - 1])
		if err != nil {
			return nil, err
		}
		for i := len(applyArgs) - 2 ; i >= 0 ; i-- {
			result, err := ev.Eval(applyArgs[i])
			if err != nil {
				return nil, err
			}
			// (cons result prev)
			head = &ConsCell{
				car: result,
				cdr: head,
			}
		}
	} else {
		head = &NilNode{}
	}

	if symbol, ok := evaledArg0.(*SymbolNode); ok {
		funcNode := lookupFunction(symbol.name)
		if funcNode == nil {
			return nil, fmt.Errorf("Undefined system function %v.", symbol.name)
		}
		evaledArg0 = funcNode
	}

	switch nd := evaledArg0.(type) {
	case *SystemFuncNode:
		f, ok := embeddedFunctions[nd.name]
		if !ok {
			return nil, fmt.Errorf("Undefined system function %v.", nd.name)
		}
		return f(ev, head)
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

