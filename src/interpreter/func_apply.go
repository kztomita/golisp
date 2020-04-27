package interpreter

import (
	"fmt"
)

func funcApply(ev *evaluator, c *ConsCell) (node, error) {
	if c == nil {
		return nil, fmt.Errorf("Too few arguments given.")
	}
	if !c.isList() {
		return nil, fmt.Errorf("Wrong type argument.")
	}
	if c.length() < 2 {
		return nil, fmt.Errorf("Too few arguments given.")
	}

	evaledArg0, err := ev.Eval(c.car)
	if err != nil {
		return nil, err
	}

	switch evaledArg0.(type) {
	case *SystemFuncNode, *FuncNode, *SymbolNode:
	default:
		return nil, fmt.Errorf("The first argument is not function.")
	}

	args := createSliceFromList(c.next())
	var head node
	if len(args) > 0 {
		// 末尾のリストにconsしていく
		var err error
		head, err = ev.Eval(args[len(args) - 1])
		if err != nil {
			return nil, err
		}
		for i := len(args) - 2 ; i >= 0 ; i-- {
			result, err := ev.Eval(args[i])
			if err != nil {
				return nil, err
			}
			// (cons result prev)
			head = &ConsCell{
				car: result,
				cdr: head,
			}
		}
	}

	var headCell *ConsCell
	switch head.(type) {
	case *ConsCell:
		headCell = head.(*ConsCell)
	case *NilNode:
		headCell = nil
	default:
		// dotリストに変換
		headCell = &ConsCell{
			car: &NilNode{},
			cdr: head,
		}
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
		return f(ev, headCell)
	case *FuncNode:
		fn := nd
		// 新しいlexical environmentを作成して切り替え
		// 新しいenvironmentは関数作成時にキャプチャしたenvironmentの子とする
		ev.pushEnvironment(newLexicalEnvironment(fn.env))

		result, err := evalFunc(ev, fn, createSliceFromList(headCell))

		ev.popEnvironment()

		return result, err
	default:
		return nil, fmt.Errorf("Logic error: invalid node type.")
	}
}

