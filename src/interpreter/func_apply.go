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
		// evaluatorに関数のlexicalScopeを積んでscopeを切り替え。
		// 関数のlexicalScopeのsymbolStackに新しいテーブルを追加。
		ev.scopeStack = append(ev.scopeStack, fn.scope)
		fn.scope.symbolTableStack = append(fn.scope.symbolTableStack, symbolTable{})

		result, err := evalFunc(ev, fn, createSliceFromList(headCell))

		fn.scope.symbolTableStack = fn.scope.symbolTableStack[0:len(fn.scope.symbolTableStack) - 1]
		ev.scopeStack = ev.scopeStack[0:len(ev.scopeStack) - 1]

		return result, err
	default:
		return nil, fmt.Errorf("Logic error: invalid node type.")
	}
}

func lookupFunction(name string) node {
	if _, ok := embeddedFunctions[name]; ok {
		return &SystemFuncNode{name: name}
	}
	if nd, ok := functionTable[name]; ok {
		if fn, ok := nd.(*FuncNode); ok {		// *MacroNodeは返さないように
			return fn
		}
	}
	return nil
}
