package interpreter

import (
	"fmt"
)

type evaluator struct {
	scopeStack	[]*lexicalScope
}

func NewEvaluator() *evaluator {
	return &evaluator{
		scopeStack: []*lexicalScope{newLexicalScope(nil)},
	}
}

func (e *evaluator) topScope() *lexicalScope {
	return e.scopeStack[len(e.scopeStack) - 1]
}

func (e *evaluator) Eval(n node) (node, error) {
	if n.GetNodeType() == ntConsCell {
		// list
		cell := n.(*ConsCell)
		// 最初の要素を関数名として扱う
		if cell.car.GetNodeType() == ntSymbol {
			symbol := cell.car.(*SymbolNode)
			funcName := symbol.name
			switch (funcName) {
			case "+":
				return funcPlus(e, cell.next())
			case "car":
				return funcCar(cell.next())
			case "cdr":
				return funcCdr(cell.next())
			case "setq":
				return funcSetq(e, cell.next())
			case "defun":
				return funcDefun(e, cell.next())
			case "let":
				return funcLet(e, cell.next())
			default:
				value, ok := e.topScope().lookupSymbol(funcName)
				if !ok {
					return nil, fmt.Errorf("%v not found.", symbol.name)
				}
				fn, ok := value.(*FuncNode)
				if !ok {
					return nil, fmt.Errorf("%v is not function.", symbol.name)
				}

				arguments := []node{}
				acell := cell.next()
				for acell != nil {
					argNode, err := e.Eval(acell.car)
					if err != nil {
						return nil, err
					}
					arguments = append(arguments, argNode)
					acell = acell.next()
				}

				// evaluatorに関数のlexicalScopeを積んでscopeを切り替え。
				// 関数のlexicalScopeのsymbolStackに新しいテーブルを追加。
				e.scopeStack = append(e.scopeStack, fn.scope)
				fn.scope.symbolTableStack = append(fn.scope.symbolTableStack, symbolTable{})

				result, err := evalFunc(e, fn, arguments)

				fn.scope.symbolTableStack = fn.scope.symbolTableStack[0:len(fn.scope.symbolTableStack) - 1]
				e.scopeStack = e.scopeStack[0:len(e.scopeStack) - 1]

				return result, err
			}
		} else {
			return nil, fmt.Errorf("invalid function.")
		}
	} else if n.GetNodeType() == ntSymbol {
		// symbol tableをlookup
		symbol := n.(*SymbolNode)
		value, ok := e.topScope().lookupSymbol(symbol.name)
		if !ok {
			return nil, fmt.Errorf("%v not found.", symbol.name)
		}
		return value, nil
	}

	return n, nil
}

func evalFunc(e *evaluator, fn *FuncNode, arguments []node) (node, error) {
	// 実引数を関数のscopeのsymbolTableに登録
	if len(fn.parameters) != len(arguments) {
		return nil, fmt.Errorf("Number of arguments is mismatch.")
	}
	symTable := e.topScope().topSymbolTable()
	for i := range fn.parameters {
		symTable[fn.parameters[i].name] = arguments[i]
	}

	current := fn.body
	var lastResult node
	lastResult = &NilNode{}
	// bodyのlistを順番に評価していく
	for current != nil {
		var err error
		list := current.car
		lastResult, err = e.Eval(list)
		if err != nil {
			return nil, err
		}
		current = current.next()
	}
	return lastResult, nil
}