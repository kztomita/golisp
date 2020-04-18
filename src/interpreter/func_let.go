package interpreter

import (
	"fmt"
)

func funcLet(ev *evaluator, c *ConsCell) (node, error) {
	// lexicalScopeを作成してscopeを切り替え。
	// 作成したlexicalScopeのsymbolStackに新しいテーブルを追加。
	scope := newLexicalScope(ev.topScope())
	ev.scopeStack = append(ev.scopeStack, scope)
	scope.symbolTableStack = append(scope.symbolTableStack, symbolTable{})

	result, err := funcLet_(ev, c)

	ev.scopeStack = ev.scopeStack[0:len(ev.scopeStack) - 1]

	return result, err
}

func funcLet_(ev *evaluator, c *ConsCell) (node, error) {
	if c == nil {
		return nil, fmt.Errorf("Wrong number of arguments.")
	}
	if !c.isList() {
		return nil, fmt.Errorf("Wrong type argument.")
	}
	if c.length() < 2 {
		return nil, fmt.Errorf("Wrong number of arguments.")
	}

	p := c
	arg0, ok := p.car.(*ConsCell)	// binding-list
	if !ok {
		return nil, fmt.Errorf("Wrong type argument(binding-list).")
	}
	if !arg0.isList() {
		return nil, fmt.Errorf("Wrong type argument(binding-list).")
	}

	symTable := ev.topScope().topSymbolTable()

	pBinding := arg0
	for pBinding != nil {
		binding, ok := pBinding.car.(*ConsCell)
		if !ok {
			return nil, fmt.Errorf("Wrong type argument(binding-list).")
		}
		if !binding.isList() || binding.length() != 2 {
			return nil, fmt.Errorf("Wrong type argument(binding-list).")
		}

		// first element
		bindingSymbol, ok := binding.car.(*SymbolNode)
		if !ok {
			return nil, fmt.Errorf("Wrong type argument(binding-list).")
		}

		// second element
		value := binding.next()
		if value == nil {
			return nil, fmt.Errorf("Wrong type argument(binding-list).")
		}
		bindingValue, err := ev.Eval(value.car)
		if err != nil {
			return nil, err
		}

		// 変数をシンボルテーブルに登録
		//fmt.Printf("%v %v", bindingSymbol.name, bindingValue.ToString())
		symTable[bindingSymbol.name] = bindingValue

		pBinding = pBinding.next()
	}

	// bodyの実行
	p = p.next()	// body list

	var lastResult node
	lastResult = &NilNode{}
	for ; p != nil ; p = p.next() {
		var err error
		lastResult, err = ev.Eval(p.car)
		if err != nil {
			return nil, err
		}
	}

	return lastResult, nil
}
