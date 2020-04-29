package interpreter

import (
	"fmt"
)

func funcLet(ev *evaluator, arglist node) (node, error) {
	// lexical environmentを作成して切り替え
	ev.pushEnvironment(newLexicalEnvironment(ev.topEnvironment()))

	result, err := funcLet_(ev, arglist)

	ev.popEnvironment()

	return result, err
}

func funcLet_(ev *evaluator, arglist node) (node, error) {
	if !isProperList(arglist) {
		return nil, fmt.Errorf("Wrong type argument.")
	}

	if countProperListLength(arglist) < 2 {
		return nil, fmt.Errorf("Wrong number of arguments.")
	}

	p := getConsCell(arglist)
	arg0, ok := p.car.(*ConsCell)	// binding-list
	if !ok {
		return nil, fmt.Errorf("Wrong type argument(binding-list).")
	}
	if !isProperList(arg0) {
		return nil, fmt.Errorf("Wrong type argument(binding-list).")
	}

	symTable := ev.topEnvironment().symbols

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
