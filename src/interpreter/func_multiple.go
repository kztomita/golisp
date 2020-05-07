package interpreter

import (
	"fmt"
)

func funcMultipleValueBind(ev *evaluator, arglist node) (node, error) {
	// lexical environmentを作成して切り替え
	ev.pushEnvironment(newLexicalEnvironment(ev.topEnvironment()))

	result, err := funcMultipleValueBind_(ev, arglist)

	ev.popEnvironment()

	return result, err
}

func funcMultipleValueBind_(ev *evaluator, arglist node) (node, error) {
	if !isProperList(arglist) {
		return nil, fmt.Errorf("multiple-value-bind: Wrong type argument.")
	}

	args, err := createSliceFromProperList(arglist)
	if err != nil {
		return nil, err
	}
	if len(args) < 2 {
		return nil, fmt.Errorf("multiple-value-bind: Wrong number of arguments.")
	}

	// (var*)
	vars, ok := args[0].(*ConsCell)
	if !ok {
		return nil, fmt.Errorf("multiple-value-bind: Wrong type argument.")
	}

	// eval values form
	evaledValues, err := ev.evaluate(args[1], true)
	if err != nil {
		return nil, err
	}
	multi, ok := evaledValues.(*MultipleValuesNode)
	if !ok {
		multi = &MultipleValuesNode{values: []node{evaledValues}}
	}

	symTable := ev.topEnvironment().symbols

	index := 0
	for cell := vars ; cell != nil ; cell = cell.next() {
		symbol, ok := cell.car.(*SymbolNode)
		if !ok {
			return nil, fmt.Errorf("multiple-value-bind: Wrong type argument.")
		}
		if index < len(multi.values) {
			symTable.set(symbol, multi.values[index])
		} else {
			symTable.set(symbol, &NilNode{})
		}
		index++
	}


	// evaluate implicit progn
	p := getConsCell(arglist).next().next()	// form head

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
