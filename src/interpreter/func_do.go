package interpreter

import (
	"fmt"
)

func funcDo(ev *evaluator, arglist node) (node, error) {
	if !isProperList(arglist) {
		return nil, fmt.Errorf("Wrong type argument.")
	}

	args, err := createSliceFromProperList(arglist)
	if err != nil {
		return nil, err
	}
	if len(args) < 2 {
		return nil, fmt.Errorf("Wrong number of arguments.")
	}

	ev.pushEnvironment(newLexicalEnvironment(ev.topEnvironment()))

	result, err := _funcDo(ev, args)

	ev.popEnvironment()

	return result, err
}

func _funcDo(ev *evaluator, args []node) (node, error) {
	var variables *ConsCell
	switch arg0 := args[0].(type) {
	case *ConsCell:
		variables = arg0
	case *NilNode:	// empty list
		variables = nil
	default:
		return nil, fmt.Errorf("Wrong type argument.")
	}

	endResult, ok := args[1].(*ConsCell)
	if !ok {
		return nil, fmt.Errorf("Wrong type argument.")
	}

	symTable := ev.topEnvironment().symbols

	stepExpressions := []node{}

	for cell := variables ; cell != nil ; cell = cell.next() {
		switch n := cell.car.(type) {
		case *ConsCell:
			// (var [init-form [step-form]])
			varInitStep, err := createSliceFromProperList(n)
			if err != nil {
				return nil, err
			}
			s, ok := varInitStep[0].(*SymbolNode)
			if !ok {
				return nil, fmt.Errorf("Variable name is not symbol.")
			}
			if len(varInitStep) >= 2 {
				// init form
				form := varInitStep[1]
				result, err := ev.Eval(form)
				if err != nil {
					return nil, nil
				}
				symTable.set(s, result)
			}
			if len(varInitStep) >= 3 {
				// step form配列
				// 変数を更新する式を作ってstepExpressionsへ
				form := varInitStep[2]
				stepExpr := createList([]node{
					&SymbolNode{name: "setq"},
					&SymbolNode{name: s.name},
					form,
				})
				stepExpressions = append(stepExpressions, stepExpr)
			}
		case *SymbolNode:
			// var
			symTable.set(n, &NilNode{})
		default:
			return nil, fmt.Errorf("Variable name is not symbol.")
		}
	}

	bodyStatements := args[2:len(args)]
	container := &ContainerNode{nodes: bodyStatements}
	endCondition := endResult.car

	for true {
		result, err := ev.Eval(endCondition)
		if err != nil {
			return nil, err
		}
		_, ok := result.(*TrueNode)
		if ok {
			// endCondition成立
			break
		}

		{
			var err error
			_, err = ev.Eval(container)
			if err != nil {
				return nil, err
			}
		}

		for _, expr := range stepExpressions {
			_, err := ev.Eval(expr)
			if err != nil {
				return nil, err
			}
		}
	}

	// result form
	resultForms := endResult.cdr

	resultProgn := &ConsCell{
		car: &SymbolNode{name: "progn"},
		cdr: resultForms,
	}

	return ev.Eval(resultProgn)
}
