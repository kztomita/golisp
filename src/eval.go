package golisp

import (
	"fmt"
	"os"
)

type lexicalScope struct {
	parent	*lexicalScope
}

type evaluator struct {
	symbolTable map[string]node
}

func newEvaluator() *evaluator {
	return &evaluator{
		symbolTable: make(map[string]node),
	}
}

func (e *evaluator) eval(n node) node {
	if n.getNodeType() == ntConsCell {
		// list
		cell := n.(*consCell)
		// 最初の要素を関数名として扱う
		if cell.car.getNodeType() == ntSymbol {
			symbol := cell.car.(*symbolNode)
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
			default:
				value, ok := e.symbolTable[funcName]
				if !ok {
					fmt.Fprintf(os.Stderr, "%v not found.", symbol.name)
					return nil
				}
				fn, ok := value.(*funcNode)
				if !ok {
					fmt.Fprintf(os.Stderr, "%v is not function.", symbol.name)
					return nil
				}
				return e.eval(fn)	// 関数実行
			}
		} else {
			// TODO error
		}
	} else if n.getNodeType() == ntSymbol {
		// symbol tableをlookup
		symbol := n.(*symbolNode)
		value, ok := e.symbolTable[symbol.name]
		if !ok {
			// TODO error
			fmt.Fprintf(os.Stderr, "%v not found.", symbol.name)
		}
		return value
	} else if n.getNodeType() == ntFunc {
		// TODO 引数
		function := n.(*funcNode)
		current := function.body
		var lastResult node
		lastResult = &nilNode{}
		// bodyのlistを順番に評価していく
		for current != nil {
			list := current.car
			lastResult = e.eval(list)
			current = current.next()
		}
		return lastResult
	} else {
		return n
	}

	return nil
}
