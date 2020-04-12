package golisp

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
		}
		return value
	} else {
		return n
	}

	return nil
}
