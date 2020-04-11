package golisp

type lexicalScope struct {
	parent	*lexicalScope
}

func eval(n node) node {
	if n.getNodeType() == ntConsCell {
		cell := n.(*consCell)
		// 最初の要素を関数名
		if cell.car.getNodeType() == ntSymbol {
			symbol := cell.car.(*symbolNode)
			funcName := symbol.name
			switch (funcName) {
			case "+":
				return funcPlus(cell.next())
			case "car":
				return funcCar(cell.next())
			case "cdr":
				return funcCdr(cell.next())
			}
		} else {
			// TODO error
		}
	} else {
		return n
	}

	return nil
}
