package golisp

import (
	"fmt"
	"os"
)

type lexicalScope struct {
	parent	*lexicalScope
}

func plus(c *consCell) *intNode {
	result := 0		// XXX 取り敢えずintのみ対応

	c2 := c
	for true {
		element := eval(c2.car)
		intResult, ok := element.(*intNode)
		if ok {
			result += intResult.value
		} else {
			fmt.Fprintf(os.Stderr, "not integer element")
			return nil
		}

		if c2.cdr.getNodeType() == ntNil {
			break
		}

		next, ok := c2.cdr.(*consCell)
		if ok {
			c2 = next
		} else {
			// dot list
			fmt.Fprintf(os.Stderr, "Wrong type argument.")
			return nil
		}
	}

	return &intNode{value: result}
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
				fmt.Printf("plus\n")
				next, ok := cell.cdr.(*consCell)
				if ok {
					return plus(next)
				}
				// TODO 計算してintNodeを返す
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
