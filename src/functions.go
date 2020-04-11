package golisp

import (
	"fmt"
	"os"
)

// (car list)
// Ex. (car '(1 2 3))
func funcCar(c *consCell) node {
	if c == nil {
		fmt.Fprintf(os.Stderr, "Wrong number of arguments.")
		return nil
	}
	if !c.isList() {
		fmt.Fprintf(os.Stderr, "Wrong type argument.")
		return nil
	}
	if c.length() != 1 {
		fmt.Fprintf(os.Stderr, "Wrong number of arguments.")
		return nil
	}

	arg0, ok := c.car.(*consCell)
	if !ok {
		fmt.Fprintf(os.Stderr, "Wrong type argument.")
		return nil
	}
	return arg0.car
}

// (cdr list)
// Ex. (cdr '(1 2 3))
func funcCdr(c *consCell) node {
	if c == nil {
		fmt.Fprintf(os.Stderr, "Wrong number of arguments.")
		return nil
	}
	if !c.isList() {
		fmt.Fprintf(os.Stderr, "Wrong type argument.")
		return nil
	}
	if c.length() != 1 {
		fmt.Fprintf(os.Stderr, "Wrong number of arguments.")
		return nil
	}

	arg0, ok := c.car.(*consCell)
	if !ok {
		fmt.Fprintf(os.Stderr, "Wrong type argument.")
		return nil
	}
	return arg0.cdr
}

func funcPlus(c *consCell) *intNode {
	result := 0		// XXX 取り敢えずintのみ対応

	if c != nil && !c.isList() {
		fmt.Fprintf(os.Stderr, "Wrong type argument.")
		return nil
	}

	c2 := c
	for c2 != nil {
		element := eval(c2.car)
		intResult, ok := element.(*intNode)
		if ok {
			result += intResult.value
		} else {
			fmt.Fprintf(os.Stderr, "not integer element")
			return nil
		}

		c2 = c2.next()
	}

	return &intNode{value: result}
}
