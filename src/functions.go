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

func funcSetq(ev *evaluator, c *consCell) node {
	if c == nil {
		fmt.Fprintf(os.Stderr, "Wrong number of arguments.")
		return nil
	}
	if !c.isList() {
		fmt.Fprintf(os.Stderr, "Wrong type argument.")
		return nil
	}
	if c.length() != 2 {
		fmt.Fprintf(os.Stderr, "Wrong number of arguments.")
		return nil
	}

	arg0, ok := c.car.(*symbolNode)	// symbolname
	if !ok {
		fmt.Fprintf(os.Stderr, "Wrong type argument.")
		return nil
	}

	arg1 := c.next().car

	ev.symbolTable[arg0.name] = ev.eval(arg1)

	return &nilNode{}
}

func funcDefun(ev *evaluator, c *consCell) node {
	if c == nil {
		fmt.Fprintf(os.Stderr, "Wrong number of arguments.")
		return nil
	}
	if !c.isList() {
		fmt.Fprintf(os.Stderr, "Wrong type argument.")
		return nil
	}
	if c.length() < 3 {
		fmt.Fprintf(os.Stderr, "Wrong number of arguments.")
		return nil
	}

	p := c
	arg0, ok := p.car.(*symbolNode)	// func name
	if !ok {
		fmt.Fprintf(os.Stderr, "Wrong type argument.")
		return nil
	}

	p = p.next()
	var arguments *consCell
	arg1, ok1 := p.car.(*consCell)	// arguments
	if ok1 {
		arguments = arg1
	}

	p = p.next()	// body list

	fn := &funcNode{
		arguments: arguments,
		body: p,
	}

	ev.symbolTable[arg0.name] = fn

	return &nilNode{}
}

func funcPlus(ev *evaluator, c *consCell) *intNode {
	result := 0		// XXX 取り敢えずintのみ対応

	if c != nil && !c.isList() {
		fmt.Fprintf(os.Stderr, "Wrong type argument.")
		return nil
	}

	c2 := c
	for c2 != nil {
		element := ev.eval(c2.car)
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
