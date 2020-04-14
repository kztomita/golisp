package golisp

import (
	"fmt"
)

// (car list)
// Ex. (car '(1 2 3))
func funcCar(c *consCell) (node, error) {
	if c == nil {
		return nil, fmt.Errorf("Wrong number of arguments.")
	}
	if !c.isList() {
		return nil, fmt.Errorf("Wrong type argument.")
	}
	if c.length() != 1 {
		return nil, fmt.Errorf("Wrong number of arguments.")
	}

	arg0, ok := c.car.(*consCell)
	if !ok {
		return nil, fmt.Errorf("Wrong type argument.")
	}
	return arg0.car, nil
}

// (cdr list)
// Ex. (cdr '(1 2 3))
func funcCdr(c *consCell) (node, error) {
	if c == nil {
		return nil, fmt.Errorf("Wrong number of arguments.")
	}
	if !c.isList() {
		return nil, fmt.Errorf("Wrong type argument.")
	}
	if c.length() != 1 {
		return nil, fmt.Errorf("Wrong number of arguments.")
	}

	arg0, ok := c.car.(*consCell)
	if !ok {
		return nil, fmt.Errorf("Wrong type argument.")
	}
	return arg0.cdr, nil
}

func funcSetq(ev *evaluator, c *consCell) (node, error) {
	if c == nil {
		return nil, fmt.Errorf("Wrong number of arguments.")
	}
	if !c.isList() {
		return nil, fmt.Errorf("Wrong type argument.")
	}
	if c.length() != 2 {
		return nil, fmt.Errorf("Wrong number of arguments.")
	}

	arg0, ok := c.car.(*symbolNode)	// symbolname
	if !ok {
		return nil, fmt.Errorf("Wrong type argument.")
	}

	arg1 := c.next().car

	result, err := ev.eval(arg1)
	if err != nil {
		return nil, err
	}
	ev.symbolTable[arg0.name] = result

	return &nilNode{}, nil
}

func funcDefun(ev *evaluator, c *consCell) (node, error) {
	if c == nil {
		return nil, fmt.Errorf("Wrong number of arguments.")
	}
	if !c.isList() {
		return nil, fmt.Errorf("Wrong type argument.")
	}
	if c.length() < 3 {
		return nil, fmt.Errorf("Wrong number of arguments.")
	}

	p := c
	arg0, ok := p.car.(*symbolNode)	// func name
	if !ok {
		return nil, fmt.Errorf("Wrong type argument.")
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

	return &nilNode{}, nil
}

func funcPlus(ev *evaluator, c *consCell) (*intNode, error) {
	result := 0		// XXX 取り敢えずintのみ対応

	if c != nil && !c.isList() {
		return nil, fmt.Errorf("Wrong type argument.")
	}

	c2 := c
	for c2 != nil {
		element, err := ev.eval(c2.car)
		if err != nil {
			return nil, err
		}
		intResult, ok := element.(*intNode)
		if ok {
			result += intResult.value
		} else {
			return nil, fmt.Errorf("not integer element")
		}

		c2 = c2.next()
	}

	return &intNode{value: result}, nil
}
