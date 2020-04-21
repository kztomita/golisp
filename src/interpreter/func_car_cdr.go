package interpreter

import (
	"fmt"
)

// (car list)
// Ex. (car '(1 2 3))
func funcCar(ev *evaluator, c *ConsCell) (node, error) {
	if c == nil {
		return nil, fmt.Errorf("Wrong number of arguments.")
	}
	if !c.isList() {
		return nil, fmt.Errorf("Wrong type argument.")
	}
	if c.length() != 1 {
		return nil, fmt.Errorf("Wrong number of arguments.")
	}

	result, err := ev.Eval(c.car)
	if err != nil {
		return nil, err
	}
	cell, ok := result.(*ConsCell)
	if !ok {
		return nil, fmt.Errorf("Wrong type argument.")
	}
	return cell.car, nil
}

// (cdr list)
// Ex. (cdr '(1 2 3))
func funcCdr(ev *evaluator, c *ConsCell) (node, error) {
	if c == nil {
		return nil, fmt.Errorf("Wrong number of arguments.")
	}
	if !c.isList() {
		return nil, fmt.Errorf("Wrong type argument.")
	}
	if c.length() != 1 {
		return nil, fmt.Errorf("Wrong number of arguments.")
	}

	result, err := ev.Eval(c.car)
	if err != nil {
		return nil, err
	}
	cell, ok := result.(*ConsCell)
	if !ok {
		return nil, fmt.Errorf("Wrong type argument.")
	}
	return cell.cdr, nil
}
