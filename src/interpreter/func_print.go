package interpreter

import (
	"fmt"
)

func funcPrin1(ev *evaluator, c *ConsCell) (node, error) {
	if c == nil {
		return nil, fmt.Errorf("Wrong number of arguments.")
	}
	if !c.isList() {
		return nil, fmt.Errorf("Wrong type argument.")
	}
	// streamは未対応
	if c.length() != 1 {
		return nil, fmt.Errorf("Wrong number of arguments.")
	}

	value, err := ev.Eval(c.car)
	if err != nil {
		return nil, err
	}

	ev.writed = true
	fmt.Print(value.ToString())

	return value, nil
}

func funcPrint(ev *evaluator, c *ConsCell) (node, error) {
	if c == nil {
		return nil, fmt.Errorf("Wrong number of arguments.")
	}
	if !c.isList() {
		return nil, fmt.Errorf("Wrong type argument.")
	}
	// streamは未対応
	if c.length() != 1 {
		return nil, fmt.Errorf("Wrong number of arguments.")
	}

	value, err := ev.Eval(c.car)
	if err != nil {
		return nil, err
	}

	ev.writed = true
	fmt.Printf("\n%v ", value.ToString())

	return value, nil
}
