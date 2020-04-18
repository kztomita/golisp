package interpreter

import (
	"fmt"
)

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

	fmt.Println(value.ToString())

	return value, nil
}
