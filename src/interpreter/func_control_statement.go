package interpreter

import (
	"fmt"
)

func funcIf(ev *evaluator, c *ConsCell) (node, error) {
	if c == nil {
		return nil, fmt.Errorf("Wrong number of arguments.")
	}
	if !c.isList() {
		return nil, fmt.Errorf("Wrong type argument.")
	}
	if c.length() != 3 {
		return nil, fmt.Errorf("Wrong number of arguments.")
	}

	args := createSliceFromList(c)

	condition, err := ev.Eval(args[0])
	if err != nil {
		return nil, err
	}

	var nd node
	if condition.GetNodeType() == NtNil {
		nd = args[2]  // else
	} else {
		nd = args[1]  // then
	}

	result, err := ev.Eval(nd)
	if err != nil {
		return nil, err
	}

	return result, nil
}
