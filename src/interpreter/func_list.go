package interpreter

import (
	"fmt"
)

func funcList(ev *evaluator, c *ConsCell) (node, error) {
	if c == nil {
		return &NilNode{}, nil
	}
	if !c.isList() {
		return nil, fmt.Errorf("Wrong type argument.")
	}

	elements := []node{}
	for ; c != nil ; c = c.next() {
		result, err := ev.Eval(c.car)
		if err != nil {
			return nil, err
		}
		elements = append(elements, result)
	}

	return createList(elements), nil
}
