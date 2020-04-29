package interpreter

import (
	"fmt"
)

func funcList(ev *evaluator, arglist node) (node, error) {
	if !isProperList(arglist) {
		return nil, fmt.Errorf("Wrong type argument.")
	}

	elements := []node{}
	for c := getConsCell(arglist) ; c != nil ; c = c.next() {
		result, err := ev.Eval(c.car)
		if err != nil {
			return nil, err
		}
		elements = append(elements, result)
	}

	return createList(elements), nil
}
