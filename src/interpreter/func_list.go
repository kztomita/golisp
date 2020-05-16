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
		elements = append(elements, c.car)
	}

	return createList(elements), nil
}
