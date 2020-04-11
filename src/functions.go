package golisp

import (
	"fmt"
	"os"
)

// (car list)
// Ex. (car '(1 2 3))
func funcCar(c *consCell) node {
	// TODO 引数が一つでリストであること
	list, ok := c.car.(*consCell)
	if !ok {
		fmt.Fprintf(os.Stderr, "Wrong type argument.")
		return nil
	}
	return list.car
}

// (cdr list)
// Ex. (cdr '(1 2 3))
func funcCdr(c *consCell) node {
	// TODO 引数が一つでリストであること
	list, ok := c.car.(*consCell)
	if !ok {
		fmt.Fprintf(os.Stderr, "Wrong type argument.")
		return nil
	}
	return list.cdr
}

