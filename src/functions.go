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

