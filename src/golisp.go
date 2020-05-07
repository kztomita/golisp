package main

import (
	"fmt"
	"io/ioutil"
	"os"

	"github.com/kztomita/golisp/interpreter"
)

func usage() {
	fmt.Fprintf(os.Stderr, "Usage:\n")
	fmt.Fprintf(os.Stderr, "%v <file>\n", os.Args[0])
	os.Exit(1)
}

func repl() {
	interpreter.Repl(os.Stdin)
}

func evaluateFile(file string) error {
	lispBytes, err := ioutil.ReadFile(file)
	if err != nil {
		return err
	}

	interpreter.Initialize()

	node, err := interpreter.Parse(string(lispBytes))
	if err != nil {
		return err
	}
	//fmt.Printf("%v\n", node.ToString())

	ev := interpreter.NewEvaluator()
	result, err := ev.Eval(node)
	if err != nil {
		return err
	}
	ev.FreshLine()

	if false {
		fmt.Printf("%v", result.ToString());
	}

	return nil
}

func main() {
	if len(os.Args) < 2 {
		repl()
		return
	}

	file := os.Args[1]
	err := evaluateFile(file)
	if err != nil {
		fmt.Fprintf(os.Stderr, "%v\n", err)
	}
}