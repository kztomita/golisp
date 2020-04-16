package main

import (
	"fmt"
	"io/ioutil"
	"log"
	"os"

	"kztomita/golisp/interpreter"
)

func usage() {
	fmt.Fprintf(os.Stderr, "Usage:\n")
	fmt.Fprintf(os.Stderr, "%v <file>\n", os.Args[0])
	os.Exit(1)
}

func main() {
	if len(os.Args) < 2 {
		usage()
	}

	file := os.Args[1]

	lispBytes, err := ioutil.ReadFile(file)
	if err != nil {
		log.Fatalf("%v\n", err)
	}

	node, err := interpreter.Parse(string(lispBytes))
	if err != nil {
		log.Fatalf("%v\n", err)
	}
	fmt.Printf("%v\n", node.ToString())

	ev := interpreter.NewEvaluator()
	result, err := ev.Eval(node)
	if err != nil {
		log.Fatalf("%v", err)
	}

	// nodeのexportが必要？
	fmt.Printf("%v", result.ToString());
}