package interpreter

import (
	"fmt"
	"io"
	"os"
)

func Repl(input io.Reader) {
	index := 1

	p := NewReaderParser(input)
	ev := NewEvaluator()

	for true {
		fmt.Printf("#[%v] ", index)

		node, err := p.ParseExpression()
		if err != nil {
			fmt.Fprintf(os.Stderr, "%v\n", err)
			continue
		}
		if node == nil {
			// EOF
			break
		}

		result, err := ev.Eval(node)
		if err != nil {
			fmt.Fprintf(os.Stderr, "%v\n", err)
			continue
		}
		fmt.Println(result.ToString())
		index++
	}

	fmt.Printf("\n")
}
