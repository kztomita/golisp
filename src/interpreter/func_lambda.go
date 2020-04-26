package interpreter

import (
	"fmt"
)

func funcLambda(ev *evaluator, c *ConsCell) (node, error) {
	if c == nil {
		return nil, fmt.Errorf("Wrong number of arguments.")
	}
	if !c.isList() {
		return nil, fmt.Errorf("Wrong type argument.")
	}

	args := createSliceFromList(c)
	if len(args) < 2 {
		return nil, fmt.Errorf("Wrong number of arguments.")
	}

	parameters := []*ordinaryLambdaListParameter{}	// 仮引数一覧
	arg0, ok := args[0].(*ConsCell)	// parameters
	if ok {
		var err error
		parameters, err = parseOrdinaryLambdaList(ev, arg0)
		if err != nil {
			return nil, err
		}
	}

	bodyHead := c.next()		// body list

	fn := &FuncNode{
		parameters: parameters,
		body: bodyHead,
		scope: newLexicalScope(ev.topScope()),	// LexicalScope作成
	}

	return fn, nil
}
