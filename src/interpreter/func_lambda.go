package interpreter

import (
	"fmt"
)

func funcLambda(ev *evaluator, arglist node) (node, error) {
	if !isProperList(arglist) {
		return nil, fmt.Errorf("Wrong type argument.")
	}

	args, err := createSliceFromProperList(arglist)
	if err != nil {
		return nil, err
	}
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

	c := getConsCell(arglist)
	bodyHead := c.next()		// body list

	fn := &FuncNode{
		parameters: parameters,
		body: bodyHead,
		env: ev.topEnvironment(),	// capture current lexical scope
	}

	return fn, nil
}
