package interpreter

import (
	"fmt"
)


func funcDefun(ev *evaluator, arglist node) (node, error) {
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

	arg0, ok := args[0].(*SymbolNode)	// func name
	if !ok {
		return nil, fmt.Errorf("Wrong type argument.")
	}

	parameters := []*ordinaryLambdaListParameter{}	// 仮引数一覧
	arg1, ok1 := args[1].(*ConsCell)	// parameters
	if ok1 {
		var err error
		parameters, err = parseOrdinaryLambdaList(ev, arg1)
		if err != nil {
			return nil, err
		}
	}

	bodyHead := cdr(cdr(arglist))		// body list

	fn := &FuncNode{
		parameters: parameters,
		body: bodyHead,
		env: ev.topEnvironment(),	// capture current lexical scope
	}

	functionTable[arg0.name] = fn

	return &SymbolNode{name: arg0.name}, nil
}

func funcDefmacro(ev *evaluator, arglist node) (node, error) {
	if !isProperList(arglist) {
		return nil, fmt.Errorf("Wrong type argument.")
	}

	args, err := createSliceFromProperList(arglist)
	if err != nil {
		return nil, err
	}
	if len(args) < 3 {
		return nil, fmt.Errorf("Wrong number of arguments.")
	}

	arg0, ok := args[0].(*SymbolNode)	// func name
	if !ok {
		return nil, fmt.Errorf("Wrong type argument.")
	}

	parameters := []*macroLambdaListParameter{}	// 仮引数一覧
	arg1, ok1 := args[1].(*ConsCell)	// parameters
	if ok1 {
		var err error
		parameters, err = parseMacroLambdaList(ev, arg1)
		if err != nil {
			return nil, err
		}
	}

	bodyHead := cdr(cdr(arglist))		// body list

	fn := &MacroNode{
		parameters: parameters,
		body: bodyHead,
		env: ev.topEnvironment(),	// capture current lexical scope
	}

	functionTable[arg0.name] = fn

	return &NilNode{}, nil
}
