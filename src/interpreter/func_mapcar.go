package interpreter

import (
	"fmt"
)

// environmentパラメータはサポートしない
func funcMapcar(ev *evaluator, arglist node) (node, error) {
	if !isProperList(arglist) {
		return nil, fmt.Errorf("typep: Wrong type argument.")
	}
	args, err := createSliceFromProperList(arglist)
	if err != nil {
			return nil, err
	}
	if len(args) < 2 {
			return nil, fmt.Errorf("mapcar: Too few arguments given.")
	}

	args, err = evalNodes(ev, args)
	if err != nil {
		return nil, err
	}

	fn := args[0]

	minimum := 0
	lists := [][]node{}
	for _, list := range args[1:len(args)] {
		nodes, err := createSliceFromProperList(list)
		if err != nil {
			return nil, err
		}
		lists = append(lists, nodes)
		if len(nodes) > minimum {
			minimum = len(nodes)
		}
	}

	results := []node{}
	for i := 0 ; i < minimum ; i++ {
		fnArgs := []node{}
		for _, list := range lists {
			fnArgs = append(fnArgs, list[i])
		}

		applyArgs := createList([]node{
			fn,
			createList([]node{
				&SymbolNode{name: "quote"},
				createList(fnArgs),
			}),
		})

		result, err := funcApply(ev, applyArgs)
		if err != nil {
			return nil, err
		}
		results = append(results, result)
	}

	return createList(results), nil
}
