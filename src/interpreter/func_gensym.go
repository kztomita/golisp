package interpreter

import (
	"fmt"
)


func funcGensym(ev *evaluator, arglist node) (node, error) {
	if !isProperList(arglist) {
		return nil, fmt.Errorf("gensym: Wrong type argument.")
	}
	if countProperListLength(arglist) != 0 {
		return nil, fmt.Errorf("gensym: Wrong number of arguments.")
	}

	return &SymbolNode{unnamed: true}, nil
}
