package interpreter

import (
	"fmt"
)

// environmentパラメータはサポートしない
func funcTypep(ev *evaluator, arglist node) (node, error) {
	if !isProperList(arglist) {
		return nil, fmt.Errorf("typep: Wrong type argument.")
	}

	args, err := createSliceFromProperList(arglist)
	if err != nil {
			return nil, err
	}
	if len(args) < 2 {
			return nil, fmt.Errorf("typep: Too few arguments given.")
	}
	if len(args) > 2 {
		return nil, fmt.Errorf("typep: environment parameter not supported.")
}

	result, err := ev.Eval(args[0])
	if err != nil {
		return nil, err
	}

	tspec, err := ev.Eval(args[1])
	if err != nil {
		return nil, err
	}

	// atomic type specifierのみサポート
	typename := ""
	switch tspec.(type) {
	case *TrueNode:
		return &TrueNode{}, nil
	case *NilNode:
		return &NilNode{}, nil
	case *SymbolNode:
		typename = tspec.(*SymbolNode).name
	default:
		return nil, fmt.Errorf("Unsupported type specifier.")
	}

	ret := false
	switch result.(type) {
	case *NilNode:
		if typename == "null" || typename == "symbol" || typename == "list" || typename == "sequence" {
			ret = true
		}
	case *ConsCell:
		if typename == "cons" || typename == "list" || typename == "sequence" {
			ret = true
		}
	case *TrueNode:
		if typename == "symbol" {
			ret = true
		}
	case *IntNode:
		if typename == "integer" || typename == "rational" || typename == "real" || typename == "number" {
			ret = true
		}
	case *FloatNode:
		if typename == "float" || typename == "real" || typename == "number" {
			ret = true
		}
	case *SymbolNode:
		if typename == "symbol" {
			ret = true
		}
	case *StringNode:
		if typename == "string" || typename == "vector" || typename == "array" || typename == "sequence" {
			ret = true
		}
	case *FuncNode, *SystemFuncNode:
		if typename == "function" {
			ret = true
		}
	default:
		return nil, fmt.Errorf("Unsupported type.")
	}

	if ret {
		return &TrueNode{}, nil
	} else {
		return &NilNode{}, nil
	}
}
