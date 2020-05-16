package interpreter

import (
	"fmt"
)

func funcTypeOf(ev *evaluator, arglist node) (node, error) {
	if !isProperList(arglist) {
		return nil, fmt.Errorf("type-of: Wrong type argument.")
	}
	if countProperListLength(arglist) != 1 {
		return nil, fmt.Errorf("type-of: Wrong number of arguments.")
	}

	arg0 := getConsCell(arglist).car

	switch arg0.(type) {
	case *NilNode:
		return &SymbolNode{name: "null"}, nil
	case *ConsCell:
		return &SymbolNode{name: "cons"}, nil
	case *TrueNode:
		return &SymbolNode{name: "boolean"}, nil
	case *IntNode:
		return &SymbolNode{name: "integer"}, nil
	case *FloatNode:
		return &SymbolNode{name: "float"}, nil
	case *SymbolNode:
		return &SymbolNode{name: "symbol"}, nil
	case *KeywordNode:
		return &SymbolNode{name: "keyword"}, nil
	case *StringNode:
		return &SymbolNode{name: "string"}, nil
	case *FuncNode, *SystemFuncNode:
		return &SymbolNode{name: "function"}, nil
	default:
		return nil, fmt.Errorf("Unsupported type.")
	}
}
