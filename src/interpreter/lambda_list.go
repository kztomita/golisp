package interpreter

import (
	"fmt"
)

type lambdaListParameter struct {
	name		string
	required	bool
	optional	bool
	defValue	node
}

const (
	ordinaryLambdaListRequired int = iota
	ordinaryLambdaListOptional
)
func parseOrdinaryLambdaList(ev *evaluator, c *ConsCell) ([]*lambdaListParameter, error) {
	parameters := []*lambdaListParameter{}

	status := ordinaryLambdaListRequired

	for ; c != nil ; c = c.next() {
		if s, ok := c.car.(*SymbolNode); ok {
			switch s.name {
			case "&optional":
				if status >= ordinaryLambdaListOptional {
					return nil, fmt.Errorf("&optional not allowed here..")
				}
				status = ordinaryLambdaListOptional
				continue
			case "&rest", "&key", "&aux":
				return nil, fmt.Errorf("Unsupported parameter(%v).", s.name)
			}	
		}

		switch status {
		case ordinaryLambdaListRequired:
			switch nd := c.car.(type) {
			case *SymbolNode:
				s := nd
				parameters = append(parameters, &lambdaListParameter{
					name: s.name,
					required: true,
				})
			default:
				return nil, fmt.Errorf("Wrong type argument.")
			}

		case ordinaryLambdaListOptional:
			switch nd := c.car.(type) {
			case *SymbolNode:
				// default is nil
				parameters = append(parameters, &lambdaListParameter{
					name: nd.name,
					optional: true,
					defValue: &NilNode{},
				})
			case *ConsCell:
				if !nd.isList() {
					return nil, fmt.Errorf("Syntax error.")
				}
				pair := createSliceFromList(nd)
				if len(pair) != 2 {
					return nil, fmt.Errorf("Syntax error.")
				}
				s, ok := pair[0].(*SymbolNode)
				if !ok {
					return nil, fmt.Errorf("Syntax error.")
				}
				result, err := ev.Eval(pair[1])
				if err != nil {
					return nil, err
				}
				parameters = append(parameters, &lambdaListParameter{
					name: s.name,
					optional: true,
					defValue: result,
				})
			default:
				return nil, fmt.Errorf("Wrong type argument.")
			}
		}
	}
	return parameters, nil
}
