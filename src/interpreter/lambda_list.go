package interpreter

import (
	"fmt"
)

type ordinaryLambdaListParameter struct {
	name		string
	required	bool
	optional	bool
	defValue	node
	rest		bool
}

const (
	ordinaryLambdaListRequired int = iota
	ordinaryLambdaListOptional
	ordinaryLambdaListRest
)
func parseOrdinaryLambdaList(ev *evaluator, c *ConsCell) ([]*ordinaryLambdaListParameter, error) {
	parameters := []*ordinaryLambdaListParameter{}

	status := ordinaryLambdaListRequired
	restKeyword := false
	rest := 0

	for ; c != nil ; c = c.next() {
		if s, ok := c.car.(*SymbolNode); ok {
			switch s.name {
			case "&optional":
				if status >= ordinaryLambdaListOptional {
					return nil, fmt.Errorf("&optional not allowed here..")
				}
				status = ordinaryLambdaListOptional
				continue
			case "&rest":
				if status >= ordinaryLambdaListRest {
					return nil, fmt.Errorf("&rest not allowed here..")
				}
				status = ordinaryLambdaListRest
				restKeyword = true
				continue
			case "&key", "&aux":
				return nil, fmt.Errorf("Unsupported parameter(%v).", s.name)
			}	
		}

		switch status {
		case ordinaryLambdaListRequired:
			switch nd := c.car.(type) {
			case *SymbolNode:
				s := nd
				parameters = append(parameters, &ordinaryLambdaListParameter{
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
				parameters = append(parameters, &ordinaryLambdaListParameter{
					name: nd.name,
					optional: true,
					defValue: &NilNode{},
				})
			case *ConsCell:
				if !nd.isList() {
					return nil, fmt.Errorf("Invalid lambda list element.")
				}
				pair := createSliceFromList(nd)
				if len(pair) > 2 {
					return nil, fmt.Errorf("Invalid lambda list element.")
				}
				s, ok := pair[0].(*SymbolNode)
				if !ok {
					return nil, fmt.Errorf("Invalid lambda list element.")
				}
				var value node = &NilNode{}
				if len(pair) == 2 {
					result, err := ev.Eval(pair[1])
					if err != nil {
						return nil, err
					}
					value = result
				}
				parameters = append(parameters, &ordinaryLambdaListParameter{
					name: s.name,
					optional: true,
					defValue: value,
				})
			default:
				return nil, fmt.Errorf("Wrong type argument.")
			}

		case ordinaryLambdaListRest:
			switch nd := c.car.(type) {
			case *SymbolNode:
				s := nd
				parameters = append(parameters, &ordinaryLambdaListParameter{
					name: s.name,
					rest: true,
				})
				rest++
			default:
				return nil, fmt.Errorf("Wrong type argument.")
			}
		}
	}

	if restKeyword && rest > 1 {
		return nil, fmt.Errorf("Only one variable is allowed after &rest.")
	}
	if restKeyword && rest == 0 {
		return nil, fmt.Errorf("Missing &rest parameter in lambda list.")
	}

	return parameters, nil
}


type macroLambdaListParameter struct {
	name		string
	required	bool
	optional	bool
	defValue	node
	rest		bool
}

const (
	macroLambdaListRequired int = iota
	macroLambdaListOptional
	macroLambdaListRest
)
func parseMacroLambdaList(ev *evaluator, c *ConsCell) ([]*macroLambdaListParameter, error) {
	parameters := []*macroLambdaListParameter{}

	status := macroLambdaListRequired
	restKeyword := false
	rest := 0

	for ; c != nil ; c = c.next() {
		if s, ok := c.car.(*SymbolNode); ok {
			switch s.name {
			case "&optional":
				if status >= macroLambdaListOptional {
					return nil, fmt.Errorf("&optional not allowed here..")
				}
				status = macroLambdaListOptional
				continue
			case "&rest", "&body":
				if status >= macroLambdaListRest {
					return nil, fmt.Errorf("&rest/&body not allowed here..")
				}
				status = macroLambdaListRest
				restKeyword = true
				continue
			case "&aux", "&environment", "&key", "&whole":
				return nil, fmt.Errorf("Unsupported parameter(%v).", s.name)
			}
		}

		switch status {
		case macroLambdaListRequired:
			switch nd := c.car.(type) {
			case *SymbolNode:
				s := nd
				parameters = append(parameters, &macroLambdaListParameter{
					name: s.name,
					required: true,
				})
			default:
				return nil, fmt.Errorf("Wrong type argument.")
			}

		case macroLambdaListOptional:
			switch nd := c.car.(type) {
			case *SymbolNode:
				// default is nil
				parameters = append(parameters, &macroLambdaListParameter{
					name: nd.name,
					optional: true,
					defValue: &NilNode{},
				})
			case *ConsCell:
				if !nd.isList() {
					return nil, fmt.Errorf("Invalid lambda list element.")
				}
				pair := createSliceFromList(nd)
				if len(pair) > 2 {
					return nil, fmt.Errorf("Invalid lambda list element.")
				}
				s, ok := pair[0].(*SymbolNode)
				if !ok {
					return nil, fmt.Errorf("Invalid lambda list element.")
				}
				var value node = &NilNode{}
				if len(pair) == 2 {
					result, err := ev.Eval(pair[1])
					if err != nil {
						return nil, err
					}
					value = result
				}
				parameters = append(parameters, &macroLambdaListParameter{
					name: s.name,
					optional: true,
					defValue: value,
				})
			default:
				return nil, fmt.Errorf("Wrong type argument.")
			}

		case macroLambdaListRest:
			switch nd := c.car.(type) {
			case *SymbolNode:
				s := nd
				parameters = append(parameters, &macroLambdaListParameter{
					name: s.name,
					rest: true,
				})
				rest++
			default:
				return nil, fmt.Errorf("Wrong type argument.")
			}
		}
	}

	if restKeyword && rest > 1 {
		return nil, fmt.Errorf("Only one variable is allowed after &rest.")
	}
	if restKeyword && rest == 0 {
		return nil, fmt.Errorf("Missing &rest parameter in lambda list.")
	}

	return parameters, nil
}
