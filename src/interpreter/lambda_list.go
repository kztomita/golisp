package interpreter

import (
	"fmt"
)

type ordinaryLambdaListParameter struct {
	symbol		*SymbolNode
	required	bool
	optional	bool
	defValue	node
	rest		bool
	key			bool
	keyword		*KeywordNode
}

const (
	ordinaryLambdaListRequired int = iota
	ordinaryLambdaListOptional
	ordinaryLambdaListRest
	ordinaryLambdaListKey
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
			case "&key":
				if status >= ordinaryLambdaListKey {
					return nil, fmt.Errorf("&key not allowed here..")
				}
				status = ordinaryLambdaListKey
				continue
			case "&aux":
				return nil, fmt.Errorf("Unsupported parameter(%v).", s.name)
			}	
		}

		switch status {
		case ordinaryLambdaListRequired:
			switch nd := c.car.(type) {
			case *SymbolNode:
				s := nd
				parameters = append(parameters, &ordinaryLambdaListParameter{
					symbol: s.clone(),
					required: true,
				})
			default:
				return nil, fmt.Errorf("Wrong type argument.")
			}

		case ordinaryLambdaListOptional:
			switch nd := c.car.(type) {
			case *SymbolNode:
				// default is nil
				s := nd
				parameters = append(parameters, &ordinaryLambdaListParameter{
					symbol: s.clone(),
					optional: true,
					defValue: &NilNode{},
				})
			case *ConsCell:
				if !isProperList(nd) {
					return nil, fmt.Errorf("Invalid lambda list element.")
				}
				pair, err := createSliceFromProperList(nd)
				if err != nil {
					return nil, fmt.Errorf("Invalid lambda list element.")
				}
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
					symbol: s.clone(),
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
					symbol: s.clone(),
					rest: true,
				})
				rest++
			default:
				return nil, fmt.Errorf("Wrong type argument.")
			}
		case ordinaryLambdaListKey:
			switch nd := c.car.(type) {
			case *SymbolNode:
				// default is nil
				s := nd
				parameters = append(parameters, &ordinaryLambdaListParameter{
					symbol: s.clone(),
					key: true,
					keyword: &KeywordNode{name: s.name},
					defValue: &NilNode{},
				})
			case *ConsCell:
				if !isProperList(nd) {
					return nil, fmt.Errorf("Invalid lambda list element.")
				}
				switch ndcar := nd.car.(type) {
				case *SymbolNode:		// (foo 1)形式
					pair, err := createSliceFromProperList(nd)
					if err != nil {
						return nil, fmt.Errorf("Invalid lambda list element.")
					}
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
						symbol: s.clone(),
						key: true,
						keyword: &KeywordNode{name: s.name},
						defValue: value,
					})
				case *ConsCell:		// ((:foo foo) 1)形式
					pair, err := createSliceFromProperList(ndcar)
					if err != nil {
						return nil, fmt.Errorf("Invalid lambda list element.")
					}
					if len(pair) > 2 {
						return nil, fmt.Errorf("Invalid lambda list element.")
					}
					k, ok := pair[0].(*KeywordNode)
					if !ok {
						return nil, fmt.Errorf("Invalid lambda list element.")
					}
					s, ok := pair[1].(*SymbolNode)
					if !ok {
						return nil, fmt.Errorf("Invalid lambda list element.")
					}
					var value node = &NilNode{}
					if next := nd.next() ; next != nil {
						initForm := next.car
						result, err := ev.Eval(initForm)
						if err != nil {
							return nil, err
						}
						value = result
					}
					parameters = append(parameters, &ordinaryLambdaListParameter{
						symbol: s.clone(),
						key: true,
						keyword: &KeywordNode{name: k.name},
						defValue: value,
					})
				default:
					return nil, fmt.Errorf("Wrong type argument.")
				}
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
	symbol		*SymbolNode
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
					symbol: s.clone(),
					required: true,
				})
			default:
				return nil, fmt.Errorf("Wrong type argument.")
			}

		case macroLambdaListOptional:
			switch nd := c.car.(type) {
			case *SymbolNode:
				// default is nil
				s := nd
				parameters = append(parameters, &macroLambdaListParameter{
					symbol: s.clone(),
					optional: true,
					defValue: &NilNode{},
				})
			case *ConsCell:
				if !isProperList(nd) {
					return nil, fmt.Errorf("Invalid lambda list element.")
				}
				pair, err := createSliceFromProperList(nd)
				if err != nil {
					return nil, fmt.Errorf("Invalid lambda list element.")
				}
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
					symbol: s.clone(),
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
					symbol: s.clone(),
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
