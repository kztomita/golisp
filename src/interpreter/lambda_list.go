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
			err := readOptionalParameter(ev , c.car, func(s *SymbolNode, defValue node) {
				parameters = append(parameters, &ordinaryLambdaListParameter{
					symbol: s,
					optional: true,
					defValue: defValue,
				})
			})
			if err != nil {
				return nil, err
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
			err := readKeywordParameter(ev , c.car, func(k *KeywordNode, s *SymbolNode, defValue node) {
				parameters = append(parameters, &ordinaryLambdaListParameter{
					symbol: s,
					key: true,
					keyword: k,
					defValue: defValue,
				})
			})
			if err != nil {
				return nil, err
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
	key			bool
	keyword		*KeywordNode
}

const (
	macroLambdaListRequired int = iota
	macroLambdaListOptional
	macroLambdaListRest
	macroLambdaListKey
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
			case "&key":
				if status >= macroLambdaListKey {
					return nil, fmt.Errorf("&key not allowed here..")
				}
				status = macroLambdaListKey
				continue
			case "&aux", "&environment", "&whole":
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
			err := readOptionalParameter(ev , c.car, func(s *SymbolNode, defValue node) {
				parameters = append(parameters, &macroLambdaListParameter{
					symbol: s,
					optional: true,
					defValue: defValue,
				})
			})
			if err != nil {
				return nil, err
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

		case ordinaryLambdaListKey:
			err := readKeywordParameter(ev , c.car, func(k *KeywordNode, s *SymbolNode, defValue node) {
				parameters = append(parameters, &macroLambdaListParameter{
					symbol: s,
					key: true,
					keyword: k,
					defValue: defValue,
				})
			})
			if err != nil {
				return nil, err
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

func readOptionalParameter(ev *evaluator, p node, fn func(s *SymbolNode, defValue node)) error {
	switch nd := p.(type) {
	case *SymbolNode:
		// default is nil
		s := nd
		fn(s.clone(), &NilNode{})
	case *ConsCell:
		if !isProperList(nd) {
			return fmt.Errorf("Invalid lambda list element.")
		}
		pair, err := createSliceFromProperList(nd)
		if err != nil {
			return fmt.Errorf("Invalid lambda list element.")
		}
		if len(pair) > 2 {
			return fmt.Errorf("Invalid lambda list element.")
		}
		s, ok := pair[0].(*SymbolNode)
		if !ok {
			return fmt.Errorf("Invalid lambda list element.")
		}
		var value node = &NilNode{}
		if len(pair) == 2 {
			result, err := ev.Eval(pair[1])
			if err != nil {
				return err
			}
			value = result
		}
		fn(s.clone(), value)
	default:
		return fmt.Errorf("Wrong type argument.")
	}

	return nil
}

func readKeywordParameter(ev *evaluator, p node, fn func(k *KeywordNode, s *SymbolNode, defValue node)) error {
	switch nd := p.(type) {
	case *SymbolNode:
		// default is nil
		s := nd
		fn(&KeywordNode{name: s.name}, s.clone(), &NilNode{})

	case *ConsCell:
		if !isProperList(nd) {
			return fmt.Errorf("Invalid lambda list element.")
		}
		if countProperListLength(nd) > 2 {
			return fmt.Errorf("Invalid lambda list element.")
		}

		var value node = &NilNode{}
		if next := nd.next() ; next != nil {
			initForm := next.car
			result, err := ev.Eval(initForm)
			if err != nil {
				return err
			}
			value = result
		}

		switch ndcar := nd.car.(type) {
		case *SymbolNode:		// (foo 1)形式
			s := ndcar
			fn(&KeywordNode{name: s.name}, s.clone(), value)

		case *ConsCell:		// ((:foo foo) 1)形式
			pair, err := createSliceFromProperList(ndcar)
			if err != nil {
				return fmt.Errorf("Invalid lambda list element.")
			}
			if len(pair) != 2 {
				return fmt.Errorf("Invalid lambda list element.")
			}
			k, ok := pair[0].(*KeywordNode)
			if !ok {
				return fmt.Errorf("Invalid lambda list element.")
			}
			s, ok := pair[1].(*SymbolNode)
			if !ok {
				return fmt.Errorf("Invalid lambda list element.")
			}
			fn(&KeywordNode{name: k.name}, s.clone(), value)

		default:
			return fmt.Errorf("Wrong type argument.")
		}
	default:
		return fmt.Errorf("Wrong type argument.")
	}

	return nil
}
