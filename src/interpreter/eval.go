package interpreter

import (
	"fmt"
	"strconv"
)

type EvaluatorError struct {
	LineNo	int
	Err  	error
}
func (e *EvaluatorError) Error() string {
	if e.LineNo > 0 {
		return "Line " + strconv.Itoa(e.LineNo) + ": " + e.Err.Error()
	} else {
		return e.Err.Error()
	}
}
func (e *EvaluatorError) Unwrap() error {
	return e.Err
}
func evError(lineno int, err error) *EvaluatorError {
	// EvaluatorErrorならそのまま返す
	if e, ok := err.(*EvaluatorError) ; ok {
		return e
	}
	return &EvaluatorError{
		LineNo: lineno,
		Err: err,
	}
}

type evaluator struct {
	envStack	[]*lexicalEnvironment
	writed		bool
}

func NewEvaluator() *evaluator {
	return &evaluator{
		envStack: []*lexicalEnvironment{newLexicalEnvironment(nil)},
	}
}

// get current lexical environment
func (e *evaluator) topEnvironment() *lexicalEnvironment {
	return e.envStack[len(e.envStack) - 1]
}
func (e *evaluator) pushEnvironment(env *lexicalEnvironment) {
	e.envStack = append(e.envStack, env)
}
func (e *evaluator) popEnvironment() {
	e.envStack = e.envStack[0:len(e.envStack) - 1]
}

func (e *evaluator) Eval(n node) (node, error) {
	return e.evaluate(n, false)
}

func (e *evaluator) evaluate(n node, requireMultipleValue bool) (node, error) {
	result, err := e.evaluate_(n)
	if err != nil {
		return nil, err
	}

	if !requireMultipleValue {
		if result.GetNodeType() == NtMultipleValues {
			mv := result.(*MultipleValuesNode)
			if len(mv.values) == 0 {
				return &NilNode{}, nil
			}
			return mv.values[0], nil
		}
	}

	return result, nil
}

func (e *evaluator) evaluate_(n node) (node, error) {
	if n.GetNodeType() == NtContainer {
		container := n.(*ContainerNode)
		var lastResult node
		lastResult = &NilNode{}
		for _, nd := range container.nodes {
			var err error
			lastResult, err = e.Eval(nd)
			if err != nil {
				return nil, err
			}
		}
		return lastResult, nil
	} else if n.GetNodeType() == NtConsCell {
		// list
		cell := n.(*ConsCell)
		// 最初の要素を関数名として扱う
		if cell.car.GetNodeType() == NtSymbol {
			symbol := cell.car.(*SymbolNode)
			funcName := symbol.name
			f, ok := embeddedFunctions[funcName]
			if ok {
				arglist := cell.cdr
				if f.fntype == lispFuncTypeFunction {
					var err error
					arglist, err = evalArglist(e, arglist)
					if err != nil {
						return nil, err
					}
				}
				result, err := f.fn(e, arglist)
				if err != nil {
					err = evError(nodeLineno(cell.car), err)
				}
				return result, err
			} else {
				value, ok := functionTable[funcName]
				if !ok {
					return nil, evError(nodeLineno(cell.car), fmt.Errorf("%v function not found.", funcName))
				}
				switch fn := value.(type) {
				case *FuncNode:
					arguments := []node{}
					acell := cell.next()
					for ; acell != nil ; acell = acell.next() {
						argNode, err := e.Eval(acell.car)
						if err != nil {
							return nil, err
						}
						arguments = append(arguments, argNode)
					}

					// 新しいlexical environmentを作成して切り替え
					// 新しいenvironmentは関数作成時にキャプチャしたenvironmentの子とする
					e.pushEnvironment(newLexicalEnvironment(fn.env))

					result, err := evalFunc(e, fn, arguments)
	
					e.popEnvironment()
	
					if err != nil {
						err = evError(nodeLineno(cell.car), err)
					}

					return result, err

				case *MacroNode:
					arguments := []node{}
					acell := cell.next()
					for ; acell != nil ; acell = acell.next() {
						// マクロでは引数の評価はしない
						arguments = append(arguments, acell.car)
					}

					// 展開＆評価
					e.pushEnvironment(newLexicalEnvironment(fn.env))

					expanded, err := expandMacro(e, fn, arguments)

					e.popEnvironment()

					if err != nil {
						err = evError(nodeLineno(cell.car), err)
						return nil, err
					}

					return e.Eval(expanded)

				default:
					return nil, evError(nodeLineno(value), fmt.Errorf("%v is not function.", symbol.name))
				}
				// not to reach
			}
		} else {
			return nil, evError(nodeLineno(cell.car), fmt.Errorf("invalid function (%v).", cell.car.ToString()))
		}
	} else if n.GetNodeType() == NtSymbol {
		symbol := n.(*SymbolNode)
		switch symbol.name {
		case "nil":
			return &NilNode{}, nil
		case "t":
			return &TrueNode{}, nil
		default:
		}
		// symbol tableをlookup
		value, ok := e.topEnvironment().lookupSymbol(symbol)
		if !ok {
			return nil, evError(nodeLineno(symbol), fmt.Errorf("%v not found.", symbol.name))
		}
		return value, nil
	}

	return n, nil
}

// arglistの各carを評価してProperListを返す。
func evalArglist(e *evaluator, arglist node) (node, error) {
	switch arglist.(type) {
	case *NilNode:
		return &NilNode{}, nil
	case *ConsCell:
		arguments := []node{}
		head := arglist.(*ConsCell)
		for cell := head ; cell != nil ; cell = cell.next() {
			result, err := e.Eval(cell.car)
			if err != nil {
				return nil, err
			}
			arguments = append(arguments, result)

			if cell.next() == head {
				return nil, fmt.Errorf("Argument is circular list.")
			}
		}
		return createList(arguments), nil
	default:
		return nil, fmt.Errorf("Argument is not list")
	}
}

func evalFunc(e *evaluator, fn *FuncNode, arguments []node) (node, error) {
	// 実引数を関数のscopeのsymbolTableに登録
	symTable := e.topEnvironment().symbols
	pi := 0	// parameters index
	ai := 0	// arguments index
	foundKeywordParam := false
	for pi = range fn.parameters {
		if fn.parameters[pi].required {
			if ai >= len(arguments)	{
				return nil, fmt.Errorf("Insufficient arguments")
			}
			symTable.set(fn.parameters[pi].symbol, arguments[ai])
			ai++
		} else if fn.parameters[pi].optional {
			if ai <= len(arguments) - 1	{
				symTable.set(fn.parameters[pi].symbol, arguments[ai])
				ai++
			} else {
				symTable.set(fn.parameters[pi].symbol, fn.parameters[pi].defValue)
			}
		} else if fn.parameters[pi].rest {
			rest := []node{}
			for ; ai < len(arguments) ; ai++ {
				rest = append(rest, arguments[ai])
			}
			symTable.set(fn.parameters[pi].symbol, createList(rest))
		} else if fn.parameters[pi].key {
			symTable.set(fn.parameters[pi].symbol, fn.parameters[pi].defValue)
			foundKeywordParam = true
		}
	}
	if foundKeywordParam {
		// キーワード引数の読み込み
		aiStart := 0	// requiredとoptional引数の数
		for pi := range fn.parameters {
			if !fn.parameters[pi].required && !fn.parameters[pi].optional {
				break
			}
			aiStart++
		}
		for ai = aiStart ; ai < len(arguments) ; ai += 2 {
			keyword, ok := arguments[ai].(*KeywordNode)
			if !ok {
				return nil, fmt.Errorf("Must be keyword name here.")
			}
			if ai + 1 >= len(arguments) {
				return nil, fmt.Errorf("keyword's value is not found.")
			}
			found := false
			for pi := range fn.parameters {
				if fn.parameters[pi].key && fn.parameters[pi].keyword.name == keyword.name {
					symTable.set(fn.parameters[pi].symbol, arguments[ai + 1])
					found = true
				}
			}
			if !found {
				return nil, fmt.Errorf("keyword %v is not found in argument list.", keyword.name)
			}
		}
	}
	if ai < len(arguments) {
		return nil, fmt.Errorf("Too many arguments given.")
	}

	var lastResult node
	lastResult = &NilNode{}
	// bodyのlistを順番に評価していく
	for current := getConsCell(fn.body) ; current != nil ; current = current.next() {
		var err error
		lastResult, err = e.Eval(current.car)
		if err != nil {
			return nil, err
		}
	}
	return lastResult, nil
}

func expandMacro(e *evaluator, mc *MacroNode, arguments []node) (node, error) {
	// 実引数を関数のscopeのsymbolTableに登録
	symTable := e.topEnvironment().symbols
	pi := 0	// parameters index
	ai := 0	// arguments index
	foundKeywordParam := false
	for pi = range mc.parameters {
		if mc.parameters[pi].required {
			if ai >= len(arguments)	{
				return nil, fmt.Errorf("Insufficient arguments")
			}
			symTable.set(mc.parameters[pi].symbol, arguments[ai])
			ai++
		} else if mc.parameters[pi].optional {
			if ai <= len(arguments) - 1	{
				symTable.set(mc.parameters[pi].symbol, arguments[ai])
				ai++
			} else {
				symTable.set(mc.parameters[pi].symbol, mc.parameters[pi].defValue)
			}
		} else if mc.parameters[pi].rest {
			rest := []node{}
			for ; ai < len(arguments) ; ai++ {
				rest = append(rest, arguments[ai])
			}
			symTable.set(mc.parameters[pi].symbol, createList(rest))
		} else if mc.parameters[pi].key {
			symTable.set(mc.parameters[pi].symbol, mc.parameters[pi].defValue)
			foundKeywordParam = true
		}
	}
	if foundKeywordParam {
		// キーワード引数の読み込み
		aiStart := 0	// requiredとoptional引数の数
		for pi := range mc.parameters {
			if !mc.parameters[pi].required && !mc.parameters[pi].optional {
				break
			}
			aiStart++
		}
		for ai = aiStart ; ai < len(arguments) ; ai += 2 {
			keyword, ok := arguments[ai].(*KeywordNode)
			if !ok {
				return nil, fmt.Errorf("Must be keyword name here.")
			}
			if ai + 1 >= len(arguments) {
				return nil, fmt.Errorf("keyword's value is not found.")
			}
			found := false
			for pi := range mc.parameters {
				if mc.parameters[pi].key && mc.parameters[pi].keyword.name == keyword.name {
					symTable.set(mc.parameters[pi].symbol, arguments[ai + 1])
					found = true
				}
			}
			if !found {
				return nil, fmt.Errorf("keyword %v is not found in argument list.", keyword.name)
			}
		}
	}
	if ai < len(arguments) {
		return nil, fmt.Errorf("Too many arguments given.")
	}

	//fmt.Printf("%v\n", mc.body.ToString())

	// bodyを評価することでマクロを展開
	// 展開によって式を破壊するのでコピーしてから評価する
	// 引数は評価されずに渡されているので、仮引数のシンボルは引数に置換される
	copiedBody, err := copyProperList(mc.body)
	if err != nil {
		return nil, err
	}
	var lastResult node
	lastResult = &NilNode{}
	for current := getConsCell(copiedBody) ; current != nil ; current = current.next() {
		var err error
		lastResult, err = e.Eval(current.car)
		if err != nil {
			return nil, err
		}
	}

	//fmt.Printf("%v\n", lastResult.ToString())

	// 展開したリストを返す
	return lastResult, nil
}

func (e *evaluator) FreshLine() {
	if e.writed {
		fmt.Println("")
		e.writed = false
	}
}
