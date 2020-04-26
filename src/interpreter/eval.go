package interpreter

import (
	"fmt"
)

type LispFunc func(ev *evaluator, c *ConsCell)(node, error)

var embeddedFunctions map[string]LispFunc
var functionTable map[string]node = make(map[string]node)		// *FuncNode or *MacroNode

func init() {
	embeddedFunctions = map[string]LispFunc{
		"+": funcAdd,
		"-": funcSubtract,
		"*": funcMultiply,
		"/": funcDivide,
		"=": funcEqual,
		"/=": funcNotEqual,
		">": funcGreaterThan,
		">=": funcGreaterThanOrEqualTo,
		"<": funcLessThan,
		"<=": funcLessThanOrEqualTo,
		"append": funcAppend,
		"cons": funcCons,
		"car": funcCar,
		"cdr": funcCdr,
		"and": funcAnd,
		"or": funcOr,
		"not": funcNot,
		"setq": funcSetq,
		"defun": funcDefun,
		"defmacro": funcDefmacro,
		"function": funcFunction,
		"let": funcLet,
		"progn": funcProgn,
		"print": funcPrint,
		"quote": funcQuote,
		"list": funcList,
		"if": funcIf,
		"do": funcDo,

		"system::backquote": funcSystemBackQuote,
	}
}

type evaluator struct {
	scopeStack	[]*lexicalScope
}

func NewEvaluator() *evaluator {
	return &evaluator{
		scopeStack: []*lexicalScope{newLexicalScope(nil)},
	}
}

func (e *evaluator) topScope() *lexicalScope {
	return e.scopeStack[len(e.scopeStack) - 1]
}

func (e *evaluator) Eval(n node) (node, error) {
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
				return f(e, cell.next())
			} else {
				value, ok := functionTable[funcName]
				if !ok {
					return nil, fmt.Errorf("%v function not found.", funcName)
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
	
					// evaluatorに関数のlexicalScopeを積んでscopeを切り替え。
					// 関数のlexicalScopeのsymbolStackに新しいテーブルを追加。
					e.scopeStack = append(e.scopeStack, fn.scope)
					fn.scope.symbolTableStack = append(fn.scope.symbolTableStack, symbolTable{})
	
					result, err := evalFunc(e, fn, arguments)
	
					fn.scope.symbolTableStack = fn.scope.symbolTableStack[0:len(fn.scope.symbolTableStack) - 1]
					e.scopeStack = e.scopeStack[0:len(e.scopeStack) - 1]
	
					return result, err

				case *MacroNode:
					arguments := []node{}
					acell := cell.next()
					for ; acell != nil ; acell = acell.next() {
						// マクロでは引数の評価はしない
						arguments = append(arguments, acell.car)
					}

					// 展開＆評価
					e.scopeStack = append(e.scopeStack, fn.scope)
					fn.scope.symbolTableStack = append(fn.scope.symbolTableStack, symbolTable{})

					expanded, err := expandMacro(e, fn, arguments)

					fn.scope.symbolTableStack = fn.scope.symbolTableStack[0:len(fn.scope.symbolTableStack) - 1]
					e.scopeStack = e.scopeStack[0:len(e.scopeStack) - 1]

					if err != nil {
						return nil, err
					}

					return e.Eval(expanded)

				default:
					return nil, fmt.Errorf("%v is not function.", symbol.name)
				}
				// not to reach
			}
		} else {
			return nil, fmt.Errorf("invalid function.")
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
		value, ok := e.topScope().lookupSymbol(symbol.name)
		if !ok {
			return nil, fmt.Errorf("%v not found.", symbol.name)
		}
		return value, nil
	}

	return n, nil
}

func evalFunc(e *evaluator, fn *FuncNode, arguments []node) (node, error) {
	// 実引数を関数のscopeのsymbolTableに登録
	symTable := e.topScope().topSymbolTable()
	ai := 0	// arguments index
	for pi := range fn.parameters {
		if fn.parameters[pi].required {
			if ai >= len(arguments)	{
				return nil, fmt.Errorf("Insufficient arguments")
			}
			symTable[fn.parameters[pi].name] = arguments[ai]
			ai++
		} else if fn.parameters[pi].optional {
			if ai <= len(arguments) - 1	{
				symTable[fn.parameters[pi].name] = arguments[ai]
				ai++
			} else {
				symTable[fn.parameters[pi].name] = fn.parameters[pi].defValue
			}
		} else if fn.parameters[pi].rest {
			rest := []node{}
			for ; ai < len(arguments) ; ai++ {
				rest = append(rest, arguments[ai])
			}
			symTable[fn.parameters[pi].name] = createList(rest)
		}
	}
	if ai < len(arguments) {
		return nil, fmt.Errorf("Too many arguments given.")
	}

	var lastResult node
	lastResult = &NilNode{}
	// bodyのlistを順番に評価していく
	for current := fn.body ; current != nil ; current = current.next() {
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
	symTable := e.topScope().topSymbolTable()
	ai := 0	// arguments index
	for pi := range mc.parameters {
		if mc.parameters[pi].required {
			if ai >= len(arguments)	{
				return nil, fmt.Errorf("Insufficient arguments")
			}
			symTable[mc.parameters[pi].name] = arguments[ai]
			ai++
		} else if mc.parameters[pi].optional {
			if ai <= len(arguments) - 1	{
				symTable[mc.parameters[pi].name] = arguments[ai]
				ai++
			} else {
				symTable[mc.parameters[pi].name] = mc.parameters[pi].defValue
			}
		} else if mc.parameters[pi].rest {
			rest := []node{}
			for ; ai < len(arguments) ; ai++ {
				rest = append(rest, arguments[ai])
			}
			symTable[mc.parameters[pi].name] = createList(rest)
		}
	}
	if ai < len(arguments) {
		return nil, fmt.Errorf("Too many arguments given.")
	}

	//fmt.Printf("%v\n", mc.body.ToString())

	// bodyを評価することでマクロを展開
	// 引数は評価されずに渡されているので、仮引数のシンボルは引数に置換される
	var lastResult node
	lastResult = &NilNode{}
	for current := mc.body ; current != nil ; current = current.next() {
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
