package interpreter

import (
	"fmt"
)
var embeddedFunctions map[string]func(ev *evaluator, c *ConsCell)(node, error)

func init() {
	embeddedFunctions = map[string]func(ev *evaluator, c *ConsCell)(node, error){
		"+": funcAdd,
		"-": funcSubtract,
		"*": funcMultiply,
		"/": funcDivide,
		"=": funcEqual,
		"/=": funcNotEqual,
		"car": funcCar,
		"cdr": funcCdr,
		"and": funcAnd,
		"or": funcOr,
		"setq": funcSetq,
		"defun": funcDefun,
		"defmacro": funcDefmacro,
		"let": funcLet,
		"progn": funcProgn,
		"print": funcPrint,
		"quote": funcQuote,
		"list": funcList,
		"if": funcIf,
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
				value, ok := e.topScope().lookupSymbol(funcName)
				if !ok {
					return nil, fmt.Errorf("%v not found.", symbol.name)
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
		// symbol tableをlookup
		symbol := n.(*SymbolNode)
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
	if len(fn.parameters) != len(arguments) {
		return nil, fmt.Errorf("Number of arguments is mismatch.")
	}
	symTable := e.topScope().topSymbolTable()
	for i := range fn.parameters {
		symTable[fn.parameters[i].name] = arguments[i]
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
	if len(mc.parameters) != len(arguments) {
		return nil, fmt.Errorf("Number of arguments is mismatch.")
	}
	// 未評価の引数をシンボルテーブルに登録
	symTable := e.topScope().topSymbolTable()
	for i := range mc.parameters {
		symTable[mc.parameters[i].name] = arguments[i]
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
