package interpreter

type LispFunc func(ev *evaluator, arglist node)(node, error)

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
		"apply": funcApply,
		"lambda": funcLambda,
		"let": funcLet,
		"progn": funcProgn,
		"prin1": funcPrin1,
		"print": funcPrint,
		"quote": funcQuote,
		"length": funcLength,
		"list": funcList,
		"if": funcIf,
		"do": funcDo,

		"system::backquote": funcSystemBackQuote,
	}
}

// nameで関数を探し*SystemFuncNodeか*FuncNodeを返す。
// 存在しない場合はnilを返す。
func lookupFunction(name string) node {
	if _, ok := embeddedFunctions[name]; ok {
		return &SystemFuncNode{name: name}
	}
	if nd, ok := functionTable[name]; ok {
		if fn, ok := nd.(*FuncNode); ok {		// *MacroNodeは返さないように
			return fn
		}
	}
	return nil
}
