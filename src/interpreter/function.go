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
		"and": funcAnd,
		"append": funcAppend,
		"apply": funcApply,
		"car": funcCar,
		"cdr": funcCdr,
		"cons": funcCons,
		"defmacro": funcDefmacro,
		"defun": funcDefun,
		"do": funcDo,
		"eq": funcEq,
		"function": funcFunction,
		"gensym": funcGensym,
		"if": funcIf,
		"lambda": funcLambda,
		"length": funcLength,
		"let": funcLet,
		"list": funcList,
		"not": funcNot,
		"or": funcOr,
		"prin1": funcPrin1,
		"print": funcPrint,
		"quote": funcQuote,
		"setq": funcSetq,

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
