package interpreter

type lispFunc func(ev *evaluator, arglist node)(node, error)

type lispFuncType	int
const (
	lispFuncTypeFunction lispFuncType = iota
	lispFuncTypeMacro
	lispFuncTypeSpecialOperator		// 現状lispFuncTypeMacroと違いはない
)

type lispFuncEntry struct {
	fn		lispFunc
	fntype	lispFuncType	// 関数ならfn呼び出し前に引数を評価する
}
var embeddedFunctions map[string]lispFuncEntry
var functionTable map[string]node = make(map[string]node)		// *FuncNode or *MacroNode

func init() {
	embeddedFunctions = map[string]lispFuncEntry{
		"+": lispFuncEntry{fn: funcAdd, fntype: lispFuncTypeFunction},
		"-": lispFuncEntry{fn: funcSubtract, fntype: lispFuncTypeFunction},
		"*": lispFuncEntry{fn: funcMultiply, fntype: lispFuncTypeFunction},
		"/": lispFuncEntry{fn: funcDivide, fntype: lispFuncTypeFunction},
		"=": lispFuncEntry{fn: funcEqual, fntype: lispFuncTypeFunction},
		"/=": lispFuncEntry{fn: funcNotEqual, fntype: lispFuncTypeFunction},
		">": lispFuncEntry{fn: funcGreaterThan, fntype: lispFuncTypeFunction},
		">=": lispFuncEntry{fn: funcGreaterThanOrEqualTo, fntype: lispFuncTypeFunction},
		"<": lispFuncEntry{fn: funcLessThan, fntype: lispFuncTypeFunction},
		"<=": lispFuncEntry{fn: funcLessThanOrEqualTo, fntype: lispFuncTypeFunction},
		"and": lispFuncEntry{fn: funcAnd, fntype: lispFuncTypeMacro},
		"append": lispFuncEntry{fn: funcAppend, fntype: lispFuncTypeFunction},
		"apply": lispFuncEntry{fn: funcApply, fntype: lispFuncTypeFunction},
		"car": lispFuncEntry{fn: funcCar, fntype: lispFuncTypeFunction},
		"cdr": lispFuncEntry{fn: funcCdr, fntype: lispFuncTypeFunction},
		"cons": lispFuncEntry{fn: funcCons, fntype: lispFuncTypeFunction},
		"defmacro": lispFuncEntry{fn: funcDefmacro, fntype: lispFuncTypeMacro},
		"defun": lispFuncEntry{fn: funcDefun, fntype: lispFuncTypeMacro},
		"do": lispFuncEntry{fn: funcDo, fntype: lispFuncTypeMacro},
		"eq": lispFuncEntry{fn: funcEq, fntype: lispFuncTypeFunction},
		"function": lispFuncEntry{fn: funcFunction, fntype: lispFuncTypeSpecialOperator},
		"gensym": lispFuncEntry{fn: funcGensym, fntype: lispFuncTypeFunction},
		"if": lispFuncEntry{fn: funcIf, fntype: lispFuncTypeSpecialOperator},
		"lambda": lispFuncEntry{fn: funcLambda, fntype: lispFuncTypeMacro},
		"length": lispFuncEntry{fn: funcLength, fntype: lispFuncTypeFunction},
		"let": lispFuncEntry{fn: funcLet, fntype: lispFuncTypeSpecialOperator},
		"list": lispFuncEntry{fn: funcList, fntype: lispFuncTypeFunction},
		"mapcar": lispFuncEntry{fn: funcMapcar, fntype: lispFuncTypeFunction},
		"multiple-value-bind": lispFuncEntry{fn: funcMultipleValueBind, fntype: lispFuncTypeMacro},
		"not": lispFuncEntry{fn: funcNot, fntype: lispFuncTypeFunction},
		"or": lispFuncEntry{fn: funcOr, fntype: lispFuncTypeMacro},
		"prin1": lispFuncEntry{fn: funcPrin1, fntype: lispFuncTypeFunction},
		"print": lispFuncEntry{fn: funcPrint, fntype: lispFuncTypeFunction},
		"rplaca": lispFuncEntry{fn: funcRplaca, fntype: lispFuncTypeFunction},
		"rplacd": lispFuncEntry{fn: funcRplacd, fntype: lispFuncTypeFunction},
		"quote": lispFuncEntry{fn: funcQuote, fntype: lispFuncTypeSpecialOperator},
		"setq": lispFuncEntry{fn: funcSetq, fntype: lispFuncTypeSpecialOperator},
		"type-of": lispFuncEntry{fn: funcTypeOf, fntype: lispFuncTypeFunction},
		"typep": lispFuncEntry{fn: funcTypep, fntype: lispFuncTypeFunction},
		"values": lispFuncEntry{fn: funcValues, fntype: lispFuncTypeFunction},

		"system::backquote": lispFuncEntry{fn: funcSystemBackQuote, fntype: lispFuncTypeMacro},
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
