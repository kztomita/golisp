package interpreter

import (
	"fmt"
)

// (car list)
// Ex. (car '(1 2 3))
func funcCar(c *ConsCell) (node, error) {
	if c == nil {
		return nil, fmt.Errorf("Wrong number of arguments.")
	}
	if !c.isList() {
		return nil, fmt.Errorf("Wrong type argument.")
	}
	if c.length() != 1 {
		return nil, fmt.Errorf("Wrong number of arguments.")
	}

	arg0, ok := c.car.(*ConsCell)
	if !ok {
		return nil, fmt.Errorf("Wrong type argument.")
	}
	return arg0.car, nil
}

// (cdr list)
// Ex. (cdr '(1 2 3))
func funcCdr(c *ConsCell) (node, error) {
	if c == nil {
		return nil, fmt.Errorf("Wrong number of arguments.")
	}
	if !c.isList() {
		return nil, fmt.Errorf("Wrong type argument.")
	}
	if c.length() != 1 {
		return nil, fmt.Errorf("Wrong number of arguments.")
	}

	arg0, ok := c.car.(*ConsCell)
	if !ok {
		return nil, fmt.Errorf("Wrong type argument.")
	}
	return arg0.cdr, nil
}

func funcAnd(ev *evaluator, c *ConsCell) (node, error) {
	if c != nil && !c.isList() {
		return nil, fmt.Errorf("Wrong type argument.")
	}

	args := createSliceFromList(c)
	for _, arg := range args {
		result, err := ev.Eval(arg)
		if err != nil {
			return nil, err
		}
		_, ok := result.(*NilNode)
		if ok {
			return &NilNode{}, nil
		}
	}
	return &TrueNode{}, nil
}

func funcOr(ev *evaluator, c *ConsCell) (node, error) {
	if c != nil && !c.isList() {
		return nil, fmt.Errorf("Wrong type argument.")
	}

	args := createSliceFromList(c)
	for _, arg := range args {
		result, err := ev.Eval(arg)
		if err != nil {
			return nil, err
		}
		_, ok := result.(*NilNode)
		if !ok {
			return &TrueNode{}, nil
		}
	}
	return &NilNode{}, nil
}

func funcSetq(ev *evaluator, c *ConsCell) (node, error) {
	if c == nil {
		return nil, fmt.Errorf("Wrong number of arguments.")
	}
	if !c.isList() {
		return nil, fmt.Errorf("Wrong type argument.")
	}

	args := createSliceFromList(c)
	if len(args) != 2 {
		return nil, fmt.Errorf("Wrong number of arguments.")
	}

	arg0, ok := args[0].(*SymbolNode)	// symbolname
	if !ok {
		return nil, fmt.Errorf("Wrong type argument.")
	}

	result, err := ev.Eval(args[1])
	if err != nil {
		return nil, err
	}

	ev.scopeStack[0].symbolTableStack[0][arg0.name] = result

	return &NilNode{}, nil
}

func funcDefun(ev *evaluator, c *ConsCell) (node, error) {
	if c == nil {
		return nil, fmt.Errorf("Wrong number of arguments.")
	}
	if !c.isList() {
		return nil, fmt.Errorf("Wrong type argument.")
	}

	args := createSliceFromList(c)
	if len(args) < 3 {
		return nil, fmt.Errorf("Wrong number of arguments.")
	}

	arg0, ok := args[0].(*SymbolNode)	// func name
	if !ok {
		return nil, fmt.Errorf("Wrong type argument.")
	}

	parameters := []*SymbolNode{}	// 仮引数名のsymbolNode
	arg1, ok1 := args[1].(*ConsCell)	// parameters
	if ok1 {
		acell := arg1
		for acell != nil {
			sym, ok := acell.car.(*SymbolNode)
			if !ok {
				return nil, fmt.Errorf("Wrong type argument.")
			}
			parameters = append(parameters, sym)
			acell = acell.next()
		}
	}

	bodyHead := c.next().next()		// body list

	fn := &FuncNode{
		parameters: parameters,
		body: bodyHead,
		scope: newLexicalScope(ev.topScope()),	// 関数定義時にLexicalScope作成
	}

	symTable := ev.topScope().topSymbolTable()
	symTable[arg0.name] = fn

	return &NilNode{}, nil
}

func funcLet(ev *evaluator, c *ConsCell) (node, error) {
	// lexicalScopeを作成してscopeを切り替え。
	// 作成したlexicalScopeのsymbolStackに新しいテーブルを追加。
	scope := newLexicalScope(ev.topScope())
	ev.scopeStack = append(ev.scopeStack, scope)
	scope.symbolTableStack = append(scope.symbolTableStack, symbolTable{})

	result, err := funcLet_(ev, c)

	ev.scopeStack = ev.scopeStack[0:len(ev.scopeStack) - 1]

	return result, err
}

func funcLet_(ev *evaluator, c *ConsCell) (node, error) {
	if c == nil {
		return nil, fmt.Errorf("Wrong number of arguments.")
	}
	if !c.isList() {
		return nil, fmt.Errorf("Wrong type argument.")
	}
	if c.length() < 2 {
		return nil, fmt.Errorf("Wrong number of arguments.")
	}

	p := c
	arg0, ok := p.car.(*ConsCell)	// binding-list
	if !ok {
		return nil, fmt.Errorf("Wrong type argument(binding-list).")
	}
	if !arg0.isList() {
		return nil, fmt.Errorf("Wrong type argument(binding-list).")
	}

	symTable := ev.topScope().topSymbolTable()

	pBinding := arg0
	for pBinding != nil {
		binding, ok := pBinding.car.(*ConsCell)
		if !ok {
			return nil, fmt.Errorf("Wrong type argument(binding-list).")
		}
		if !binding.isList() || binding.length() != 2 {
			return nil, fmt.Errorf("Wrong type argument(binding-list).")
		}

		// first element
		bindingSymbol, ok := binding.car.(*SymbolNode)
		if !ok {
			return nil, fmt.Errorf("Wrong type argument(binding-list).")
		}

		// second element
		value := binding.next()
		if value == nil {
			return nil, fmt.Errorf("Wrong type argument(binding-list).")
		}
		bindingValue, err := ev.Eval(value.car)
		if err != nil {
			return nil, err
		}

		// 変数をシンボルテーブルに登録
		//fmt.Printf("%v %v", bindingSymbol.name, bindingValue.ToString())
		symTable[bindingSymbol.name] = bindingValue

		pBinding = pBinding.next()
	}

	// bodyの実行
	p = p.next()	// body list

	var lastResult node
	lastResult = &NilNode{}
	for ; p != nil ; p = p.next() {
		var err error
		lastResult, err = ev.Eval(p.car)
		if err != nil {
			return nil, err
		}
	}

	return lastResult, nil
}

func funcProgn(ev *evaluator, c *ConsCell) (node, error) {
	var lastResult node
	lastResult = &NilNode{}
	for p := c ; p != nil ; p = p.next() {
		var err error
		lastResult, err = ev.Eval(p.car)
		if err != nil {
			return nil, err
		}
	}

	return lastResult, nil
}

func funcPrint(ev *evaluator, c *ConsCell) (node, error) {
	if c == nil {
		return nil, fmt.Errorf("Wrong number of arguments.")
	}
	if !c.isList() {
		return nil, fmt.Errorf("Wrong type argument.")
	}
	// streamは未対応
	if c.length() != 1 {
		return nil, fmt.Errorf("Wrong number of arguments.")
	}

	value, err := ev.Eval(c.car)
	if err != nil {
		return nil, err
	}

	fmt.Println(value.ToString())

	return value, nil
}

func funcIf(ev *evaluator, c *ConsCell) (node, error) {
	if c == nil {
		return nil, fmt.Errorf("Wrong number of arguments.")
	}
	if !c.isList() {
		return nil, fmt.Errorf("Wrong type argument.")
	}
	if c.length() != 3 {
		return nil, fmt.Errorf("Wrong number of arguments.")
	}

	args := createSliceFromList(c)

	condition, err := ev.Eval(args[0])
	if err != nil {
		return nil, err
	}

	var nd node
	if condition.GetNodeType() == NtNil {
		nd = args[2]  // else
	} else {
		nd = args[1]  // then
	}

	result, err := ev.Eval(nd)
	if err != nil {
		return nil, err
	}

	return result, nil
}

func funcPlus(ev *evaluator, c *ConsCell) (*IntNode, error) {
	result := 0		// XXX 取り敢えずintのみ対応

	if c != nil && !c.isList() {
		return nil, fmt.Errorf("Wrong type argument.")
	}

	args := createSliceFromList(c)

	for _, arg := range args {
		element, err := ev.Eval(arg)
		if err != nil {
			return nil, err
		}
		intResult, ok := element.(*IntNode)
		if ok {
			result += intResult.value
		} else {
			return nil, fmt.Errorf("not integer element")
		}
	}

	return &IntNode{value: result}, nil
}
