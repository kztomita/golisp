package interpreter

func Initialize() error {
	node, err := Parse(systemFuncDefinitions)
	if err != nil {
		return err
	}

	ev := NewEvaluator()
	_, err = ev.Eval(node)
	if err != nil {
		return err
	}

	return nil
}
