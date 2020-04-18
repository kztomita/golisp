package interpreter

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
