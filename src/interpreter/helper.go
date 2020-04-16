package interpreter

func createList(elements []node) node {
	if len(elements) == 0 {
		return &NilNode{}
	}

	var p node
	var prev node
	prev = &NilNode{}
	for i := len(elements) - 1 ; i >= 0 ; i-- {
		el := elements[i]
		p = &ConsCell{
			car: el,
			cdr: prev,
		}
		prev = p
	}

	return p
}

// 最後のエントリをcdrに入れる
func createDotList(elements []node) node {
	if len(elements) < 2 {
		return nil
	}

	var p node
	var prev node
	prev = elements[len(elements) - 1]
	for i := len(elements) - 2 ; i >= 0 ; i-- {
		el := elements[i]
		p = &ConsCell{
			car: el,
			cdr: prev,
		}
		prev = p
	}

	return p
}
