package interpreter

func createList(elements []node) node {
	if len(elements) == 0 {
		return &nilNode{}
	}

	var p node
	var prev node
	prev = &nilNode{}
	for i := len(elements) - 1 ; i >= 0 ; i-- {
		el := elements[i]
		p = &consCell{
			car: el,
			cdr: prev,
		}
		prev = p
	}

	return p
}