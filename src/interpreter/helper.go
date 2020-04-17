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

func createSliceFromList(head *ConsCell) []node {
	result := []node{}

	c := head
	for c != nil {
		result = append(result, c.car)

		// 正しく作成されていればcdrはnilにはならない(終端は&NilNode)。
		if c.cdr == nil {
			break
		}

		switch cdr := c.cdr.(type) {
		case *ConsCell:
			c = cdr		// next cons cell
		case *NilNode:
			c = nil		// terminate
		default:
			// dot list
			result = append(result, cdr)
			c = nil
		}
	}

	return result
}
